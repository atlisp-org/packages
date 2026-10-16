"""逐布局写图 + 独立校验：把各布局跑一遍、落盘、并从文件本身验几何。

设计要点（都是踩过的坑，改动前先读）：
1. **判定一律来自磁盘上的文件**，不在 CAD 内存里数实体。AutoCAD 会缓存已打开的
   文档，"当前文档"未必是你刚跑的那个；本机 dwg2dxf + ezdxf 读文件才可信。
2. **不用 COM 的 vla-Open 切文档**。实测它会把 entmake 建出的实体落到另一个文档，
   vla-SaveAs 存回来的是空图，而且返回值报"成功"。
3. **不覆盖已存在的 dwg**。目标文件若仍被 CAD 打开，保存会报"保存文档时出错"。
   做法是先存到全新临时名，校验通过后再在本机改名。
4. **布局 6 默认跳过**。实测只喂 5 个点就跑不完，会把 CAD 阻塞十几分钟。
5. open_dwg 会重置 LISP 命名空间，每次跑完布局都要重新加载包源码。

用法:
  python3 verify_layouts.py                      # 跑全部（跳过 6）
  python3 verify_layouts.py --layouts 0,8        # 只跑指定布局
  python3 verify_layouts.py --verify-only        # 只校验已有 dim-leader-L*.dwg，不碰 CAD
  python3 verify_layouts.py --host http://192.168.1.55:8110
  python3 verify_layouts.py --include 6          # 明知会卡 CAD 也要跑
"""
import argparse
import collections
import os
import re
import subprocess
import sys
import time

import ezdxf

import mcp_client as mc

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_SRC = os.path.join(HERE, "高程点图_局部.dwg")
LAYOUT_NAMES = {
    0: "0 本地错开", 1: "1 右侧表格", 2: "2 分区表格", 3: "3 二维分区",
    4: "4 四向", 5: "5 分向轨道", 6: "6 芯片式", 7: "7 紧凑双侧", 8: "8 通道布线",
}
# 布局 6 实测 5 个点即跑不完并阻塞 CAD，见 README「已知限制」
DEFAULT_SKIP = (6,)


# ---------------------------------------------------------------- 几何校验

def _orient(px, py, qx, qy, rx, ry, eps=1e-9):
    v = (qx - px) * (ry - py) - (qy - py) * (rx - px)
    return 0 if abs(v) < eps else (1 if v > 0 else -1)


def _cross(a1, a2, b1, b2, eps=1e-9):
    """两条轴对齐线段是否真交叉。**只有一横一竖才算**。

    同向（都水平或都垂直）一律不算：要么平行（永不相交），要么共线
    （交点无穷多，或仅端点相接），都谈不上"交叉"。

    必须避开的坑：只判"同向共线"、把其余情况默认 return True，
    会把两条 y 不同的水平引线段误计为交叉 —— 历史数据就是这么虚高 2~5 倍的。
    _selftest() 锁住这条语义。
    """
    if _orient(*a1, *a2, *b1, eps) * _orient(*a1, *a2, *b2, eps) > 0:
        return False
    if _orient(*b1, *b2, *a1, eps) * _orient(*b1, *b2, *a2, eps) > 0:
        return False
    h_a, h_b = abs(a1[1] - a2[1]) < eps, abs(b1[1] - b2[1]) < eps
    return h_a != h_b                  # 一横一竖 -> 唯一真交叉情形


def _selftest():
    """锁住 _cross 的语义，改动后先跑这个。"""
    cases = [
        ("X 形相交",        ((0, 5), (10, 5)), ((5, 0), (5, 10)), True),
        ("T 形相接",        ((0, 0), (10, 0)), ((5, 0), (5, 5)), True),
        ("异高水平(平行)",  ((0, 0), (10, 0)), ((0, 5), (10, 5)), False),
        ("异 x 垂直(平行)", ((0, 0), (0, 10)), ((5, 0), (5, 10)), False),
        ("共线重叠",        ((0, 0), (10, 0)), ((5, 0), (15, 0)), False),
        ("共线端点相接",    ((0, 0), (10, 0)), ((10, 0), (20, 0)), False),
        ("共线端点相接(垂)", ((0, 0), (0, 10)), ((0, 10), (0, 20)), False),
        ("完全分离",        ((0, 0), (1, 0)), ((5, 5), (6, 5)), False),
    ]
    for name, a, b, want in cases:
        got = _cross(a[0], a[1], b[0], b[1])
        assert got == want, "%s: 期望 %s 实得 %s" % (name, want, got)
    return True




def _count_crossings(segs, cell=20.0):
    """线段两两交叉数。按 20 单位分桶，只与同桶内的比。"""
    grid = collections.defaultdict(list)
    for i, (a, b) in segs:
        for gx in range(int(min(a[0], b[0]) // cell), int(max(a[0], b[0]) // cell) + 1):
            for gy in range(int(min(a[1], b[1]) // cell), int(max(a[1], b[1]) // cell) + 1):
                grid[(gx, gy)].append((i, (a, b)))
    seen, n = set(), 0
    for items in grid.values():
        for x in range(len(items)):
            for y in range(x + 1, len(items)):
                i, (a, b) = items[x]
                j, (c, d) = items[y]
                if i == j:
                    continue
                k = (i, j) if i < j else (j, i)
                if k in seen:
                    continue
                seen.add(k)
                if _cross(a, b, c, d):
                    n += 1
    return n


def check_dwg(path, arc_expect=None, text_expect=None):
    """把 dwg 转 dxf 读几何做校验。返回 (统计字典, 问题列表)。"""
    dxf = os.path.join("/tmp", os.path.basename(path) + ".dxf")
    try:
        subprocess.run(["dwg2dxf", "-o", dxf, path], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=300)
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired,
            FileNotFoundError) as e:
        return {}, ["dwg2dxf 转档失败: %s" % e]
    try:
        msp = ezdxf.readfile(dxf).modelspace()
    except Exception as e:                                   # noqa: BLE001
        return {}, ["ezdxf 读取失败: %s" % e]
    finally:
        if os.path.exists(dxf):
            os.remove(dxf)

    cnt = collections.Counter(e.dxftype() for e in msp)
    routes, segs = [], []
    non_orth = degen = 0
    for e in msp:
        if e.dxftype() != "LWPOLYLINE":
            continue
        pts = [(round(v[0], 4), round(v[1], 4)) for v in e.get_points("xy")]
        if len(pts) < 2:
            degen += 1
            continue
        for k in range(len(pts) - 1):
            (x1, y1), (x2, y2) = pts[k], pts[k + 1]
            if abs(x1 - x2) > 1e-6 and abs(y1 - y2) > 1e-6:
                non_orth += 1
            segs.append((len(routes), (pts[k], pts[k + 1])))
        routes.append(pts)

    total = sum(
        ((pts[i + 1][0] - pts[i][0]) ** 2 + (pts[i + 1][1] - pts[i][1]) ** 2) ** 0.5
        for pts in routes for i in range(len(pts) - 1)
    )
    starts = collections.Counter(p[0] for p in routes)
    stat = dict(text=cnt["TEXT"], pline=cnt["LWPOLYLINE"], arc=cnt["ARC"],
                non_orth=non_orth, degen=degen,
                cross=_count_crossings(segs) if segs else 0,
                avg_len=round(total / len(routes), 1) if routes else 0.0,
                dup_start=sum(1 for v in starts.values() if v > 1))

    bad = []
    if not routes:
        bad.append("没有任何引线（多半是存图落到了别的文档，见文件头第 2 条）")
    if degen:
        bad.append("%d 条引线顶点不足 2 个" % degen)
    if non_orth:
        bad.append("%d 段非正交" % non_orth)
    if stat["dup_start"]:
        bad.append("%d 个起点被多条引线共用" % stat["dup_start"])
    if arc_expect is not None and cnt["ARC"] != arc_expect:
        bad.append("ARC 数 %d != 底图 %d" % (cnt["ARC"], arc_expect))
    if text_expect is not None and cnt["TEXT"] != text_expect:
        bad.append("TEXT 数 %d != 底图 %d" % (cnt["TEXT"], text_expect))
    return stat, bad


# ---------------------------------------------------------------- 写图

RUN_LISP = """(progn
  (setq *t0* (getvar "MILLISECS"))
  (dim-leader:run-layout %d)
  (setq *t1* (getvar "MILLISECS"))
  (list (- *t1* *t0*) (sslength (ssget "_X" '((0 . "LWPOLYLINE"))))))"""

SAVE_LISP = """(progn
  (vla-SaveAs (vla-get-ActiveDocument (vlax-get-acad-object)) "%s")
  (vl-file-size "%s"))"""

# 上一次运行若在保存后异常退出，会留下"文件已删但文档仍开着"的幽灵文档：
# 该名字被占住后 vl-file-copy 建不出同名文件，open_dwg 也报"文件不存在"。
# 每轮先关掉所有 vltmp- 前缀的文档。留一个文档垫底，避免关成"无文档"状态。
CLOSE_LISP = """(defun vl-close-vltmp (/ acad docs i n lst doc)
  (setq acad (vlax-get-acad-object) docs (vla-get-Documents acad)
        i 0 lst nil)
  (while (< i (vla-get-Count docs))
    (setq n (vla-get-Name (vla-Item docs i)))
    (if (and (vl-string-search "vltmp-" n) (> (vla-get-Count docs) 1))
      (progn
        (setq doc (vla-Item docs i))
        (setq lst (cons (list n (vl-catch-all-apply 'vla-Close (list doc 0))) lst)))
      (setq i (1+ i))))          ; 关掉后不递增：后续文档会前移，重试同一位置
  (reverse lst))"""


def run_layout(n, src, out, host=None, settle=2.0):
    """在 CAD 上跑布局 n 并另存到 out（必须是全新文件名）。返回布局耗时 ms。"""
    if host:
        os.environ["CAD_MCP_URL"] = host
    tmp = os.path.join(HERE, "vltmp-L%d.dwg" % n)
    h_tmp, h_out = mc.host_path(tmp), mc.host_path(out)
    mc.eval_lisp(CLOSE_LISP, timeout=120)                  # 清掉上轮残留的幽灵文档
    for h in (h_tmp, h_out):
        mc.eval_lisp('(if (findfile "%s") (vl-file-delete "%s"))' % (h, h), timeout=120)
    mc.eval_lisp('(vl-file-copy "%s" "%s")' % (mc.host_path(src), h_tmp), timeout=180)
    mc.call("open_dwg", {"filePath": h_tmp.replace("/", "\\")}, timeout=300)
    time.sleep(settle)
    mc.ensure_package(HERE, force=True)           # open_dwg 会重置 LISP 命名空间
    r = str(mc.eval_lisp(RUN_LISP % n, timeout=600)).strip()
    num = re.search(r"-?\d+", r)
    ms = int(num.group()) if num else -1
    mc.eval_lisp(SAVE_LISP % (h_out, h_out), timeout=300)
    return ms


# ---------------------------------------------------------------- 主流程

def report(rows):
    print()
    print("%-4s %-12s %7s %6s %6s %6s %8s %9s  %s"
          % ("布局", "名称", "耗时ms", "TEXT", "PLINE", "非正交", "交叉", "平均引线", "问题"))
    nbad = 0
    for n, stat, bad in rows:
        nbad += len(bad)
        print("%-4s %-12s %7s %6s %6s %6s %8s %9s  %s"
              % ("L%d" % n, LAYOUT_NAMES.get(n, "?"), stat.get("ms", "-"),
                 stat.get("text", "-"), stat.get("pline", "-"),
                 stat.get("non_orth", "-"), stat.get("cross", "-"),
                 stat.get("avg_len", "-"), "；".join(bad) or "OK"))
    print()
    if nbad:
        print("发现 %d 个问题，见上表「问题」列。" % nbad)
    else:
        print("全部通过。")
    return 1 if nbad else 0


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--layouts", help="逗号分隔的布局编号，如 0,8")
    ap.add_argument("--src", default=DEFAULT_SRC, help="底图 dwg")
    ap.add_argument("--host", help="CAD MCP 地址，如 http://192.168.1.55:8110")
    ap.add_argument("--include", default="", help="这些编号不跳过（如 --include 6）")
    ap.add_argument("--verify-only", action="store_true",
                    help="只校验已有 dim-leader-L*.dwg，完全不碰 CAD")
    ap.add_argument("--keep", action="store_true", help="保留临时文件")
    args = ap.parse_args()

    if args.host:
        os.environ["CAD_MCP_URL"] = args.host
    forced = {int(x) for x in args.include.split(",") if x.strip()}
    lays = ([int(x) for x in args.layouts.split(",")] if args.layouts
            else [n for n in sorted(LAYOUT_NAMES) if n not in DEFAULT_SKIP])
    lays = [n for n in lays if n not in (set(DEFAULT_SKIP) - forced) or n in forced]
    if not os.path.exists(args.src):
        sys.exit("底图不存在: %s" % args.src)

    base_stat, _ = check_dwg(args.src)
    arc0, text0 = base_stat.get("arc"), base_stat.get("text")
    _selftest()

    if args.verify_only:
        rows = []
        for n in lays:
            p = os.path.join(HERE, "dim-leader-L%d.dwg" % n)
            if not os.path.exists(p):
                rows.append((n, {}, ["文件不存在"]))
                continue
            stat, bad = check_dwg(p, arc0, text0)
            rows.append((n, stat, bad))
        return report(rows)

    mc.eval_lisp('(getvar "ACADVER")', timeout=60)          # 触发服务器选择
    print("MCP: %s" % mc.BASE)
    rows = []
    for n in lays:
        final = os.path.join(HERE, "dim-leader-L%d.dwg" % n)
        staged = os.path.join(HERE, "vltmp-out-L%d.dwg" % n)
        print("--- 布局 %d %s ---" % (n, LAYOUT_NAMES.get(n, "")), flush=True)
        try:
            ms = run_layout(n, args.src, staged, args.host)
            stat, bad = check_dwg(staged, arc0, text0)
            stat["ms"] = ms
            if bad:
                print("    校验未通过，暂存为 %s" % os.path.basename(staged), flush=True)
            else:
                os.replace(staged, final)                  # 校验通过才落到正式名
                print("    已写入 %s" % os.path.basename(final), flush=True)
            rows.append((n, stat, bad))
        except Exception as e:                              # noqa: BLE001
            print("    失败: %r" % (e,), flush=True)
            rows.append((n, {}, ["异常: %r" % (e,)]))
            if os.path.exists(staged):
                os.remove(staged)
        finally:
            # 只清工作副本与锁文件。staged 若已保存就是 CAD 当前打开的文档，
            # 删它会留下指向已删文件的打开文档 + 残留 .dwl 锁。
            # 校验失败时也要保留 staged 供排查，故不在此处删。
            if not args.keep:
                import glob
                for p in [os.path.join(HERE, "vltmp-L%d.dwg" % n)] + \
                         glob.glob(os.path.join(HERE, "vltmp-*.dwl*")):
                    if os.path.exists(p) and p != final:
                        os.remove(p)
    return report(rows)


if __name__ == "__main__":
    sys.exit(main() or 0)
