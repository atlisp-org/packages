"""在 CAD 里重建基准场景（高程点图_局部.dwg 的 507 个圆心标注点）。

CAD 与本机不共享文件系统，无法把 DWG 送过去；改为用 bench_src.json（本地用
ezdxf 从真实 DWG 抽出的圆心/文本）在 CAD 中 entmakex 出同样的实体，
再由 CAD 侧跑 find-centers/match-texts 得到基准输入（文本宽以 CAD 的 textbox 为准）。

实体：每个匹配点 = 2 条半圆弧(0-180 / 180-360，同心) + 1 个 TEXT(对齐点=ref, 72=0, 73=2)；
      每个障碍文本 = 1 个 TEXT。用法: python3 bench_scene.py [bench_src.json]
"""
import json
import os
import sys

import mcp_client

HERE = os.path.dirname(os.path.abspath(__file__))
SCENE_DWG = "C:/Users/Public/bench-scene.dwg"   # CAD 主机上的基准场景（供重复打开）
CHUNK = 50
RADIUS = 1.0

ERASE_ALL = r"""
(progn
  (command "_.erase" "_all" "")
  (setq *s* (ssget "_X" '((0 . "ARC,TEXT"))))
  (princ (strcat "left=" (itoa (if *s* (sslength *s*) 0)))))
"""


PI = 3.141592653589793
ROT90 = "1.5707963267948966"   # 该 CAD 的角度组码(50/51)以弧度计


def num(x):
    return f"{x:.6f}"


def text_code(ref, h, txt, rot=ROT90):
    return (f'(entmakex (list (quote (0 . "TEXT")) (cons 10 (list {num(ref[0])} '
            f'{num(ref[1])} 0.0)) (cons 11 (list {num(ref[0])} {num(ref[1])} 0.0)) '
            f'(cons 40 {num(h)}) (cons 1 "{txt}") (cons 7 "Standard") '
            f'(cons 41 0.7) (cons 50 {rot}) (cons 72 0) (cons 73 2)))')


def arc_code(ctr, a0, a1):
    """a0/a1 为弧度（该 CAD 的 entget 对 ARC 50/51 返回弧度）。"""
    return (f'(entmakex (list (quote (0 . "ARC")) (cons 10 (list {num(ctr[0])} '
            f'{num(ctr[1])} 0.0)) (cons 40 {num(RADIUS)}) (cons 50 {a0!r}) '
            f'(cons 51 {a1!r})))')


def build_spot(m):
    c = m["ctr"]
    return [arc_code(c, 0.0, PI), arc_code(c, PI, 2 * PI),
            text_code(m["ref"], m["h"], m["txt"])]


def main():
    src = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "bench_src.json")
    data = json.load(open(src, encoding="utf-8"))
    mcp_client.ensure_package(HERE)

    print("erase:", mcp_client.eval_lisp(ERASE_ALL, timeout=300).strip())

    n = 0
    for i in range(0, len(data["matched"]), CHUNK):
        chunk = data["matched"][i:i + CHUNK]
        code = "(progn " + " ".join(c for m in chunk for c in build_spot(m)) + " nil)"
        mcp_client.eval_lisp(code, timeout=300)
        n += len(chunk)
        print(f"  spots {n}/{len(data['matched'])}", flush=True)

    for i in range(0, len(data["obstacles"]), CHUNK):
        chunk = data["obstacles"][i:i + CHUNK]
        code = "(progn " + " ".join(
            text_code(o["ref"], o["h"], o["txt"]) for o in chunk) + " nil)"
        mcp_client.eval_lisp(code, timeout=300)
    print(f"  obstacles {len(data['obstacles'])}")

    check = r"""
(progn
  (defun c:n (s) (if s (sslength s) 0))
  (setq *c* (dim-leader:find-centers)
        *m* (dim-leader:match-texts *c*))
  (princ (strcat "centers=" (itoa (length *c*))
                 " matched=" (itoa (length *m*))
                 " arcs=" (itoa (c:n (ssget "_X" '((0 . "ARC")))))
                 " texts=" (itoa (c:n (ssget "_X" '((0 . "TEXT"))))))))
"""
    print("verify:", mcp_client.eval_lisp(check, timeout=600).strip())
    if os.environ.get("BENCH_SAVE_SCENE", "1") == "1":
        r = mcp_client.call("save_dwg", {"filePath": SCENE_DWG}, timeout=300)
        print("saved:", r.strip(), "->", SCENE_DWG)


if __name__ == "__main__":
    main()
