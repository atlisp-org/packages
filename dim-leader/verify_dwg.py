"""独立校验 dim-leader 的输出 DWG（用 ezdxf，不依赖包内代码）。

检查项（PLAN.md F7 验收口径）:
  box_overlap  文本盒两两重叠数        期望 0
  collinear    引线段共线重叠段对数    期望 0
  non_orth     非正交段数              期望 0
  text_on_leader 文字插入点落在他人引线段上的个数  期望 0

文本盒用独立公式（不调用 dim-leader:text-box，避免自证）：宽度按
len(txt)*h*width_factor*0.6 估算，该值略大于 CAD 的 textbox，属保守估计。

用法: python3 verify_dwg.py test-L8.dwg
"""
import math
import sys

import ezdxf

EPS = 1e-6


def world_box(t):
    """按 DXF 组码独立计算文本的世界包围盒 (bl, br, bb, bt)。"""
    a = t.dxfattribs()
    h = float(a.get("height", 2.5))
    wf = float(a.get("width", 1.0)) or 1.0
    txt = a.get("text", "") or ""
    if txt.startswith("\\"):                       # 去掉 \\A1; 之类的格式码
        txt = txt.split(";")[-1]
    w = max(1.0, len(txt) * h * wf * 0.6)
    p = a.get("align_point", a.get("insert", (0, 0, 0)))
    x, y = float(p[0]), float(p[1])
    halign = int(a.get("halign", 0) or 0)
    valign = int(a.get("valign", 0) or 0)
    # 水平定位点 -> 盒左边界偏移
    dx = {-1: -w, 0: 0.0, 1: -w / 2.0, 2: -w / 2.0, 3: -w / 2.0, 4: -w}.get(halign, 0.0)
    dy = {-1: -h, 0: 0.0, 1: -h / 2.0, 2: -h / 2.0, 3: -h / 2.0,
          4: 0.0, 5: -h / 2.0}.get(valign, -h / 2.0)
    rot = float(a.get("rotation", 0.0) or 0.0) % 180.0
    if rot < 1.0 or rot > 179.0:                  # 近似水平
        return (x + dx, x + dx + w, y + dy, y + dy + h)
    return (x - h / 2.0, x + h / 2.0, y, y + w)   # 竖排


def main():
    path = sys.argv[1] if len(sys.argv) > 1 else "test-L8.dwg"
    dxf = path.rsplit(".", 1)[0] + ".dxf"
    import os
    import subprocess
    if not os.path.exists(dxf) or os.path.getmtime(dxf) < os.path.getmtime(path):
        subprocess.run(["dwg2dxf", "-o", dxf, path], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    msp = ezdxf.readfile(dxf).modelspace()

    texts = [t for t in msp.query("TEXT") if not (t.dxfattribs().get("text") or "").strip().isdigit()]
    boxes = [world_box(t) for t in msp.query("TEXT")]
    segs, npl = [], 0
    for e in msp.query("LWPOLYLINE"):
        npl += 1
        pts = [(float(p[0]), float(p[1])) for p in e.get_points()]
        segs += list(zip(pts, pts[1:]))

    # 盒重叠（扫描线：按 x 排序 + 活动表）
    order = sorted(range(len(boxes)), key=lambda i: boxes[i][0])
    ov, active = 0, []
    for i in order:
        bi = boxes[i]
        active = [j for j in active if boxes[j][1] > bi[0] + EPS]
        for j in active:
            bj = boxes[j]
            if (bj[3] > bi[2] + EPS and bj[2] < bi[3] + EPS
                    and bj[1] > bi[0] + EPS and bj[0] < bi[1] + EPS):
                ov += 1
        active.append(i)

    # 正交性 + 共线重叠
    nonorth, grp = 0, {}
    for (x1, y1), (x2, y2) in segs:
        hz, vz = abs(y1 - y2) < EPS, abs(x1 - x2) < EPS
        if not (hz or vz):
            nonorth += 1
            continue
        if hz:
            grp.setdefault(("h", round(y1, 3)), []).append((min(x1, x2), max(x1, x2)))
        else:
            grp.setdefault(("v", round(x1, 3)), []).append((min(y1, y2), max(y1, y2)))
    col = 0
    for v in grp.values():
        v.sort()
        col += sum(1 for a, b in zip(v, v[1:]) if a[1] > b[0] + EPS)

    # 文字插入点是否落在引线段上
    onp = 0
    for t in msp.query("TEXT"):
        a = t.dxfattribs()
        p = a.get("align_point", a.get("insert", (0, 0, 0)))
        px, py = float(p[0]), float(p[1])
        for (x1, y1), (x2, y2) in segs:
            if abs(y1 - y2) < EPS and abs(py - y1) < EPS and \
                    min(x1, x2) - EPS <= px <= max(x1, x2) + EPS:
                onp += 1
                break

    avg = sum(abs(b[0] - a[0]) + abs(b[1] - a[1]) for a, b in segs) / max(1, npl)
    print(f"{path}: text={len(boxes)} leader={npl} pts={len(segs)} "
          f"avg_leader={avg:.1f}")
    print(f"  box_overlap={ov}  collinear={col}  non_orth={nonorth}  text_on_leader={onp}")
    print("  PASS" if ov == 0 and col == 0 and nonorth == 0 and onp == 0 else "  FAIL")


if __name__ == "__main__":
    main()
