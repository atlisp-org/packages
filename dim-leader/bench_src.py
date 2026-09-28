"""从真实 DWG 抽取 dim-leader 的输入（圆心 + 匹配文本 + 障碍文本）→ bench_src.json。

在本地用 ezdxf 复刻 dim-leader:find-centers / match-texts 的判定，
使基准数据与 高程点图_局部.dwg 的真实分布一致（507 点）。
文本宽 w 稍后由 CAD 的 (textbox) 回读，见 bench_data.py。

用法: python3 bench_src.py [dwg] [out.json]
"""
import json
import math
import os
import subprocess
import sys
import tempfile

import ezdxf

HERE = os.path.dirname(os.path.abspath(__file__))
PI = math.pi


def span_to_half(d, full, target, tol):
    a = math.fmod(d, full)
    if a < 0.0:
        a += full
    if a > full / 2.0:
        a = full - a
    return abs(a - target) < tol


def half_circle_p(sa, ea):
    d = ea - sa
    return span_to_half(d, 2 * PI, PI, 0.02) or span_to_half(d, 360.0, 180.0, 0.5)


def center_key(p):
    return (round(p[0], 3), round(p[1], 3))


def ref_of(e):
    """对应 (cdr (assoc 11 dat))，缺失时退回 (cdr (assoc 10 dat))。"""
    a = e.dxfattribs()
    p = a.get("align_point", a.get("insert", (0.0, 0.0, 0.0)))
    return tuple(p)


def to_dxf(dwg):
    dxf = os.path.join(tempfile.gettempdir(), "bench_src.dxf")
    if not os.path.exists(dxf) or os.path.getmtime(dxf) < os.path.getmtime(dwg):
        subprocess.run(["dwg2dxf", "-o", dxf, dwg], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return ezdxf.readfile(dxf)


def main():
    dwg = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "高程点图_局部.dwg")
    out = sys.argv[2] if len(sys.argv) > 2 else os.path.join(HERE, "bench_src.json")
    offset, tol = 2.0, 0.5

    doc = to_dxf(dwg)
    msp = doc.modelspace()

    groups = {}
    for e in msp.query("ARC"):
        try:
            cen = tuple(e.dxf.center)[:2]
            sa, ea = e.dxf.start_angle, e.dxf.end_angle
        except Exception:
            continue
        groups.setdefault(center_key(cen), []).append((sa, ea, cen))
    centers = [g[0][2] for g in groups.values()
               if len(g) == 2 and all(half_circle_p(a, b) for a, b, _ in g)]

    texts = []
    for e in msp.query("TEXT"):
        r = ref_of(e)
        try:
            txt = e.plain_text()
        except Exception:
            txt = str(e.dxf.get("text", ""))
        texts.append((tuple(r)[:2], float(e.dxf.height), txt))

    matched, used = [], set()
    cands = []
    for ctr in centers:
        cx, cy = ctr
        for i, (ref, h, txt) in enumerate(texts):
            for s in (1, -1):
                d = math.hypot(cx + s * offset - ref[0], cy - ref[1])
                if d < tol:
                    cands.append((d, ctr, i, s))
    cands.sort(key=lambda c: c[0])
    usedc, usedt = set(), set()
    for d, ctr, i, s in cands:
        k = center_key(ctr)
        if k in usedc or i in usedt:
            continue
        usedc.add(k)
        usedt.add(i)
        ref, h, txt = texts[i]
        matched.append({"ctr": [round(ctr[0], 6), round(ctr[1], 6)],
                        "ref": [round(ref[0], 6), round(ref[1], 6)],
                        "h": round(h, 6), "txt": txt, "s": s})

    obst = [{"ref": [round(r[0], 6), round(r[1], 6)], "h": round(h, 6),
             "txt": t} for i, (r, h, t) in enumerate(texts) if i not in usedt]

    data = {"src_dwg": os.path.basename(dwg), "offset": offset, "tol": tol,
            "centers": len(centers), "matched": matched, "obstacles": obst}
    json.dump(data, open(out, "w", encoding="utf-8"), ensure_ascii=False, indent=1)

    xs = [m["ctr"][0] for m in matched]
    ys = [m["ctr"][1] for m in matched]
    hs = {m["h"] for m in matched}
    print(f"arc_centers={len(centers)} matched={len(matched)} "
          f"obstacles={len(obst)}  -> {out}")
    print(f"x:[{min(xs):.2f},{max(xs):.2f}] y:[{min(ys):.2f},{max(ys):.2f}] "
          f"heights={sorted(hs)[:5]}")


if __name__ == "__main__":
    main()
