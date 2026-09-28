"""验收: 对布局 JSON 做几何检查(与 PLAN.md F7 验收项一致)。

  box_overlap  文本盒两两重叠数 (期望 0)
  collinear    引线段共线重叠段对数 (期望 0)
  non_orth     非正交段数 (期望 0)
"""
import json
import sys

EPS = 1e-6


def text_box(ax, ay, rot, w, h):
    if rot % 180 == 0:
        return (ax, ax + w, ay - h / 2.0, ay + h / 2.0)
    return (ax - h / 2.0, ax + h / 2.0, ay, ay + w)


def segments(routes):
    segs = []
    for r in routes:
        for a, b in zip(r, r[1:]):
            segs.append((a, b))
    return segs


def main():
    laid = json.load(open(sys.argv[1], encoding="utf-8"))
    data = json.load(open(sys.argv[2], encoding="utf-8")) if len(sys.argv) > 2 else None
    dims = {m["handle"]: m for m in (data["matched"] if data else [])}

    boxes, segs = [], []
    for it in laid:
        m = dims.get(it["handle"], {})
        w = m.get("w", 10.0)
        h = m.get("height", 2.5)
        boxes.append(text_box(it["ax"], it["ay"], it["rot"], w, h))
        segs += [(a, b) for a, b in zip(it["route"], it["route"][1:])]

    # 盒重叠
    ov = 0
    n = len(boxes)
    xs = sorted(range(n), key=lambda i: boxes[i][0])
    active = []
    for i in xs:
        bi = boxes[i]
        active = [j for j in active if boxes[j][1] > bi[0] + EPS]
        for j in active:
            bj = boxes[j]
            if (bj[3] > bi[2] + EPS and bj[2] < bi[3] + EPS
                    and bj[1] > bi[0] + EPS and bj[0] < bi[1] + EPS):
                ov += 1
        active.append(i)

    # 正交性 + 共线重叠
    non_orth = 0
    groups = {}
    for (x1, y1), (x2, y2) in segs:
        hz = abs(y1 - y2) < EPS
        vz = abs(x1 - x2) < EPS
        if not (hz or vz):
            non_orth += 1
            continue
        if hz:
            lo, hi = min(x1, x2), max(x1, x2)
            groups.setdefault(("h", round(y1, 3)), []).append((lo, hi))
        else:
            lo, hi = min(y1, y2), max(y1, y2)
            groups.setdefault(("v", round(x1, 3)), []).append((lo, hi))
    col = 0
    for spans in groups.values():
        spans.sort()
        for a, b in zip(spans, spans[1:]):
            if a[1] > b[0] + EPS:
                col += 1

    avg_len = sum(abs(b[0] - a[0]) + abs(b[1] - a[1])
                  for a, b in segs) / max(1, len(laid))
    print(f"items={len(laid)}  box_overlap={ov}  collinear={col}  "
          f"non_orth={non_orth}  avg_leader={avg_len:.1f}")
    ok = ov == 0 and col == 0 and non_orth == 0
    print("PASS" if ok else "FAIL")
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
