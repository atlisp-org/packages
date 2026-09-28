"""dim-leader 紧凑布局（Python 参考实现）

由 dim-leader.lsp 导出的数据 JSON 计算文字/引线布局：
  - 圆心按 X 对半分配左右两侧（数量接近）
  - 每侧按圆心 Y 中位数分上/下两组，文字带绕点群中心 Y 居中
  - 文字独占行、K 列交错，行距=字高+rowGap、列距=字宽+colGap
  - 引线为 3 段正交折线：圆心 ->(横) lane ->(竖) ty ->(横) 文字内缘
  - 可选：在圆心处加 Y 向折线偏移，贪心+迭代最小化引线交叉

输入 JSON（由 atlisp 侧导出）:
  {
    "config": {"gap": 1.0, ...},
    "centers": [[x,y], ...],
    "matched": [{"handle","ctr":[x,y],"ref":[x,y],"height","width_factor",
                 "rot","72","73","w","text"}, ...],
    "obstacles": [{"handle","box":[bl,br,bb,bt],"text"}, ...]
  }

输出 JSON: [{"handle","ax","ay","hj","vj","rot","route":[[x,y],...]}, ...]
"""

import argparse
import json
import math

EPS = 1.0e-9


def orient(ax, ay, bx, by, cx, cy):
    d = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    if d < -EPS:
        return -1
    if d > EPS:
        return 1
    return 0


def seg_seg(ax, ay, bx, by, cx, cy, dx, dy):
    o1 = orient(ax, ay, bx, by, cx, cy)
    o2 = orient(ax, ay, bx, by, dx, dy)
    if o1 * o2 > 0:
        return False
    o3 = orient(cx, cy, dx, dy, ax, ay)
    o4 = orient(cx, cy, dx, dy, bx, by)
    if o3 * o4 > 0:
        return False
    if min(ax, bx) > max(cx, dx) + EPS:
        return False
    if min(cx, dx) > max(ax, bx) + EPS:
        return False
    if min(ay, by) > max(cy, dy) + EPS:
        return False
    if min(cy, dy) > max(ay, by) + EPS:
        return False
    return True


def pt_in(x, y, bl, br, bb, bt):
    return bl <= x <= br and bb <= y <= bt


def seg_box(x1, y1, x2, y2, bl, br, bb, bt):
    if pt_in(x1, y1, bl, br, bb, bt) or pt_in(x2, y2, bl, br, bb, bt):
        return True
    return (seg_seg(x1, y1, x2, y2, bl, bb, br, bb)
            or seg_seg(x1, y1, x2, y2, br, bb, br, bt)
            or seg_seg(x1, y1, x2, y2, br, bt, bl, bt)
            or seg_seg(x1, y1, x2, y2, bl, bt, bl, bb))


def route_hit_box(route, box):
    for i in range(len(route) - 1):
        if seg_box(route[i][0], route[i][1], route[i + 1][0], route[i + 1][1], *box):
            return True
    return False


def route_hit_route(a, b):
    for i in range(len(a) - 1):
        for j in range(len(b) - 1):
            if seg_seg(a[i][0], a[i][1], a[i + 1][0], a[i + 1][1],
                       b[j][0], b[j][1], b[j + 1][0], b[j + 1][1]):
                return True
    return False


def dedupe(pts):
    res = []
    for p in pts:
        if not res or math.dist(res[-1], p) > 1e-6:
            res.append(p)
    return res


def layout_compact(matched, config, D=None, row_gap=None, col_gap=None, K=3):
    gap = config.get("gap") or 1.0
    D = float(D if D is not None else config.get("side_distance", 20.0))
    row_gap = float(row_gap if row_gap is not None else config.get("row_gap", 0.2))
    col_gap = float(col_gap if col_gap is not None else config.get("col_gap", 0.2))
    minX = min(m["ctr"][0] for m in matched)
    maxX = max(m["ctr"][0] for m in matched)
    minY = min(m["ctr"][1] for m in matched)
    maxY = max(m["ctr"][1] for m in matched)
    yc = (minY + maxY) / 2.0
    srt = sorted(matched, key=lambda m: m["ctr"][0])
    nl = len(srt) // 2
    groups = {2: srt[:nl], 1: srt[nl:]}
    ts = []
    for side in (2, 1):
        grp = groups[side]
        if not grp:
            continue
        gs = sorted(grp, key=lambda m: m["ctr"][1])
        med = gs[len(gs) // 2]["ctr"][1]
        subs = [([m for m in gs if m["ctr"][1] >= med], True),
                ([m for m in gs if m["ctr"][1] < med], False)]
        for u, isup in subs:
            if not u:
                continue
            m = len(u)
            maxh = max(x["height"] for x in u)
            maxw = max(x["w"] for x in u)
            rowPitch = maxh + row_gap
            colPitch = maxw + col_gap
            order = sorted(range(m), key=lambda k: u[k]["ctr"][1], reverse=isup)
            lo, hi = ((maxX + 0.5, maxX + D - 0.5) if side == 1
                      else (minX - D + 0.5, minX - 0.5))
            for r, c in enumerate(order):
                mm = u[c]
                lane_rank = (m - 1 - r) if isup else r
                lane = lo + (hi - lo) * (lane_rank + 0.5) / m
                col = r % K
                if isup:
                    ty = yc + gap / 2.0 + maxh / 2.0 + (m - 1 - r) * rowPitch
                else:
                    ty = yc - gap / 2.0 - maxh / 2.0 - (m - 1 - r) * rowPitch
                w, h = mm["w"], mm["height"]
                if side == 1:
                    tx = maxX + D + col * colPitch
                    box = (tx, tx + w, ty - h / 2.0, ty + h / 2.0)
                    att = (tx - gap / 2.0, ty)
                else:
                    tx = minX - D - col * colPitch
                    box = (tx - w, tx, ty - h / 2.0, ty + h / 2.0)
                    att = (tx + gap / 2.0, ty)
                cx, cy = mm["ctr"]
                ts.append({"handle": mm["handle"], "side": side, "box": box,
                           "ax": tx, "ay": ty, "hj": (0 if side == 1 else 2),
                           "route": dedupe([(cx, cy), (lane, cy), (lane, ty), att])})
    return ts


def optimize_bends(ts, config, H, step=0.25, kmax=24, passes=4):
    gap = config.get("gap") or 1.0
    boxes = [t["box"] for t in ts]
    N = len(ts)
    metas = []
    for t in ts:
        r = t["route"]
        metas.append((r[0], r[1][0], r[2][1], r[3], r[2][1] > r[0][1]))
    offs = [0] + [k * step for k in range(1, kmax + 1)] + [-k * step for k in range(1, kmax + 1)]

    def mk(i, k):
        (cx, cy), lane, ty, att, up = metas[i]
        o = k * H * (1 if up else -1)
        return dedupe([(cx, cy), (cx, cy + o), (lane, cy + o), (lane, ty), att])

    def own_hit(r, i):
        return any(j != i and route_hit_box(r, boxes[j]) for j in range(N))

    idx = sorted(range(N), key=lambda i: metas[i][0][1])
    rs = [None] * N
    for i in idx:
        best = None
        for k in offs:
            r = mk(i, k)
            if own_hit(r, i):
                continue
            c = sum(1 for j in range(N) if rs[j] is not None and route_hit_route(r, rs[j]))
            if best is None or c < best[0]:
                best = (c, r)
        rs[i] = best[1] if best else ts[i]["route"]
    for _ in range(passes):
        imp = 0
        for i in range(N):
            cur = sum(1 for j in range(N) if j != i and route_hit_route(rs[i], rs[j]))
            best = (cur, rs[i])
            for k in offs:
                r = mk(i, k)
                if own_hit(r, i):
                    continue
                c = sum(1 for j in range(N) if j != i and route_hit_route(r, rs[j]))
                if c < best[0]:
                    best = (c, r)
            if best[0] < cur:
                rs[i] = best[1]
                imp += 1
        if imp == 0:
            break
    for i, t in enumerate(ts):
        t["route"] = rs[i]
    return ts


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("src", help="输入数据 JSON")
    ap.add_argument("out", help="输出布局 JSON")
    ap.add_argument("--distance", type=float, default=10.0)
    ap.add_argument("--row-gap", type=float, default=0.2)
    ap.add_argument("--col-gap", type=float, default=0.2)
    ap.add_argument("--columns", type=int, default=3)
    ap.add_argument("--optimize", action="store_true")
    ap.add_argument("--height", type=float, default=None, help="按此字高缩放宽度")
    a = ap.parse_args()
    data = json.load(open(a.src, encoding="utf-8"))
    matched = data["matched"]
    if a.height:
        for m in matched:
            oh = m.get("height") or 3.0
            m["w"] = m["w"] * (a.height / oh)
            m["height"] = a.height
    ts = layout_compact(matched, data.get("config", {}), a.distance, a.row_gap, a.col_gap, a.columns)
    if a.optimize:
        H = max(m["height"] for m in matched)
        ts = optimize_bends(ts, data.get("config", {}), H)
    out = [{"handle": t["handle"], "ax": t["ax"], "ay": t["ay"],
            "hj": t["hj"], "vj": 2, "rot": 0, "route": t["route"]} for t in ts]
    json.dump(out, open(a.out, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
    print(f"laid={len(out)} -> {a.out}")


if __name__ == "__main__":
    main()
