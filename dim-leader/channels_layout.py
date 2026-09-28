"""dim-leader 通道布线（布局 8, F7）Python 参考实现

与 dim-leader.lsp 的 `dim-leader:layout-channels` 等价，用于先验证再移植。

输入 JSON（与 chip_layout.py 相同）:
  {"config": {"gap": 1.0, "lane_gap": 1.0},
   "matched": [{"handle","ctr":[x,y],"w","height"}, ...]}

输出 JSON: [{"handle","ax","ay","hj","vj","rot","route":[[x,y],...]}, ...]

算法:
  1. 圆心按到点群外接矩形四边的最近距离分成 上/下/左/右 四个带;
  2. 每带沿带方向按 (字高+gap) 分列成网格, 列顺序与圆心顺序一致;
  3. 每行靠点群一侧留泳道区, 用 left-edge 分配独占泳道(同泳道区间不重叠);
  4. 引线 = 圆心 ->(深度) 泳道 ->(沿带) 列 ->(深度) 文字边, 全正交。
  竖直/水平段只落在圆心坐标或列坐标上, 保证引线零共线重叠。
"""
import argparse
import json
import math

EPS = 1e-9


def left_edge(spans):
    order = sorted(range(len(spans)), key=lambda k: (spans[k][0], spans[k][1]))
    ends = []
    assign = [0] * len(spans)
    for k in order:
        s, e = spans[k]
        if s > e:
            s, e = e, s
        done = False
        for t in range(len(ends)):
            if ends[t] <= s + EPS:
                ends[t] = e
                assign[k] = t
                done = True
                break
        if not done:
            ends.append(e)
            assign[k] = len(ends) - 1
    return assign, len(ends)


def spread(n, lo, hi):
    if n == 1:
        return [(lo + hi) / 2.0]
    return [lo + (hi - lo) * k / (n - 1.0) for k in range(n)]


def isotonic(p, pitch):
    """等间距约束(pitch)下尽量贴近 p 的列位(PAVA 保序回归, L2)."""
    v0 = [p[k] - k * pitch for k in range(len(p))]
    blocks = []
    for x in v0:
        blocks.append([x, 1])
        while len(blocks) >= 2 and blocks[-2][0] / blocks[-2][1] > blocks[-1][0] / blocks[-1][1]:
            s, c = blocks.pop(); ps, pc = blocks.pop()
            blocks.append([ps + s, pc + c])
    v = []
    for s, c in blocks:
        v += [s / c] * c
    return [v[k] + k * pitch for k in range(len(p))]


def layout_channels(matched, config):
    gap = config.get("gap") or 1.0
    lane = config.get("lane_gap") or gap
    cols = config.get("columns", 1)
    if not matched:
        return []
    for m in matched:
        if not m.get("height"):
            m["height"] = 2.5
        if not m.get("w"):
            m["w"] = m["height"] * 4.0
    xs = [m["ctr"][0] for m in matched]
    ys = [m["ctr"][1] for m in matched]
    xL, xR, yB, yU = min(xs), max(xs), min(ys), max(ys)
    maxh = max(m["height"] for m in matched)

    groups = {"T": [], "B": [], "L": [], "R": []}
    for m in matched:
        cx, cy = m["ctr"]
        d = {"T": yU - cy, "B": cy - yB, "L": cx - xL, "R": xR - cx}
        groups[min(d, key=d.get)].append(m)

    out = []

    def band_h(g, above):
        if not g:
            return
        g = sorted(g, key=lambda z: z["ctr"][0])
        per = max(1, int((xR - xL) / (maxh + gap)))
        lines = [g[i:i + per] for i in range(0, len(g), per)]
        cursor = (yU + gap) if above else (yB - gap)
        for line in lines:
            n = len(line)
            if cols == 1:
                us = isotonic([z["ctr"][0] for z in line], maxh + gap)
            else:
                us = spread(n, xL, xR)
            spans = [(min(line[k]["ctr"][0], us[k]), max(line[k]["ctr"][0], us[k]))
                     for k in range(n)]
            asg, nt = left_edge(spans)
            depth = max(z["w"] for z in line)
            if above:
                textb = cursor + nt * lane + gap
            else:
                textt = cursor - nt * lane - gap
            for k, z in enumerate(line):
                u = us[k]
                ly = (cursor + (asg[k] + 0.5) * lane) if above else (cursor - (asg[k] + 0.5) * lane)
                if above:
                    route = [[z["ctr"][0], z["ctr"][1]], [z["ctr"][0], ly], [u, ly], [u, textb]]
                    out.append({"handle": z["handle"], "ax": u, "ay": textb + z["w"] / 2.0,
                                "rot": 90, "hj": 1, "vj": 2, "route": route})
                else:
                    route = [[z["ctr"][0], z["ctr"][1]], [z["ctr"][0], ly], [u, ly], [u, textt]]
                    out.append({"handle": z["handle"], "ax": u, "ay": textt - z["w"] / 2.0,
                                "rot": 90, "hj": 1, "vj": 2, "route": route})
            cursor = (textb + depth + gap) if above else (textt - depth - gap)

    def band_v(g, right):
        if not g:
            return
        g = sorted(g, key=lambda z: z["ctr"][1])
        per = max(1, int((yU - yB) / (maxh + gap)))
        lines = [g[i:i + per] for i in range(0, len(g), per)]
        cursor = (xR + gap) if right else (xL - gap)
        for line in lines:
            n = len(line)
            if cols == 1:
                us = isotonic([z["ctr"][1] for z in line], maxh + gap)
                if us[0] < yB:
                    us = [v + (yB - us[0]) for v in us]
                elif us[-1] > yU:
                    us = [v + (yU - us[-1]) for v in us]
            else:
                us = spread(n, yB, yU)
            spans = [(min(line[k]["ctr"][1], us[k]), max(line[k]["ctr"][1], us[k]))
                     for k in range(n)]
            asg, nt = left_edge(spans)
            depth = max(z["w"] for z in line)
            if right:
                textl = cursor + nt * lane + gap
            else:
                textr = cursor - nt * lane - gap
            for k, z in enumerate(line):
                u = us[k]
                lx = (cursor + (asg[k] + 0.5) * lane) if right else (cursor - (asg[k] + 0.5) * lane)
                if right:
                    route = [[z["ctr"][0], z["ctr"][1]], [lx, z["ctr"][1]], [lx, u], [textl, u]]
                    out.append({"handle": z["handle"], "ax": textl, "ay": u,
                                "rot": 0, "hj": 0, "vj": 2, "route": route})
                else:
                    route = [[z["ctr"][0], z["ctr"][1]], [lx, z["ctr"][1]], [lx, u], [textr, u]]
                    out.append({"handle": z["handle"], "ax": textr, "ay": u,
                                "rot": 0, "hj": 2, "vj": 2, "route": route})
            cursor = (textl + depth + gap) if right else (textr - depth - gap)

    band_h(groups["T"], True)
    band_h(groups["B"], False)
    band_v(groups["R"], True)
    band_v(groups["L"], False)
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("src", help="输入数据 JSON")
    ap.add_argument("out", help="输出布局 JSON")
    ap.add_argument("--lane-gap", type=float, default=None, help="相邻平行引线间距")
    a = ap.parse_args()
    data = json.load(open(a.src, encoding="utf-8"))
    cfg = dict(data.get("config", {}))
    if a.lane_gap:
        cfg["lane_gap"] = a.lane_gap
    res = layout_channels(data["matched"], cfg)
    json.dump(res, open(a.out, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
    print(f"laid={len(res)} -> {a.out}")


if __name__ == "__main__":
    main()
