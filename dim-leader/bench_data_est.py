"""从 bench_src.json 生成一个"临时" data.json（文本宽按字符数估算）。

只用于本地自测 Python 实现；正式 A/B 对比必须用 bench_data.py 从 CAD 抽取的
data.json（文本宽取 CAD 的 textbox 结果）。
"""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))


def main():
    src = json.load(open(os.path.join(HERE, "bench_src.json"), encoding="utf-8"))
    ratio = float(os.environ.get("W_RATIO", "0.79"))   # textbox/字符估算 比值
    matched, obstacles = [], []
    for m in src["matched"]:
        h = m["h"]
        w = max(1.0, len(m["txt"]) * h * 0.7 * 0.6 * ratio)
        matched.append([m["ctr"][0], m["ctr"][1], m["ref"][0], m["ref"][1],
                        round(w, 9), h, m["s"]])
    for o in src["obstacles"]:
        h = o["h"]
        w = max(1.0, len(o["txt"]) * h * 0.7 * 0.6 * ratio)
        obstacles.append([o["ref"][0] - w / 2.0, o["ref"][0] + w / 2.0,
                          o["ref"][1] - h / 2.0, o["ref"][1] + h / 2.0])
    data = {"dwg": "estimated", "n_matched": len(matched),
            "n_obstacles": len(obstacles),
            "config": {"gap": 1.0, "columns": 4, "rotate": 1, "avoid-leader": 0,
                       "zones": 8, "offset": 2.0, "lane-gap": 1.0,
                       "channels-blocks": 10, "channels-columns": 1,
                       "channels-lines": 0, "side-distance": 10.0,
                       "leader-width": None, "layer": "", "layout": 0,
                       "match-tol": 0.5, "leader-colors": ""},
            "matched": matched, "obstacles": obstacles}
    out = os.path.join(HERE, "data_est.json")
    json.dump(data, open(out, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
    ws = [m[4] for m in matched]
    print(f"-> {out}  matched={len(matched)} obstacles={len(obstacles)} "
          f"w:{min(ws):.2f}~{max(ws):.2f}")


if __name__ == "__main__":
    main()
