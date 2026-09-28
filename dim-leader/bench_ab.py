"""在 CAD 里测量 9 种布局的 AutoLISP 耗时（每个布局一次调用，避免 MCP 卡死窗口）。

口径与 Python 侧一致：
- 只测布局函数本身，不含写图 entmake；
- 布局内部每条匹配都会 entget + textbox 取文本宽/参考点，这段与算法无关，
  另测 c:metrics 基线，报告里同时给出"含度量"和"扣除度量"的耗时；
- 每布局跑 rep 次取最小值（MILLISECS 计时，处理 32 位回绕）；
- 布局 6 的代价是 O(标签数 × 通道候选 × (障碍盒+keepout))，507 点不可行，
  改用截断到 --n6 点的输入单独测并单独标注。

用法: python3 bench_ab.py [data.json] [--rep 3] [--n6 16] [--only 0,8]
"""
import argparse
import json
import os
import sys

import mcp_client

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import bench_data  # noqa: E402  复用场景校验与 PREP

SETUP = """
(progn
  (defun c:limit (n lst / r i)
    (setq r nil i 0)
    (while (and (< i n) lst) (setq r (cons (car lst) r)) (setq lst (cdr lst)) (setq i (1+ i)))
    r)
  (defun c:ms () (getvar "MILLISECS"))
  (defun c:dt (a b) (if (< b a) (- (+ b a) 4294967296.0) (- b a)))
  ;; 只做 entget + textbox 的度量提取，作为布局净耗时的基线
  (defun c:metrics (a b / m d rf tb w s)
    (setq s 0.0)
    (foreach m a
      (setq d (entget (nth 1 m))
            rf (cdr (assoc 11 d)))
      (if (null rf) (setq rf (cdr (assoc 10 d))))
      (setq tb (textbox d)
            w (if tb (abs (- (car (cadr tb)) (caar tb)))
                   (dim-leader:text-fallback-width d)))
      (setq s (+ s w)))
    s)
  (defun c:l0 (a b) (dim-leader:layout a b))
  (defun c:l1 (a b) (dim-leader:layout-table a b))
  (defun c:l2 (a b) (dim-leader:layout-table-zone a b (@:get-config 'dim-leader:zones)))
  (defun c:l3 (a b) (dim-leader:layout-zone2d a b 3))
  (defun c:l4 (a b) (dim-leader:layout-4dir a b))
  (defun c:l5 (a b) (dim-leader:layout-lanes a b))
  (defun c:l6 (a b) (dim-leader:layout-chip a b))
  (defun c:l7 (a b) (dim-leader:layout-compact a b))
  (defun c:l8 (a b) (dim-leader:layout-channels a b))
  (defun c:bench (fn a b rep / i t0 t1 d best n)
    (setq best -1.0 n -1 i 0)
    (while (< i rep)
      (setq t0 (c:ms))
      (setq *r* (vl-catch-all-apply fn (list a b)))
      (setq t1 (c:ms))
      (if (not (vl-catch-all-error-p *r*))
        (progn
          (setq d (c:dt t0 t1))
          (if (or (< best 0.0) (< d best)) (setq best d))
          (setq n (if (listp *r*) (length *r*) -1))))
      (setq i (1+ i)))
    (list best n))
  (princ "setup ok"))
"""


def setup(blocks=None):
    bench_data.require_scene()
    mcp_client.eval_lisp(bench_data.PREP, timeout=1200)
    n = int(mcp_client.eval_lisp("(itoa (length *bm*))", timeout=120).strip())
    print("setup:", mcp_client.eval_lisp(SETUP, timeout=300).strip(),
          f"| matched={n}")
    if blocks is not None:
        mcp_client.eval_lisp(
            f"(@:set-config 'dim-leader:channels-blocks {blocks})", timeout=120)
    return n


def bench_one(fn, rep, n6=16, use6set=False):
    args = ("(c:limit %d *bm*) (c:limit %d *bo*)" % (n6, n6)
            if use6set else "*bm* *bo*")
    code = f"(c:bench '{fn} {args} {rep})"
    out = mcp_client.eval_lisp(code, timeout=1800).strip()[1:-1].split()
    return float(out[0]), int(out[1])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("data", nargs="?", default=os.path.join(HERE, "data.json"))
    ap.add_argument("--rep", type=int, default=3)
    ap.add_argument("--n6", type=int, default=16)
    ap.add_argument("--only", default="")
    ap.add_argument("--blocks", type=int, default=None,
                    help="设置 dim-leader:channels-blocks（缺省沿用 CAD 当前值）")
    ap.add_argument("--out", default=os.path.join(HERE, "lisp_bench.json"))
    a = ap.parse_args()

    n_all = setup(a.blocks)
    ids = ([int(x) for x in a.only.split(",") if x != "m"] if a.only
           else [0, 1, 2, 3, 4, 5, 7, 8, 6])
    res, met = {}, None
    if not a.only or "m" in a.only:
        met = bench_one("c:metrics", a.rep)
        print(f"metrics(entget+textbox) {met[0]:9.0f} ms", flush=True)
    for lid in ids:
        ms, n = bench_one(f"c:l{lid}", a.rep, a.n6, use6set=(lid == 6))
        res[lid] = {"ms": ms, "n": n,
                    "n_in": a.n6 if lid == 6 else n_all}
        print(f"  L{lid}: {ms:9.0f} ms  n={n}  (输入 {a.n6 if lid == 6 else n_all})",
              flush=True)
    out = {"host": mcp_client.BASE, "dwg": n_all, "rep": a.rep,
           "metrics_ms": met[0] if met else None, "layouts": res}
    json.dump(out, open(a.out, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
    print("->", a.out)


if __name__ == "__main__":
    main()
