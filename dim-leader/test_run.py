"""在 .53 主机上用真实图纸跑 dim-leader 全流程：识别 → 匹配 → 9 种布局计时 → 写图验收。

图纸走共享目录（W:/atlisp <-> ~/atlisp），直接打开仓库里的 dwg。
用法:
  python3 test_run.py [图纸.dwg] [--layouts 0,1,2,3,4,5,7,8] [--rep 3]
                     [--write 8] [--out test-L8.dwg] [--skip-l6]
"""
import argparse
import os
import sys

import mcp_client

HERE = os.path.dirname(os.path.abspath(__file__))

# 识别 + 匹配 + 障碍物收集（与 dim-leader:run 的前半段一致），并留下计时工具
PREP = r"""
(progn
  (defun c:ms () (getvar "MILLISECS"))
  (defun c:dt (a b) (if (< b a) (- (+ b a) 4294967296.0) (- b a)))
  (defun c:bench (body rep / best n i t0 t1 d r)
    (setq best -1.0 n -1 i 0)
    (while (< i rep)
      (setq t0 (c:ms))
      (setq r (vl-catch-all-apply (function (lambda (a b) (eval body))) (list *bm* *bo*)))
      (setq t1 (c:ms) d (c:dt t0 t1))
      (if (and (not (vl-catch-all-error-p r)) (or (< best 0.0) (< d best)))
        (setq best d n (if (listp r) (length r) -1)))
      (setq i (1+ i)))
    (list best n))
  (setq *bm* (dim-leader:match-texts (dim-leader:find-centers))
        *bo* nil
        *me* (mapcar (function (lambda (x) (nth 1 x))) *bm*))
  (if (setq *ss* (ssget "_X" '((0 . "TEXT"))))
    (progn
      (setq *i* 0)
      (while (< *i* (sslength *ss*))
        (setq *e* (ssname *ss* *i*))
        (if (not (member *e* *me*))
          (progn
            (setq *bx* (dim-leader:text-box *e*)
                  *ox* (/ (+ (nth 0 *bx*) (nth 2 *bx*)) 2.0)
                  *oy* (/ (+ (nth 1 *bx*) (nth 3 *bx*)) 2.0))
            (setq *bo* (cons (dim-leader:mk-placed (nth 0 *bx*) (nth 1 *bx*)
                                                 (nth 2 *bx*) (nth 3 *bx*)
                                                 *ox* *oy* *ox* *oy* *ox* *oy* *ox* *oy*)
                             *bo*))))
        (setq *i* (1+ *i*)))))
  (princ (strcat "centers=" (itoa (length (dim-leader:find-centers)))
                 " matched=" (itoa (length *bm*))
                 " obstacles=" (itoa (length *bo*)))))
"""

BODIES = {
    0: "(dim-leader:layout a b)",
    1: "(dim-leader:layout-table a b)",
    2: "(dim-leader:layout-table-zone a b (@:get-config 'dim-leader:zones))",
    3: "(dim-leader:layout-zone2d a b 3)",
    4: "(dim-leader:layout-4dir a b)",
    5: "(dim-leader:layout-lanes a b)",
    6: "(dim-leader:layout-chip a b)",
    7: "(dim-leader:layout-compact a b)",
    8: "(dim-leader:layout-channels a b)",
}


def open_drawing(name):
    """打开图纸；若 CAD 已经开着它就直接复用（重复 open_dwg 会报"不是有效的图形文件"）。"""
    p = mcp_client.host_path(name)
    want = os.path.basename(name).lower()
    cur = ""
    try:
        cur = mcp_client.eval_lisp('(getvar "DWGNAME")', timeout=60).strip().lower()
    except Exception:
        pass
    if cur == want:
        print("open: 已在当前文档", name)
    else:
        print("open:", mcp_client.call("open_dwg", {"filePath": p},
                                       timeout=300)[:80])
    mcp_client.ensure_package(HERE, force=True)
    print("pkg reloaded | dwg =",
          mcp_client.eval_lisp('(getvar "DWGNAME")', timeout=120).strip())
    return p


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("dwg", nargs="?", default="test.dwg")
    ap.add_argument("--layouts", default="0,1,2,3,4,5,7,8")
    ap.add_argument("--rep", type=int, default=3)
    ap.add_argument("--blocks", type=int, default=10)
    ap.add_argument("--write", type=int, default=None,
                    help="真正写图的布局编号（用 dim-leader:run-layout）")
    ap.add_argument("--out", default=None, help="--write 时的另存文件名")
    a = ap.parse_args()

    open_drawing(a.dwg)
    mcp_client.eval_lisp(f"(@:set-config 'dim-leader:channels-blocks {a.blocks})",
                         timeout=200)
    print("prep:", mcp_client.eval_lisp(PREP, timeout=1200).strip(), flush=True)

    for lid in [int(x) for x in a.layouts.split(",") if x]:
        rep = 1 if lid == 0 else a.rep
        body = BODIES[lid]
        code = f"(c:bench (quote {body}) {rep})"
        try:
            ms, n = mcp_client.eval_lisp(code, timeout=1800).strip()[1:-1].split()
            print(f"  L{lid}: {float(ms):9.0f} ms   n={n}  (rep={rep})", flush=True)
        except Exception as e:
            print(f"  L{lid}: ERR {str(e)[:90]}", flush=True)

    if a.write is not None:
        print(f"\nwrite layout {a.write} ->", flush=True)
        r = mcp_client.eval_lisp(f"(dim-leader:run-layout {a.write})", timeout=1800)
        print("  run:", r.strip()[:200], flush=True)
        n_pl = mcp_client.eval_lisp(
            '(if (ssget "_X" (quote ((0 . "LWPOLYLINE")))) '
            '(itoa (sslength (ssget "_X" (quote ((0 . "LWPOLYLINE")))))) "0")',
            timeout=300).strip()
        print("  LWPOLYLINE in drawing:", n_pl, flush=True)
        if a.out:
            out = mcp_client.host_path(a.out)
            print("  save:", mcp_client.call("save_dwg", {"filePath": out},
                                             timeout=600)[:100])
            print("  ->", out)


if __name__ == "__main__":
    main()
