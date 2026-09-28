"""从 CAD 分块导出 dim-leader 的运行结果（文本世界盒 + 引线折点）→ result_dump.json。

用 CAD 自己的 (dim-leader:text-box)/textbox 度量，避免本地按字符数估算字宽。
分块回传（每块 40 个），绕开 MCP 单次响应长度限制。

前置：已在 CAD 里跑过 (dim-leader:run) 或 (dim-leader:run-layout N)。
用法: python3 dump_result.py [--chunk 40]
"""
import argparse
import json
import os
import re
import sys

import mcp_client

HERE = os.path.dirname(os.path.abspath(__file__))

SETUP = """
(progn
  (defun c:n (l / r) (setq r nil)
    (foreach x l (setq r (cons (strcat (rtos x 2 6) " ") r)))
    (apply 'strcat (reverse r)))
  ;; 优先用预先抓好的句柄表 *mh*（跑过 run 之后再算 match-texts 会得 0，
  ;; 因为文字已被移走）；没有 *mh* 时现场重算。用句柄而非 ename 判定，
  ;; ename 在别的 db 调用后会失效导致 member 失配。
  (setq *me* (if *mh*
                *mh*
                (progn
                  (setq *bm* (dim-leader:match-texts (dim-leader:find-centers)))
                  (mapcar (function (lambda (x)
                              (cdr (assoc 5 (entget (nth 1 x))))))
                          *bm*)))
        *tlist* nil *rlist* nil
        *ss* (ssget "_X" '((0 . "TEXT"))) *i* 0)
  (while (< *i* (sslength *ss*))
    (setq *e* (ssname *ss* *i*)
          *flag* (if (member (cdr (assoc 5 (entget *e*))) *me*) "1" "0"))
    (setq *e* (ssname *ss* *i*) *b* (dim-leader:text-box *e*))
    (setq *tlist* (cons (strcat (c:n (list (nth 0 *b*) (nth 1 *b*) (nth 2 *b*) (nth 3 *b*)))
                                 *flag*)
                        *tlist*))
    (setq *i* (1+ *i*)))
  (setq *ss* (ssget "_X" '((0 . "LWPOLYLINE"))) *i* 0)
  (while (< *i* (sslength *ss*))
    (setq *e* (ssname *ss* *i*) *p* (entget *e*) *pts* nil)
    (foreach g *p* (if (= (car g) 10) (setq *pts* (cons (c:n (cdr g)) *pts*))))
    (setq *rlist* (cons (strcat (apply 'strcat (reverse *pts*)) "|") *rlist*))
    (setq *i* (1+ *i*)))
  (setq *tlist* (reverse *tlist*) *rlist* (reverse *rlist*))
  (princ (strcat (itoa (length *tlist*)) " " (itoa (length *rlist*)))))
"""

TEXT_CHUNK = """
(progn
  (setq *o* nil *i* %d *n* (min (length *tlist*) (+ %d %d)))
  (while (< *i* *n*) (setq *o* (cons (strcat (nth *i* *tlist*) " ") *o*)) (setq *i* (1+ *i*)))
  (apply 'strcat (reverse *o*)))
"""

ROUTE_CHUNK = """
(progn
  (setq *o* nil *i* %d *n* (min (length *rlist*) (+ %d %d)))
  (while (< *i* *n*) (setq *o* (cons (nth *i* *rlist*) *o*)) (setq *i* (1+ *i*)))
  (apply 'strcat (reverse *o*)))
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--chunk", type=int, default=40)
    ap.add_argument("--out", default=os.path.join(HERE, "result_dump.json"))
    a = ap.parse_args()

    mcp_client.ensure_package(HERE)
    n_t, n_r = mcp_client.eval_lisp(SETUP, timeout=900).strip().split()
    print(f"text={n_t} leader={n_r}")

    boxes = []
    for i in range(0, int(n_t), a.chunk):
        s = mcp_client.eval_lisp(TEXT_CHUNK % (i, i, a.chunk), timeout=600)
        t = s.split()
        for k in range(0, len(t) - 4, 5):
            rec = t[k:k + 5]
            if rec[4] in ("0", "1"):
                boxes.append([float(x) for x in rec[:4]] + [int(rec[4])])

    routes = []
    for i in range(0, int(n_r), a.chunk):
        s = mcp_client.eval_lisp(ROUTE_CHUNK % (i, i, a.chunk), timeout=600)
        for part in s.strip().split("|"):
            nums = [float(x) for x in re.findall(r"-?\d+\.?\d*", part)]
            if len(nums) >= 4 and len(nums) % 2 == 0:
                routes.append([(nums[k], nums[k + 1]) for k in range(0, len(nums), 2)])

    print(f"parsed boxes={len(boxes)} routes={len(routes)}")
    assert len(boxes) == int(n_t), f"boxes {len(boxes)} != {n_t}"
    assert len(routes) == int(n_r), f"routes {len(routes)} != {n_r}"
    json.dump({"boxes": boxes, "routes": routes}, open(a.out, "w"))
    print("->", a.out)


if __name__ == "__main__":
    main()
