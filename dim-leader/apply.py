"""步骤 3: 把 Python 布局结果写回 DWG

- 移动/对齐/旋转文本 (10/11/50/72/73)
- 按 route 点表创建 LWPOLYLINE 引线
用法:
  python3 apply.py layout.json [--layer LAYER] [--cleanup] [--save NAME.dwg]
    --layer   引线所在图层(缺省用配置 dim-leader:layer, 仍为空则 CLAYER)
    --cleanup 先删除 --layer 上旧的 LWPOLYLINE(重复实验用, 必须与 --layer 同用)
    --save    另存为指定 dwg(缺省不保存)
"""
import argparse
import json
import os
import sys

import mcp_client

HERE = os.path.dirname(os.path.abspath(__file__))


def fmt_pt(p):
    return f"({p[0]:.6f} {p[1]:.6f})"


def build_lisp(items, layer, cleanup):
    rows = []
    for it in items:
        route = " ".join(fmt_pt(p) for p in it["route"])
        rows.append(
            f"(\"{it['handle']}\" {it['ax']:.6f} {it['ay']:.6f} "
            f"{it['rot']} {it['hj']} {it['vj']} ({route}))")
    lst = " ".join(rows)
    layer_lit = json.dumps(layer) if layer else "nil"
    cleanup_code = ""
    if cleanup:
        cleanup_code = f"""
  (progn
    (setq *ss* (ssget "_X" (list (cons 0 "LWPOLYLINE") (cons 8 "{layer}"))) *i* 0)
    (while *ss*
      (command "_.erase" *ss* "")
      (setq *ss* (ssget "_X" (list (cons 0 "LWPOLYLINE") (cons 8 "{layer}"))))))
"""
    return f"""
(progn
  (setq *laid* '({lst})){cleanup_code}
  (progn
    (setq *ss* (ssget "_X" '((0 . "TEXT"))) *i* 0 *hmap* nil)
    (while (< *i* (sslength *ss*))
      (setq *hmap* (cons (cons (cdr (assoc 5 (entget (ssname *ss* *i*))))
                               (ssname *ss* *i*)) *hmap*))
      (setq *i* (1+ *i*))))
  (setq *ok* 0 *miss* 0)
  (foreach *it* *laid*
    (setq *en* (cdr (assoc (car *it*) *hmap*)))
    (if *en*
      (progn
        (dim-leader:place-text-j *en* (nth 1 *it*) (nth 2 *it*)
                                 (nth 3 *it*) (nth 4 *it*) (nth 5 *it*))
        (dim-leader:make-polyline (nth 6 *it*) {layer_lit} nil)
        (setq *ok* (1+ *ok*)))
      (setq *miss* (1+ *miss*))))
  (strcat "ok=" (itoa *ok*) " miss=" (itoa *miss*)
         " total=" (itoa (length *laid*))))
"""


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("layout", help="布局 JSON (channels_layout.py 的输出)")
    ap.add_argument("--layer", default=None, help="引线图层")
    ap.add_argument("--cleanup", action="store_true", help="先删旧引线")
    ap.add_argument("--save", default=None, help="另存为路径")
    a = ap.parse_args()

    if a.cleanup and not a.layer:
        sys.exit("--cleanup 必须与 --layer 同用")

    layer = a.layer
    if layer is None:
        layer = mcp_client.eval_lisp("(@:get-config 'dim-leader:layer')").strip()
        if layer in ('""', "''", "nil"):
            layer = None

    items = json.load(open(a.layout, encoding="utf-8"))
    mcp_client.ensure_package(HERE)
    code = build_lisp(items, layer, a.cleanup)
    report = mcp_client.eval_lisp(code, timeout=600)
    print(f"write-back: {report}")

    if a.save:
        r = mcp_client.call("save_dwg", {"filePath": a.save})
        print(f"saved: {r}")


if __name__ == "__main__":
    main()
