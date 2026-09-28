"""步骤 1: 从 DWG 提取数据 → data.json

提取内容: 共圆心双半弧的圆心、圆心旁匹配的单行文本(handle/圆心/插入点/字宽/字高/内容)。
用法: python3 extract.py [data.json]
"""
import json
import os
import sys

import mcp_client

HERE = os.path.dirname(os.path.abspath(__file__))

EXTRACT_LISP = r"""
(progn
  (defun c:dl-jesc (s)
    (if (null s) "null"
      (progn
        (setq s (vl-string-subst "\\" "\\\\" s))
        (vl-string-subst "\"" "\\\"" s))))
  (defun c:dl-rtos (x / s)
    (setq s (rtos (if (numberp x) x 0.0) 2 6))
    (if (= s "-0") "0" s))
  (setq *c* (dim-leader:find-centers)
        *m* (dim-leader:match-texts *c*))
  (progn
    (setq items nil)
    (foreach m *m*
      (setq ctr (car m) ent (cadr m))
      (setq dat (entget ent))
      (setq items (cons
        (strcat ",\n    {\"handle\": " (vl-prin1-to-string (cdr (assoc 5 dat)))
                ", \"ctr\": [" (c:dl-rtos (car ctr)) ", " (c:dl-rtos (cadr ctr)) "]"
                ", \"pt\": [" (c:dl-rtos (car (cdr (assoc 10 dat)))) ", " (c:dl-rtos (cadr (cdr (assoc 10 dat)))) "]"
                ", \"w\": " (c:dl-rtos (if (and (setq tb (textbox dat))
                                          (> (car (cadr tb)) 0.0))
                                  (- (car (cadr tb)) (caar tb))
                                  (dim-leader:text-fallback-width dat)))
                ", \"height\": " (c:dl-rtos (cdr (assoc 40 dat)))
                ", \"txt\": \"" (c:dl-jesc (cdr (assoc 1 dat))) "\"}")
        items)))
    (strcat "{\"centers\": " (itoa (length *c*))
            ", \"matched_n\": " (itoa (length *m*))
            ", \"config\": {\"gap\": " (c:dl-rtos (@:get-config 'dim-leader:gap))
            ", \"lane_gap\": " (c:dl-rtos (@:get-config 'dim-leader:lane-gap))
            ", \"columns\": " (itoa (if (numberp (@:get-config 'dim-leader:channels-columns)) (@:get-config 'dim-leader:channels-columns) 1))
            "}, \"matched\": [\n"
            (substr (apply 'strcat (reverse items)) 3)
            "\n]}")))
"""


def main():
    out = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "data.json")
    mcp_client.ensure_package(HERE)
    raw = mcp_client.eval_lisp(EXTRACT_LISP, timeout=600)
    data = json.loads(raw)
    json.dump(data, open(out, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
    print(f"centers={data['centers']} matched={data['matched_n']} -> {out}")


if __name__ == "__main__":
    main()
