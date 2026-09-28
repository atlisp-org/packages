"""从 CAD 场景抽取布局基准数据 → data.json（一次抽取，A/B 两边共用）。

抽取内容与 dim-leader:run 的输入完全一致：
  matched   match-texts 的输出，按 CAD 原始顺序，每条 7 个数：
            圆心x 圆心y 参考点x 参考点y 文本宽 文本高 方位
  obstacles run 中"未匹配文本"的盒 (bl br bb bt)，按 CAD 原始顺序
  config    参与布局的全部 @:define-config 取值

文本宽 w 取 CAD 的 (textbox) 结果，避免把字体度量差异算进布局耗时。
数值用空格分隔的扁平串回传（不在 LISP 里拼 JSON），配置用 vl-prin1-to-string 逐项回传。
前置：先用 bench_scene.py 在 CAD 内重建 507 点场景。
用法: python3 bench_data.py [out.json]
"""
import json
import os
import sys
import time

import mcp_client

HERE = os.path.dirname(os.path.abspath(__file__))

CONFIGS = ["layer", "offset", "match-tol", "gap", "columns", "avoid-leader",
           "layout", "zones", "rotate", "leader-width", "side-distance",
           "leader-colors", "lane-gap", "channels-lines", "channels-columns",
           "channels-blocks"]

PREP = """
(progn
  (defun c:nums (l / r)
    (setq r nil)
    (foreach x l (setq r (cons (strcat (rtos x 2 9) " ") r)))
    (apply 'strcat (reverse r)))
  (setq *bm* (dim-leader:match-texts (dim-leader:find-centers))
        *bmatch* (mapcar (function (lambda (x) (nth 1 x))) *bm*)
        *bo* nil)
  (if (setq *ss* (ssget "_X" '((0 . "TEXT"))))
    (progn
      (setq *i* 0)
      (while (< *i* (sslength *ss*))
        (setq *e* (ssname *ss* *i*))
        (if (not (member *e* *bmatch*))
          (progn
            (setq *bx* (dim-leader:text-box *e*)
                  *ox* (/ (+ (nth 0 *bx*) (nth 2 *bx*)) 2.0)
                  *oy* (/ (+ (nth 1 *bx*) (nth 3 *bx*)) 2.0))
            (setq *bo* (cons (dim-leader:mk-placed (nth 0 *bx*) (nth 1 *bx*)
                                                 (nth 2 *bx*) (nth 3 *bx*)
                                                 *ox* *oy* *ox* *oy* *ox* *oy* *ox* *oy*)
                             *bo*))))
        (setq *i* (1+ *i*))))
  (princ (strcat "prepared " (itoa (length *bm*)) " matched, "
                        (itoa (length *bo*)) " obstacles"))))
"""

MATCHED_CHUNK = """
(progn
  (setq *out* nil *i* %d *n* (min (length *bm*) (+ %d %d)))
  (while (< *i* *n*)
    (setq *m* (nth *i* *bm*)
          *d* (entget (nth 1 *m*))
          *rf* (cdr (assoc 11 *d*)))
    (if (null *rf*) (setq *rf* (cdr (assoc 10 *d*))))
    (setq *tb* (textbox *d*)
          *w* (if *tb* (abs (- (car (cadr *tb*)) (caar *tb*)))
                 (dim-leader:text-fallback-width *d*)))
    (if (< *w* 0.0) (setq *w* (- 0.0 *w*)))
    (setq *out* (cons (c:nums (list (car (nth 0 *m*)) (cadr (nth 0 *m*))
                                    (car *rf*) (cadr *rf*) *w*
                                    (cdr (assoc 40 *d*)) (nth 2 *m*)))
                      *out*))
    (setq *i* (1+ *i*)))
  (apply 'strcat (reverse *out*)))
"""

OBST_CHUNK = """
(progn
  (setq *out* nil *i* %d *n* (min (length *bo*) (+ %d %d)))
  (while (< *i* *n*)
    (setq *o* (nth *i* *bo*))
    (setq *out* (cons (c:nums (list (nth 0 *o*) (nth 1 *o*)
                                    (nth 2 *o*) (nth 3 *o*)))
                      *out*))
    (setq *i* (1+ *i*)))
  (apply 'strcat (reverse *out*)))
"""


def split_lisp_list(s):
    """把 "(a \"b c\" nil 2.0)" 拆成 ['a', '"b c"', 'nil', '2.0']。"""
    toks, i, n = [], 0, len(s)
    while i < n:
        c = s[i]
        if c in "() \t\r\n,":
            i += 1
        elif c == '"':
            j, buf = i + 1, []
            while j < n and s[j] != '"':
                if s[j] == "\\":
                    buf.append(s[j:j + 2])
                    j += 2
                    continue
                buf.append(s[j])
                j += 1
            toks.append('"' + "".join(buf) + '"')
            i = j + 1
        else:
            j = i
            while j < n and s[j] not in "() \t\r\n,":
                j += 1
            toks.append(s[i:j])
            i = j
    return toks


def parse_lisp_value(t):
    t = t.strip()
    if t in ("nil", '""', ""):
        return None
    if t == "T":
        return True
    try:
        return json.loads(t)
    except ValueError:
        return t


def chunks(total, size):
    for i in range(0, total, size):
        yield i, min(total, i + size)


def require_scene(expected=507, tries=6):
    """CAD 是共用的，活动文档可能被别人切走；先打开并确认基准场景。"""
    dwg = mcp_client.open_scene(arcs=expected * 2, tries=tries, pkg_dir=HERE)
    n_text = int(mcp_client.eval_lisp(
        "(if (ssget \"_X\" '((0 . \"TEXT\"))) "
        "(itoa (sslength (ssget \"_X\" '((0 . \"TEXT\"))))) \"0\")",
        timeout=120).strip())
    print(f"scene ok: {dwg} arcs={expected * 2} texts={n_text}")
    return dwg


def main():
    out_path = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "data.json")
    size = int(os.environ.get("BENCH_CHUNK", "150"))
    require_scene()

    prep = mcp_client.eval_lisp(PREP, timeout=1200).strip()
    n_m = int(mcp_client.eval_lisp("(itoa (length *bm*))", timeout=300).strip())
    n_o = int(mcp_client.eval_lisp("(itoa (length *bo*))", timeout=300).strip())
    dwg = mcp_client.eval_lisp('(getvar "DWGNAME")', timeout=60).strip()
    print(f"{prep} | dwg={dwg}")

    matched = []
    for a, b in chunks(int(n_m), size):
        nums = mcp_client.eval_lisp(
            MATCHED_CHUNK % (a, a, size), timeout=600).strip().split()
        assert len(nums) == (b - a) * 7, (len(nums), (b - a) * 7)
        matched += [[float(x) for x in nums[i:i + 7]] for i in range(0, len(nums), 7)]

    obstacles = []
    for a, b in chunks(int(n_o), size):
        nums = mcp_client.eval_lisp(
            OBST_CHUNK % (a, a, size), timeout=600).strip().split()
        assert len(nums) == (b - a) * 4
        obstacles += [[float(x) for x in nums[i:i + 4]] for i in range(0, len(nums), 4)]

    cfg_src = "(list " + " ".join(
        f"(vl-prin1-to-string (@:get-config 'dim-leader:{k}))" for k in CONFIGS) + ")"
    vals = mcp_client.eval_lisp(cfg_src, timeout=300).strip()[1:-1]
    parts = split_lisp_list(vals)
    assert len(parts) == len(CONFIGS), f"{len(parts)} != {len(CONFIGS)}"
    config = {k: parse_lisp_value(v) for k, v in zip(CONFIGS, parts)}

    data = {"dwg": dwg, "n_matched": len(matched), "n_obstacles": len(obstacles),
            "config": config, "matched": matched, "obstacles": obstacles}
    json.dump(data, open(out_path, "w", encoding="utf-8"),
              ensure_ascii=False, indent=1)
    print(f"-> {out_path}")
    print("config:", config)
    ws = [m[4] for m in matched]
    print(f"text w: min={min(ws):.2f} max={max(ws):.2f} "
          f"avg={sum(ws)/len(ws):.2f}  h={matched[0][5]}")


if __name__ == "__main__":
    main()
