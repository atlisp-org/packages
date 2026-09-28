import json, sys
sys.path.insert(0, '/home/vitalgg/atlisp/packages/dim-leader')
import mcp_client

def check(s):
    d = 0
    for i, c in enumerate(s):
        if c == '(':
            d += 1
        elif c == ')':
            d -= 1
            if d < 0:
                raise SystemExit(f'unbalanced at {i}: {s[max(0,i-30):i+10]}')
    if d:
        raise SystemExit(f'not closed, depth={d}, tail: {s[-60:]}')

laid = json.load(open('/tmp/opencode/layout.json'))
rows = ['("%s" %.6f %.6f %d %d %d)' % (it['handle'], it['ax'], it['ay'], it['rot'], it['hj'], it['vj']) for it in laid]
lst = ' '.join(rows)

code = f'''
(progn
  (setq *items* '({lst}))
  (setq *ss* (ssget "_X" '((0 . "TEXT"))) *i* 0 *hmap* nil)
  (while (< *i* (sslength *ss*))
    (setq *d* (entget (ssname *ss* *i*)))
    (setq *hmap* (cons (cons (cdr (assoc 5 *d*))
      (list (cdr (assoc 11 *d*)) (cdr (assoc 50 *d*))
            (cdr (assoc 72 *d*)) (cdr (assoc 73 *d*)))) *hmap*))
    (setq *i* (1+ *i*)))
  (setq *bad* 0 *msgs* nil)
  (foreach *it* *items*
    (setq *g* (cdr (assoc (car *it*) *hmap*)))
    (if (or (null *g*)
            (> (abs (- (car (nth 0 *g*)) (nth 1 *it*))) 0.01)
            (> (abs (- (cadr (nth 0 *g*)) (nth 2 *it*))) 0.01)
            (> (abs (- (nth 1 *g*) (/ (* (nth 3 *it*) 3.141592653589793) 180.0))) 1e-6)
            (/= (nth 2 *g*) (nth 4 *it*))
            (/= (nth 3 *g*) (nth 5 *it*)))
      (progn
        (setq *bad* (1+ *bad*))
        (if (< (length *msgs*) 5) (setq *msgs* (cons (car *it*) *msgs*))))))
  (list *bad* *msgs*))
'''
check(code)
print('balanced ok, sending...')
print(mcp_client.eval_lisp(code, timeout=300))
print('polylines:', mcp_client.eval_lisp('(sslength (ssget "_X" (list (cons 0 "LWPOLYLINE") (cons 8 "DL_PY"))))'))
