"""dim-leader 9 种布局的 Python 实现（与 dim-leader.lsp/chip.lsp/channels.lsp 逐句对应）。

目的：在同一份输入上与 LISP 版对比执行效率，故这里刻意照抄算法，不做任何优化，
只保证语义一致（vl-sort 丢重复、fix 向零截断、rem 符号、cons/reverse 顺序等）。

输入 data.json（由 bench_data.py 从 CAD 抽取）：
  matched   [[cx, cy, refx, refy, w, h, s], ...]  顺序与 CAD 侧一致
  obstacles [[bl, br, bb, bt], ...]              顺序与 CAD 侧一致
  config    布局用到的 @:define-config 取值

各 layout* 返回与 LISP 同构的记录表（8 元素；布局 0 返回 15 元素）。

用法:
  python3 layouts_py.py data.json            # 跑全部 9 种并输出耗时
  python3 layouts_py.py data.json --only 8   # 只跑布局 8
  python3 layouts_py.py data.json --out l8.json
"""
import argparse
import functools
import json
import math
import time

# ---------------------------------------------------------------------------
# AutoLISP 语义
# ---------------------------------------------------------------------------


def fix(x):
    """(fix x)：向零截断。"""
    return int(x)


def lrem(a, b):
    """(rem a b)：与 AutoLISP 同余（结果符号跟 a）。"""
    return a - b * fix(a / b)


def vl_sort(lst, less):
    """复刻 AutoLISP vl-sort：按 less 稳定升序，**不去重**。

    实测本 CAD 的 vl-sort 对并列键既不丢元素也保序：
      (vl-sort '((1 5) (1 9) (1 7)) #'(< (lambda (a b) (< (car a) (car b)))))
      => ((1 5) (1 9) (1 7))    共 3 个，顺序不变
    channels.lsp 里 ch-sort-order / ch-left-edge 用"原始序号打破平局"只是让
    比较器成为全序（防御性写法），这里照做：比较器不等价时按原始下标定胜负，
    结果与"稳定排序+保序"一致。
    """
    if len(lst) < 2:
        return list(lst)

    def cmp(a, b):
        ia, ib = a[1], b[1]
        va, vb = lst[ia], lst[ib]
        if less(va, vb):
            return -1
        if less(vb, va):
            return 1
        return 0 if ia == ib else (-1 if ia < ib else 1)

    return [t[0] for t in sorted([(v, i) for i, v in enumerate(lst)],
                                 key=functools.cmp_to_key(
                                     lambda p, q: cmp(p, q)))]


def setnth(lst, idx, val):
    return [val if i == idx else v for i, v in enumerate(lst)]


def dedupe_pts(pts, eps=1.0e-6):
    res = []
    for p in pts:
        if not res or math.hypot(res[-1][0] - p[0], res[-1][1] - p[1]) > eps:
            res.append(p)
    return res


# ---------------------------------------------------------------------------
# 几何工具（dim-leader.lsp :52-147）
# ---------------------------------------------------------------------------


def box_hit_p(placed, bl, br, bb, bt):
    for p in placed:
        if p[1] > bl and p[0] < br and p[3] > bb and p[2] < bt:
            return True
    return False


def pt_in_rect(x, y, bl, br, bb, bt):
    return bl <= x <= br and bb <= y <= bt


def seg_seg_hit(ax, ay, bx, by, cx, cy, dx, dy):
    d1 = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    d2 = (bx - ax) * (dy - ay) - (by - ay) * (dx - ax)
    d3 = (dx - cx) * (ay - cy) - (dy - cy) * (ax - cx)
    d4 = (dx - cx) * (by - cy) - (dy - cy) * (bx - cx)
    return (((d1 > 0.0) != (d2 > 0.0)) or ((d1 < 0.0) != (d2 < 0.0))) and \
           (((d3 > 0.0) != (d4 > 0.0)) or ((d3 < 0.0) != (d4 < 0.0)))


def seg_rect_hit(x1, y1, x2, y2, bl, br, bb, bt):
    return (pt_in_rect(x1, y1, bl, br, bb, bt)
            or pt_in_rect(x2, y2, bl, br, bb, bt)
            or seg_seg_hit(x1, y1, x2, y2, bl, bb, br, bb)
            or seg_seg_hit(x1, y1, x2, y2, br, bb, br, bt)
            or seg_seg_hit(x1, y1, x2, y2, br, bt, bl, bt)
            or seg_seg_hit(x1, y1, x2, y2, bl, bt, bl, bb))


def mk_placed(bl, br, bb, bt, s1x1, s1y1, s1x2, s1y2, s2x1, s2y1, s2x2, s2y2):
    xs = (bl, s1x1, s1x2, s2x1, s2x2)
    return [bl, br, bb, bt, s1x1, s1y1, s1x2, s1y2, s2x1, s2y1, s2x2, s2y2,
            min(xs), max(xs)]


def eval_place(placed, bl, br, bb, bt,
               s1x1, s1y1, s1x2, s1y2, s2x1, s2y1, s2x2, s2y2):
    minx = min(bl, s1x1, s1x2, s2x1, s2x2)
    maxx = max(br, s1x1, s1x2, s2x1, s2x2)
    hit, cross = False, 0
    for p in placed:
        if p[13] > minx and p[12] < maxx:
            if p[1] > bl and p[0] < br and p[3] > bb and p[2] < bt:
                hit = True
            if (seg_rect_hit(s1x1, s1y1, s1x2, s1y2, p[0], p[1], p[2], p[3])
                    or seg_rect_hit(s2x1, s2y1, s2x2, s2y2, p[0], p[1], p[2], p[3])
                    or seg_rect_hit(p[4], p[5], p[6], p[7], bl, br, bb, bt)
                    or seg_rect_hit(p[8], p[9], p[10], p[11], bl, br, bb, bt)):
                cross += 1
    return hit, cross


def seg_cross_boxes(boxes, px, py, qx, qy):
    c, minx, maxx = 0, min(px, qx), max(px, qx)
    for b in boxes:
        if b[2] > minx and b[0] < maxx and seg_rect_hit(px, py, qx, qy, *b):
            c += 1
    return c


def route_cross(boxes, pts):
    return sum(seg_cross_boxes(boxes, pts[i][0], pts[i][1],
                              pts[i + 1][0], pts[i + 1][1])
               for i in range(len(pts) - 1))


# dim-leader.lsp :149-265 芯片式布线工具
def orient(ax, ay, bx, by, cx, cy):
    d = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    if d < -1.0e-9:
        return -1
    return 1 if d > 1.0e-9 else 0


def seg_seg_hit2(ax, ay, bx, by, cx, cy, dx, dy):
    o1, o2 = orient(ax, ay, bx, by, cx, cy), orient(ax, ay, bx, by, dx, dy)
    o3, o4 = orient(cx, cy, dx, dy, ax, ay), orient(cx, cy, dx, dy, bx, by)
    return (o1 * o2 <= 0 and o3 * o4 <= 0
            and min(ax, bx) <= max(cx, dx) + 1.0e-9
            and min(cx, dx) <= max(ax, bx) + 1.0e-9
            and min(ay, by) <= max(cy, dy) + 1.0e-9
            and min(cy, dy) <= max(ay, by) + 1.0e-9)


def seg_box_hit2(x1, y1, x2, y2, bl, br, bb, bt):
    return (pt_in_rect(x1, y1, bl, br, bb, bt)
            or pt_in_rect(x2, y2, bl, br, bb, bt)
            or seg_seg_hit2(x1, y1, x2, y2, bl, bb, br, bb)
            or seg_seg_hit2(x1, y1, x2, y2, br, bb, br, bt)
            or seg_seg_hit2(x1, y1, x2, y2, br, bt, bl, bt)
            or seg_seg_hit2(x1, y1, x2, y2, bl, bt, bl, bb))


def route_hit_box(route, box):
    for i in range(len(route) - 1):
        a, b = route[i], route[i + 1]
        if seg_box_hit2(a[0], a[1], b[0], b[1], *box):
            return True
    return False


def route_hit_boxes(route, boxes):
    return any(route_hit_box(route, b) for b in boxes)


def route_hit_route(a, b):
    for i in range(len(a) - 1):
        s1, s2 = a[i], a[i + 1]
        for j in range(len(b) - 1):
            sa1, sa2 = b[j], b[j + 1]
            if seg_seg_hit2(s1[0], s1[1], s2[0], s2[1],
                            sa1[0], sa1[1], sa2[0], sa2[1]):
                return True
    return False


def route_keepouts(route, clearance):
    boxes = []
    for i in range(len(route) - 1):
        (x1, y1), (x2, y2) = route[i], route[i + 1]
        if x1 == x2:
            boxes.append([x1 - clearance, x1 + clearance,
                          min(y1, y2) - clearance, max(y1, y2) + clearance])
        else:
            boxes.append([min(x1, x2) - clearance, max(x1, x2) + clearance,
                          y1 - clearance, y1 + clearance])
    return boxes


def route_safe_p(route, boxes, keepouts):
    return (not route_hit_boxes(route, boxes)
            and not route_hit_boxes(route, keepouts))


def route_length(route):
    return sum(math.hypot(route[i + 1][0] - route[i][0],
                          route[i + 1][1] - route[i][1])
               for i in range(len(route) - 1))


def route_bends(route):
    n = 0
    for i in range(1, len(route) - 1):
        a, b = route[i - 1], route[i + 1]
        if a[0] != b[0] and a[1] != b[1]:
            n += 1
    return n


def unique_nums(vals, eps=1.0e-7):
    res = []
    for v in vl_sort([v for v in vals if isinstance(v, (int, float))],
                     lambda a, b: a < b):
        if not res or abs(v - res[-1]) > eps:
            res.append(v)
    return res


def expand_nums(vals, gap):
    out = []
    for v in vals:
        out += [v - gap, v, v + gap]
    return unique_nums(out)


def thin_nums(vals, limit):
    vals = unique_nums(vals)
    n = len(vals)
    if n <= limit:
        return vals
    return [vals[fix(0.5 + i * (1 - n) / (1 - limit))] for i in range(limit)]


# ---------------------------------------------------------------------------
# 输入构造：对应 LISP 里各布局开头重复的 entget 循环
# ---------------------------------------------------------------------------


class Inp:
    """把 data.json 转成各布局要的形态，顺序与 LISP 的 cons/reverse 完全一致。"""

    def __init__(self, data):
        self.matched = data["matched"]      # [cx, cy, refx, refy, w, h, s]
        self.cfg = data["config"]
        self.obstacles = data["obstacles"]  # [bl, br, bb, bt]
        self.n = len(self.matched)

    def items_ref(self):
        """布局 0-5：cons 出 (ctr ent refx refy w h)，ent 位置放 None 占位。

        不 reverse（随后 vl-sort）；保持与 LISP 相同的下标，便于逐行对照。
        """
        return [[(m[0], m[1]), None, m[2], m[3], m[4], m[5]]
                for m in self.matched][::-1]

    def items_cwh(self):
        """布局 6-8：cons 出 (ctr ent w h) 后 reverse，ent 位置放 None 占位。"""
        return [[(m[0], m[1]), None, m[4], m[5]]
                for m in self.matched][::-1]

    def obstacles_placed(self):
        """run 里的 obstacles 是 mk-placed（两条退化为点的引线段）。"""
        out = []
        for o in self.obstacles:
            ox = (o[0] + o[2]) / 2.0
            oy = (o[1] + o[3]) / 2.0
            out.append(mk_placed(o[0], o[1], o[2], o[3],
                                 ox, oy, ox, oy, ox, oy, ox, oy))
        return out

    def c(self, key, default):
        v = self.cfg.get(key, default)
        return default if v is None else v


def _dims(items):
    """items 为 (ctr ... w h)，返回 (maxw, maxh)。"""
    maxw = maxh = 0.0
    for m in items:
        if m[-2] > maxw:
            maxw = m[-2]
        if m[-1] > maxh:
            maxh = m[-1]
    return maxw, maxh


# ---------------------------------------------------------------------------
# 布局 0：本地错开（dim-leader.lsp :608-739）
# 返回 8 元素 (ctr ax ay rot 0 0 route)
# ---------------------------------------------------------------------------


def layout0(inp, state=None):
    gap = inp.c("gap", 1.0)
    rotallow = inp.c("rotate", 1) == 1
    avoid = inp.c("avoid-leader", 0) == 1
    cols = int(inp.c("columns", 4))
    items = inp.items_ref()
    maxw, maxh = _dims(items)
    levelH, pitch0, pitch90 = maxh + gap, maxw + gap, maxh + gap
    items = vl_sort(items, lambda a, b: a[2] if a[2] != b[2]
                    else (a[3] if a[3] != b[3] else a[4]))
    klist = [0]
    for k in range(1, cols + 1):
        klist += [k, -k]

    placed = inp.obstacles_placed()
    res = []
    for it in items:
        ctr, base_x, base_y, w, h = it[0], it[2], it[3], it[4], it[5]
        cx, cy = ctr[0], ctr[1]
        bl, br, bb, bt = base_x, base_x + w, base_y - h / 2.0, base_y + h / 2.0
        best = bestcost = None
        if avoid:
            hit, cross = eval_place(placed, bl, br, bb, bt,
                                    cx, cy, cx, base_y, cx, base_y,
                                    base_x, base_y)
        else:
            hit, cross = box_hit_p(placed, bl, br, bb, bt), 0
        if not hit and cross == 0:
            best = [0, base_x, base_y, bl, br, bb, bt,
                    cx, cy, cx, base_y, cx, base_y, base_x, base_y]
        if best is None:
            for rot in ([0, 90] if rotallow else [0]):
                step = (w + gap) if rot == 90 else levelH
                for k in klist:
                    ax = base_x + k * (pitch90 if rot == 90 else pitch0)
                    found, o, oo = False, 0, None
                    while not found:
                        for oo2 in ([0] if o == 0 else [o, -o]):
                            if not found:
                                oo = oo2
                                ay = base_y + oo * step
                                if rot == 0:
                                    bl, br = ax, ax + w
                                    bb, bt = ay - h / 2.0, ay + h / 2.0
                                else:
                                    bl, br = ax - h / 2.0, ax + h / 2.0
                                    bb, bt = ay - w / 2.0, ay + w / 2.0
                                if not box_hit_p(placed, bl, br, bb, bt):
                                    found = True
                        o += 1
                        if o > 400:
                            found = True
                    if oo is not None:
                        cross0 = 0
                        if avoid:
                            if rot == 0:
                                s1 = (cx, cy, cx, ay)
                                s2 = (cx, ay, ax, ay)
                            else:
                                s1 = (cx, cy, cx, ay)
                                s2 = (cx, ay, ax - h / 2.0, ay)
                            _, cross0 = eval_place(placed, bl, br, bb, bt,
                                                   *s1, *s2)
                        cand_cost = (abs(k * (pitch90 if rot == 90 else pitch0))
                                     + abs(oo * step)
                                     + (3.0 if rot == 90 else 0.0)
                                     + cross0 * 8.0)
                    else:
                        cand_cost = None
                    if oo is not None and (bestcost is None
                                           or cand_cost < bestcost):
                        bestcost = cand_cost
                        # 与 LISP 的 best 同构：15 元素
                        best = [rot, ax, ay, bl, br, bb, bt]
                        best += [cx, cy, cx, ay, cx, ay, ax, ay] if rot == 0 \
                            else [cx, cy, cx, ay, cx, ay, ax - h / 2.0, ay]
        rot, ax, ay = best[0], best[1], best[2]
        bl, br, bb, bt = best[3], best[4], best[5], best[6]
        if rot == 0:
            elbow_x, anchor_x, attach_y, anchor_y = ax, ax, ay, ay
        else:
            elbow_x, anchor_x = ax - h / 2.0, ax
            attach_y, anchor_y = ay, ay - w / 2.0
        placed = placed + [mk_placed(bl, br, bb, bt, best[7], best[8],
                                     best[9], best[10], best[11], best[12],
                                     best[13], best[14])]
        res.append([ctr, anchor_x, anchor_y, rot, elbow_x, attach_y])

    # 绕行走廊后处理：5 个竖向走廊 x 中取穿越文本最少者
    boxes = [[p[0], p[1], p[2], p[3]] for p in placed]
    out = []
    for r in res:
        ctr, ax, ay, rot, anchor_x, anchor_y = r[0], r[1], r[2], r[3], r[4], r[5]
        cx, cy = ctr[0], ctr[1]
        xms = [cx, ax, (cx + ax) / 2.0, (cx + ax + ax) / 3.0,
               (cx + cx + ax) / 3.0]
        best, bestnx = None, None
        for xm in xms:
            pl = dedupe_pts([(cx, cy), (xm, cy), (xm, ay), (ax, ay)])
            nx = route_cross(boxes, pl)
            if bestnx is None or nx < bestnx or \
                    (nx == bestnx and len(pl) < len(best)):
                bestnx, best = nx, pl
        out.append([ctr, anchor_x, anchor_y, rot, best])
    return out


# ---------------------------------------------------------------------------
# 布局 1：右侧表格（dim-leader.lsp :746-792）
# ---------------------------------------------------------------------------


def layout1(inp, state=None):
    gap = inp.c("gap", 1.0)
    items = inp.items_ref()
    maxw, maxh = _dims(items)
    colPitch, rowPitch = maxw + gap, maxh + gap
    x0, yTop, yBottom = -1.0e30, -1.0e30, 1.0e30
    for m in items:
        cx, cy = m[0][0], m[0][1]
        x0 = max(x0, cx)
        yTop = max(yTop, cy)
        yBottom = min(yBottom, cy)
    for o in inp.obstacles:
        x0 = max(x0, o[1])
        yTop = max(yTop, o[3])
        yBottom = min(yBottom, o[2])
    rows = max(1, fix((yTop - yBottom) / rowPitch))
    x0 += colPitch + 5.0
    yBottom -= 2.0
    items = vl_sort(items, lambda a, b: a[0][1] < b[0][1])
    trkPitch = max(0.25, (yTop - yBottom) / max(1, len(items)))
    res = []
    for i, m in enumerate(items):
        ctr, cx, cy, h = m[0], m[0][0], m[0][1], m[5]
        col, row = fix(i / rows), lrem(i, rows)
        lx = x0 + col * colPitch
        ly = yBottom + row * rowPitch + h / 2.0
        gy = yBottom + i * trkPitch
        route = dedupe_pts([(cx, cy), (cx, gy), (lx - 0.5, gy),
                            (lx - 0.5, ly), (lx, ly)])
        res.append([ctr, lx, ly, 0, route])
    return res[::-1]


# ---------------------------------------------------------------------------
# 布局 2：分区表格（dim-leader.lsp :798-856）
# ---------------------------------------------------------------------------


def layout2(inp, state=None):
    nbands = int(inp.c("zones", 8))
    gap = inp.c("gap", 1.0)
    items = inp.items_ref()
    maxw, maxh = _dims(items)
    colPitch, rowPitch = maxw + gap, maxh + gap
    yTop, yBottom = -1.0e30, 1.0e30
    for m in items:
        yTop = max(yTop, m[0][1])
        yBottom = min(yBottom, m[0][1])
    for o in inp.obstacles:
        yTop = max(yTop, o[3])
        yBottom = min(yBottom, o[2])
    yBottom -= 2.0
    bandH = (yTop - yBottom) / nbands
    res = []
    for band in range(nbands):
        b0 = yBottom + band * bandH
        b1 = b0 + bandH
        bvals, bmaxx = [], -1.0e30
        for m in items:
            cx, cy = m[0][0], m[0][1]
            if b0 <= cy < b1:
                bvals.append(m)
                bmaxx = max(bmaxx, cx)
        if not bvals:
            continue
        bvals = vl_sort(bvals, lambda a, b: a[0][1] < b[0][1])
        r0 = fix((b0 - yBottom) / rowPitch)
        r1 = max(r0 + 1, fix((b1 - yBottom) / rowPitch))
        rows = r1 - r0
        for i, m in enumerate(bvals):
            ctr, cx, cy, h = m[0], m[0][0], m[0][1], m[5]
            col = fix(i / rows)
            rr = r0 + lrem(i, rows)
            lx = bmaxx + colPitch + 3.0 + col * colPitch
            ly = yBottom + rr * rowPitch + h / 2.0
            gy = yBottom + rr * rowPitch + maxh + gap / 2.0
            route = dedupe_pts([(cx, cy), (cx, gy), (lx - 0.5, gy),
                                (lx - 0.5, ly), (lx, ly)])
            res.append([ctr, lx, ly, 0, route])
    return res[::-1]


# ---------------------------------------------------------------------------
# 布局 3：二维分区 + 空白矩形（dim-leader.lsp :862-994）
# ---------------------------------------------------------------------------


def layout3(inp, state=None):
    nz = 3          # LISP 里硬编码 3（run:1214）
    gap = inp.c("gap", 1.0)
    items = inp.items_ref()
    maxw, maxh = _dims(items)
    boxlist = [[m[2], m[2] + m[4], m[3] - m[5] / 2.0, m[3] + m[5] / 2.0]
               for m in items]
    boxlist += [list(o) for o in inp.obstacles]
    colPitch, rowPitch = maxw + gap, maxh + gap
    minX, maxX, minY, maxY = 1.0e30, -1.0e30, 1.0e30, -1.0e30
    for b in boxlist:
        minX, maxX = min(minX, b[0]), max(maxX, b[1])
        minY, maxY = min(minY, b[2]), max(maxY, b[3])
    for m in items:
        minX, maxX = min(minX, m[0][0]), max(maxX, m[0][0])
        minY, maxY = min(minY, m[0][1]), max(maxY, m[0][1])
    minX, minY = minX - colPitch, minY - rowPitch
    maxX, maxY = maxX + colPitch, maxY + rowPitch
    nx = 1 + fix((maxX - minX) / colPitch)
    ny = 1 + fix((maxY - minY) / rowPitch)
    grid = [[False] * nx for _ in range(ny)]
    for b in boxlist:
        i0, i1 = fix((b[0] - minX) / colPitch), fix((b[1] - minX) / colPitch)
        j0, j1 = fix((b[2] - minY) / rowPitch), fix((b[3] - minY) / rowPitch)
        for i in range(i0, i1 + 1):
            for j in range(j0, j1 + 1):
                if 0 <= i < nx and 0 <= j < ny:
                    grid[j][i] = True

    zoneW, zoneH = (maxX - minX) / nz, (maxY - minY) / nz
    allLab = []
    for z in range(nz * nz):
        cxz = minX + (z % nz) * zoneW
        cyz = minY + fix(z / nz) * zoneH
        labList = [m for m in items
                   if cxz <= m[0][0] < cxz + zoneW and cyz <= m[0][1] < cyz + zoneH]
        if not labList:
            continue
        nLab = len(labList)
        tabRows = max(1, fix(zoneH / rowPitch))
        tabCols = max(1, 1 + fix((1 - nLab) / tabRows))
        best, bestd = None, None
        for j0 in range(0, ny - tabRows + 1):
            for i0 in range(0, nx - tabCols + 1):
                ok = True
                for ii in range(tabCols):
                    for jj in range(tabRows):
                        if grid[j0 + jj][i0 + ii]:
                            ok = False
                    if not ok:
                        break
                if ok:
                    c = (minX + i0 * colPitch, minY + j0 * rowPitch)
                    d = math.hypot(c[0] - cxz, c[1] - (cyz + zoneH / 2.0))
                    if bestd is None or d < bestd:
                        bestd, best = d, (i0, j0)
        if best is None:
            best = (0, 0)
        i0, j0 = best
        for k, m in enumerate(labList):
            c, rr = fix(k / tabRows), lrem(k, tabRows)
            lx = minX + (i0 + c) * colPitch
            ly = minY + (j0 + rr) * rowPitch + m[5] / 2.0
            allLab.append([m[0], m[1], lx, ly, m[4], m[5]])

    for L in allLab:
        i0, i1 = (fix((L[2] - minX) / colPitch),
                  fix((L[2] + L[4] - minX) / colPitch))
        j0, j1 = (fix((L[3] - L[5] / 2.0 - minY) / rowPitch),
                  fix((L[3] + L[5] / 2.0 - minY) / rowPitch))
        for i in range(i0, i1 + 1):
            for j in range(j0, j1 + 1):
                if 0 <= i < nx and 0 <= j < ny:
                    grid[j][i] = True

    res = []
    for L in allLab:
        ctr, lx, ly = L[0], L[2], L[3]
        cx, cy = ctr[0], ctr[1]
        ci1 = max(0, fix((cx - minX) / colPitch))
        ci2 = min(nx - 1, max(ci1, 1 - fix((lx - minX) / colPitch)))
        jc = max(0, min(ny - 1, fix((cy - minY) / rowPitch)))
        gy, kk = None, 0
        while gy is None and kk <= ny:
            for jj in ([jc] if kk == 0 else [jc + kk, jc - kk]):
                if gy is None and 0 <= jj < ny:
                    ok = True
                    for ii in range(ci1, ci2 + 1):
                        if grid[jj][ii]:
                            ok = False
                    if ok:
                        gy = minY + jj * rowPitch + rowPitch / 2.0
            kk += 1
        if gy is None:
            gy = minY + jc * rowPitch + rowPitch / 2.0
        route = dedupe_pts([(cx, cy), (cx, gy), (lx - 0.5, gy),
                            (lx - 0.5, ly), (lx, ly)])
        res.append([ctr, lx, ly, 0, route])
    return res[::-1]


# ---------------------------------------------------------------------------
# 布局 4：四向（dim-leader.lsp :1000-1050）
# ---------------------------------------------------------------------------


def layout4(inp, state=None):
    gap = inp.c("gap", 1.0)
    items = inp.items_ref()
    maxw, maxh = _dims(items)
    levelH = maxh + gap
    n = len(items)
    Cx = sum(m[0][0] for m in items) / n
    Cy = sum(m[0][1] for m in items) / n
    items = vl_sort(items, lambda a, b: a[0][1] < b[0][1])
    placed = [list(o) for o in inp.obstacles]
    res = []
    for m in items:
        ctr = m[0]
        cx, cy, w, h = ctr[0], ctr[1], m[4], m[5]
        if abs(cx - Cx) >= abs(cy - Cy):
            d0 = 1 if cx >= Cx else 2
        else:
            d0 = 3 if cy >= Cy else 4
        dlist = []
        for d in [d0, 1, 2, 3, 4]:
            if d not in dlist:
                dlist.append(d)
        best, off = None, 2.0
        bl = br = bb = bt = ax = ay = hj = vj = 0
        while best is None and off < 400.0:
            for d in dlist:
                if best is not None:
                    break
                if d == 1:
                    bl, br = cx + off, cx + off + w
                    bb, bt = cy - h / 2.0, cy + h / 2.0
                    ax, ay, hj, vj = cx + off, cy, 0, 2
                elif d == 2:
                    bl, br = cx - off - w, cx - off
                    bb, bt = cy - h / 2.0, cy + h / 2.0
                    ax, ay, hj, vj = cx - off, cy, 2, 2
                elif d == 3:
                    bl, br = cx - w / 2.0, cx + w / 2.0
                    bb, bt = cy + off, cy + off + h
                    ax, ay, hj, vj = cx, cy + off, 1, 0
                else:
                    bl, br = cx - w / 2.0, cx + w / 2.0
                    bb, bt = cy - off - h, cy - off
                    ax, ay, hj, vj = cx, cy - off, 1, 3
                if not box_hit_p(placed, bl, br, bb, bt):
                    best = [bl, br, bb, bt, ax, ay, hj, vj]
            off += levelH
        if best is None:
            best = [bl, br, bb, bt, cx, cy, 0, 2]
        placed = placed + [best[0:4]]
        res.append([ctr, best[4], best[5], 0, best[6], best[7],
                    dedupe_pts([(cx, cy), (best[4], best[5])])])
    return res[::-1]


# ---------------------------------------------------------------------------
# 布局 5：分向轨道（dim-leader.lsp :1057-1129）
# ---------------------------------------------------------------------------


def layout5(inp, state=None):
    gap = inp.c("gap", 1.0)
    items = inp.items_cwh()
    maxw, maxh = _dims(items)
    minX, maxX, minY, maxY = 1.0e30, -1.0e30, 1.0e30, -1.0e30
    sx = sy = 0.0
    for m in items:
        cx, cy = m[0][0], m[0][1]
        minX, maxX = min(minX, cx), max(maxX, cx)
        minY, maxY = min(minY, cy), max(maxY, cy)
        sx += cx
        sy += cy
    n = len(items)
    Cx, Cy = sx / n, sy / n
    levelH = maxh + gap
    XR = maxX + maxw + gap + 5.0
    XL = minX - maxw - gap - 5.0
    YU = maxY + maxh + gap + 5.0
    YD = minY - maxh - gap - 5.0
    groups = [[], [], [], []]
    for m in items:
        cx, cy = m[0][0], m[0][1]
        if abs(cx - Cx) >= abs(cy - Cy):
            dd = 1 if cx >= Cx else 2
        else:
            dd = 3 if cy >= Cy else 4
        groups[dd - 1].append(m)
    rows = max(1, fix((maxY - minY) / (maxh + gap)))
    res = []

    def side(g, key, vertical, left):
        out = []
        g2 = vl_sort(list(reversed(g)), lambda a, b: a[0][key] < b[0][key])
        for i, m in enumerate(g2):
            ctr = m[0]
            cx, cy, w, h = ctr[0], ctr[1], m[2], m[3]
            col, row = fix(i / rows), lrem(i, rows)
            gy = minY + fix((cy - minY) / levelH) * levelH + maxh + gap / 2.0
            if vertical:
                lx = (XR + col * (maxw + gap)) if not left \
                    else (XL - col * (maxw + gap))
                ly = minY + row * (maxh + gap) + h / 2.0
                lane = lx - 0.5 if not left else lx + 0.5
                hj = 0 if not left else 2
                route = dedupe_pts([(cx, cy), (cx, gy), (lane, gy),
                                    (lane, ly), (lx, ly)])
            else:
                ly = (YU + col * (maxh + gap)) if not left \
                    else (YD - col * (maxh + gap))
                lx = minX + row * (maxw + gap) + w / 2.0
                glx = lx - w / 2.0 - 0.5
                route = dedupe_pts([(cx, cy), (cx, gy), (glx, gy),
                                    (glx, ly), (lx, ly)])
                hj = 1
                vj = 0 if not left else 3
                out.append([ctr, lx, ly, 0, hj, vj, route])
                continue
            out.append([ctr, lx, ly, 0, hj, 2, route])
        return out

    res += side(groups[0], 1, True, False)    # 右：竖轨道
    res += side(groups[1], 1, True, True)     # 左：竖轨道，右对齐
    res += side(groups[2], 0, False, False)   # 上：横轨道，底对齐
    res += side(groups[3], 0, False, True)    # 下：横轨道，顶对齐
    return res[::-1]


# ---------------------------------------------------------------------------
# 布局 6：芯片式左右分区（dim-leader.lsp :371-493 + :279-369）
# ---------------------------------------------------------------------------


def chip_find_route(start, goal, xs, ys, tracks, boxes, keepouts, gap):
    sx, sy = start
    gx, gy = goal
    best, bestcost = None, None

    def consider(cand):
        nonlocal best, bestcost
        cand = dedupe_pts(cand)
        if len(cand) <= 1 or not route_safe_p(cand, boxes, keepouts):
            return
        cost = route_length(cand) + route_bends(cand) * gap * 2.0
        if bestcost is None or cost < bestcost:
            best, bestcost = cand, cost

    if sx == gx or sy == gy:
        consider([start, goal])
    consider([start, (sx, gy), goal])
    consider([start, (gx, sy), goal])
    for x in xs:
        consider([start, (x, sy), (x, gy), goal])
    for y in ys:
        consider([start, (sx, y), (gx, y), goal])
    for x in xs:
        for y in tracks:
            consider([start, (x, sy), (x, y), (gx, y), goal])
    return best


def chip_route_attempt(plans, boxes, xs, ys, tracks, clearance, gap):
    out, keepouts = [], []
    for plan in plans:
        route = dedupe_pts(chip_find_route(plan[8], plan[9], xs, ys, tracks,
                                           boxes, keepouts, gap) or [])
        if route and len(route) > 1:
            out.append(plan + [route])
            keepouts = keepouts + route_keepouts(route, clearance)
    return out[::-1] if len(out) == len(plans) else None


def routes_valid_p(routes, boxes):
    for i, r in enumerate(routes):
        if route_hit_boxes(r, boxes):
            return False
        for j in range(i + 1, len(routes)):
            if route_hit_route(r, routes[j]):
                return False
    return True


def chip_side_plans(g, side, edge, rowLo, rowPitch, rowCap, colPitch, gap):
    plans = []
    n = len(g)
    colCount = max(1, 1 + fix((1 - n) / rowCap))
    rowCount = max(1, fix((n + colCount - 1) / colCount))
    for i in range(n):
        col, row = fix(i / rowCount), lrem(i, rowCount)
        ctr, w, h = g[i][0], g[i][2], g[i][3]
        ay = rowLo + row * rowPitch
        if side == 1:
            ax = edge + col * colPitch
            box = [ax, ax + w, ay - h / 2.0, ay + h / 2.0]
            goal = (ax - gap / 2.0, ay)
            hj = 0
        else:
            ax = edge - col * colPitch
            box = [ax - w, ax, ay - h / 2.0, ay + h / 2.0]
            goal = (ax + gap / 2.0, ay)
            hj = 2
        plans.append([side, ctr, None, ax, ay, hj, 2, box, (ctr[0], ctr[1]),
                      goal])
    return plans


def layout6(inp, state=None):
    gap = inp.c("gap", 1.0)
    items = inp.items_cwh()
    maxw, maxh = _dims(items)
    if not items or maxw == 0.0 or maxh == 0.0:
        if state is not None:
            state["error"] = "无可用文字尺寸"
        return None

    centerX = sum(m[0][0] for m in items) / len(items)
    rgroup = [m for m in items if m[0][0] >= centerX]
    lgroup = [m for m in items if m[0][0] < centerX]
    boxes = []
    minX, maxX, minY, maxY = 1.0e30, -1.0e30, 1.0e30, -1.0e30
    for m in items:
        cx, cy = m[0][0], m[0][1]
        minX, maxX = min(minX, cx), max(maxX, cx)
        minY, maxY = min(minY, cy), max(maxY, cy)
    for ob in inp.obstacles_placed():
        b = ob[0:4]
        boxes.append(b)
        minX, maxX = min(minX, b[0]), max(maxX, b[1])
        minY, maxY = min(minY, b[2]), max(maxY, b[3])
    margin = max(gap * 3.0, maxh)
    rowPitch, colPitch = maxh + gap, maxw + gap
    rowLo = minY + margin - maxh / 2.0
    rowHi = maxY - margin + maxh / 2.0
    rowCap = max(1, 1 + fix((rowHi - rowLo) / rowPitch))
    rgroup = vl_sort(rgroup, lambda a, b: a[0][1] < b[0][1])
    lgroup = vl_sort(lgroup, lambda a, b: a[0][1] < b[0][1])
    rEdge, lEdge = maxX + margin, minX - margin

    plans = []
    if rgroup:
        plans += chip_side_plans(rgroup, 1, rEdge, rowLo, rowPitch, rowCap,
                                 colPitch, gap)
    if lgroup:
        plans += chip_side_plans(lgroup, 2, lEdge, rowLo, rowPitch, rowCap,
                                 colPitch, gap)
    boxes = boxes + [p[7] for p in plans]

    vals = []
    for p in plans:
        vals += [p[7][0], p[7][1], p[8][0], p[8][0], p[9][0], p[9][0]]
    for b in boxes:
        vals += [b[0], b[1]]
    xs = thin_nums(expand_nums(vals, gap), 90)
    vals = []
    for p in plans:
        vals += [p[7][2], p[7][3], p[8][1], p[8][1], p[9][1], p[9][1]]
    for b in boxes:
        vals += [b[2], b[3]]
    ys = thin_nums(expand_nums(vals, gap), 90)

    tracks = []
    for k in range(4):
        tracks += [rowLo - margin - k * gap, rowHi + margin + k * gap]
    tracks = unique_nums(tracks)

    routed = None
    for clearance in [max(0.001, gap / 8.0), max(0.001, gap / 20.0), 0.001]:
        if routed is not None:
            break
        for order in (plans, plans[::-1]):
            if routed is not None:
                break
            if order:
                routed = chip_route_attempt(order, boxes, xs, ys, tracks,
                                            clearance, gap)
    if routed is None:
        if state is not None:
            state["error"] = "无可用正交通道，未修改图形"
        return None
    routeplans = [p[-1] for p in routed]
    if not routes_valid_p(routeplans, boxes):
        if state is not None:
            state["error"] = "引线交叉复核未通过，未修改图形"
        return None
    if state is not None:
        state["error"] = None
    return [[p[1], None, p[3], p[4], 0, p[5], p[6], p[-1]]
            for p in routed][::-1]


# ---------------------------------------------------------------------------
# 布局 7：紧凑双侧（chip.lsp）
# ---------------------------------------------------------------------------


def compact_side(g, side, up, gap, D, K, minX, maxX, yc):
    m = len(g)
    maxh, maxw = 0.0, 0.0
    for mm in g:
        maxh = max(maxh, mm[3])
        maxw = max(maxw, mm[2])
    rowPitch, colPitch = maxh + gap, maxw + gap
    pairs = [[mm[0][1], r] for r, mm in enumerate(g)]
    pairs = vl_sort(pairs, lambda a, b: a[0] < b[0])
    if up:
        pairs = pairs[::-1]
    order = [p[1] for p in pairs]
    lo, hi = ((maxX + 0.5, maxX + D - 0.5) if side == 1
              else (minX - D + 0.5, minX - 0.5))
    res = []
    for r, c in enumerate(order):
        mm = g[c]
        w, h, ctr = mm[2], mm[3], mm[0]
        laneRank = (m - 1 - r) if up else r
        lane = lo + (hi - lo) / m * (laneRank + 0.5)
        ty = (yc + gap / 2.0 + maxh / 2.0 + (m - 1 - r) * rowPitch if up
              else yc - gap / 2.0 - maxh / 2.0 - (m - 1 - r) * rowPitch)
        col = lrem(r, K)
        if side == 1:
            tx = maxX + D + col * colPitch
            att = (tx - gap / 2.0, ty)
            hj = 0
        else:
            tx = minX - D - col * colPitch
            att = (tx + gap / 2.0, ty)
            hj = 2
        res.append([ctr, None, tx, ty, 0, hj, 2,
                    dedupe_pts([(ctr[0], ctr[1]), (lane, ctr[1]),
                                (lane, ty), att])])
    return res[::-1]


def layout7(inp, state=None):
    gap = inp.c("gap", 1.0)
    D = inp.c("side-distance", 10.0)
    if D is None or D <= 0:
        D = 10.0
    K = 3
    items = inp.items_cwh()
    maxw, maxh = _dims(items)
    if not items or maxw == 0.0 or maxh == 0.0:
        if state is not None:
            state["error"] = "无可用文字尺寸"
        return None
    minX, maxX, minY, maxY = 1.0e30, -1.0e30, 1.0e30, -1.0e30
    for m in items:
        cx, cy = m[0][0], m[0][1]
        minX, maxX = min(minX, cx), max(maxX, cx)
        minY, maxY = min(minY, cy), max(maxY, cy)
    yc = (minY + maxY) / 2.0
    srt = vl_sort(items, lambda a, b: a[0][0] < b[0][0])
    n = len(srt)
    nl = fix(n / 2)
    left, right = srt[:nl], srt[nl:]
    res = []
    for grp, side in ((left, 2), (right, 1)):
        if not grp:
            continue
        gs = vl_sort(grp, lambda a, b: a[0][1] < b[0][1])
        med = gs[fix(len(gs) / 2)][0][1]
        up = [it for it in gs if it[0][1] >= med]
        dn = [it for it in gs if it[0][1] < med]
        res += compact_side(up, side, True, gap, D, K, minX, maxX, yc)
        res += compact_side(dn, side, False, gap, D, K, minX, maxX, yc)
    if state is not None:
        state["error"] = None
    return res


# ---------------------------------------------------------------------------
# 布局 8：四带通道布线（channels.lsp）
# ---------------------------------------------------------------------------


def ch_ceil(a, b):
    return 1 if a <= 0 else fix((a + b - 1) / b)


def ch_sort_order(idx, lst):
    """按圆心第 idx 分量无损升序（用原始序号打破平局，vl-sort 会丢重复）。"""
    tag = [[z[0][idx], i, z] for i, z in enumerate(lst)]

    def less(a, b):
        return a[0] < b[0] or (not b[0] < a[0] and a[1] < b[1])

    return [t[2] for t in vl_sort(tag, less)]


def ch_isotonic(p, pitch):
    """PAVA 保序回归：等间距(pitch)约束下最贴近 p 的列位表。"""
    v = [x - k * pitch for k, x in enumerate(p)]
    blocks = [[x, 1] for x in v]
    for x in v:
        while len(blocks) >= 2:
            a, b = blocks[-2], blocks[-1]
            if a[0] / a[1] > b[0] / b[1]:
                blocks = blocks[:-2] + [[a[0] + b[0], a[1] + b[1]]]
            else:
                break
    res, k = [], 0
    for b in blocks:
        vk = b[0] / b[1]
        for _ in range(b[1]):
            res.append(vk + k * pitch)
            k += 1
    return res


def ch_left_edge(spans):
    """left-edge 区间分配，返回 (轨道号表, 轨道总数)。"""
    n = len(spans)
    lst = [[min(s[0], s[1]), max(s[0], s[1]), i] for i, s in enumerate(spans)]

    def less(a, b):
        if a[0] != b[0]:
            return a[0] < b[0]
        if a[1] != b[1]:
            return a[1] < b[1]
        return a[2] < b[2]

    lst = vl_sort(lst, less)
    ends, assign = [], [0] * n
    for s, e, idx in lst:
        found, tr = False, 0
        while not found and tr < len(ends):
            if ends[tr] <= s + 1.0e-9:
                ends[tr] = e
                assign[idx] = tr
                found = True
            tr += 1
        if not found:
            ends.append(e)
            assign[idx] = len(ends) - 1
    return assign, len(ends)


def ch_chunks(lst, n):
    res, lst = [], list(lst)
    while lst:
        seg, k = [], 0
        while lst and k < n:
            seg.append(lst.pop(0))
            k += 1
        res.append(seg)
    return res


def ch_spread(n, lo, hi):
    if n == 1:
        return [(lo + hi) / 2.0]
    return [lo + (hi - lo) / (n - 1) * k for k in range(n)]


def ch_band_blocks(g, horizontal, positive, gap, lane, blocks, maxh,
                   minX, maxX, minY, maxY):
    """分块通道布线：块间缝隙按深行引线数自适应，深行引线经缝上行。"""
    if not g:
        return None
    g = ch_sort_order(0 if horizontal else 1, g)
    ridx = 0 if horizontal else 1
    oidx = 1 - ridx
    pitch = maxh + gap
    runmin, runmax = (minX, maxX) if horizontal else (minY, maxY)
    ran = runmax - runmin
    sgn = 1 if positive else -1
    m = len(g)
    gw = [gap] * (blocks - 1)
    outer = 0
    rows, cap, grid = 0, 0, []
    while outer < 8:
        per0 = max(1, fix(ran / pitch))
        bs = ch_ceil(per0, blocks)
        cap = bs * blocks
        rows = ch_ceil(m, cap)
        extent = cap * pitch + sum(gw)
        mid = (minX + maxX) / 2.0 if horizontal else (minY + maxY) / 2.0
        cols, gapleft = [], []
        xcur = mid - extent / 2.0
        for b in range(blocks):
            for _ in range(bs):
                cols.append([xcur + pitch / 2.0, b])
                xcur += pitch
            if b < blocks - 1:
                gapleft.append(xcur)
                xcur += gw[b]
        # 沿带方向错开（角部避让）
        if horizontal:
            b = ((runmax - (cols[-1][0] + pitch / 2.0)) if positive
                 else (runmin - (cols[0][0] - pitch / 2.0)))
        else:
            b = ((runmin - (cols[0][0] - pitch / 2.0)) if positive
                 else (runmax - (cols[-1][0] + pitch / 2.0)))
        cols = [[cc[0] + b, cc[1]] for cc in cols]
        gapleft = [x + b for x in gapleft]
        grid = []
        for i, z in enumerate(g):
            r, ci = fix(i / cap), lrem(i, cap)
            ci = min(ci, len(cols) - 1)
            u, bi = cols[ci]
            cand = ([bi - 1] if bi > 0 else []) + ([bi] if bi < blocks - 1 else [])
            gi = -1
            if cand:
                gi = cand[0]
                for cc in cand:
                    if (abs(gapleft[cc] + gw[cc] / 2.0 - z[0][ridx])
                            < abs(gapleft[gi] + gw[gi] / 2.0 - z[0][ridx])):
                        gi = cc
            grid.append([z, r, u, gi])
        cntl = [0] * len(gapleft)
        for it in grid:
            if it[1] > 0 and it[3] >= 0:
                cntl[it[3]] += 1
        ngw = [max(gap, c * lane + gap) for c in cntl]
        if ngw == gw:
            outer = 8
        else:
            gw, outer = ngw, outer + 1

    tracks = [None] * len(grid)
    riser = [None] * len(grid)
    for gi in range(len(gapleft)):
        idxs = [k for k, it in enumerate(grid) if it[1] > 0 and it[3] == gi]
        idxs = vl_sort(idxs, lambda a, b: grid[a][2] < grid[b][2])
        for k, ii in enumerate(idxs):
            tracks[ii] = gapleft[gi] + (k + 0.5) * lane
    for k, it in enumerate(grid):
        riser[k] = it[0][0][ridx] if it[1] == 0 else tracks[k]

    lanet = [0] * rows
    lane_t = [None] * len(grid)
    for r in range(rows):
        idxs = [k for k, it in enumerate(grid) if it[1] == r]
        spans = [[min(riser[ii], grid[ii][2]), max(riser[ii], grid[ii][2])]
                 for ii in idxs]
        asg, nt = ch_left_edge(spans)
        lanet[r] = nt
        for j, ii in enumerate(idxs):
            lane_t[ii] = asg[j]

    depth = max(z[2] for z in g)
    edge = [0.0] * rows
    cursor = ((maxY + gap) if positive else (minY - gap)) if horizontal \
        else ((maxX + gap) if positive else (minX - gap))
    for r in range(rows):
        e = cursor + sgn * (lanet[r] * lane + gap)
        edge[r] = e
        cursor = e + sgn * (depth + gap)

    seen = {}
    for k, it in enumerate(grid):
        key = f"{it[0][0][oidx]:.4f}"
        cnt = seen.get(key, 0)
        lane_t[k] = [lane_t[k], cnt]
        seen[key] = cnt + 1

    res = []
    for k, it in enumerate(grid):
        z, r, u, w = it[0], it[1], it[2], it[0][2]
        cx, cy = z[0][0], z[0][1]
        j = lane_t[k][1]
        lanev = (edge[r] - sgn * (lanet[r] * lane + gap)
                 + sgn * ((lane_t[k][0] + 0.5) * lane))
        rx = riser[k]
        if horizontal:
            sy = cy + j * lane
            route = dedupe_pts([(cx, cy), (cx, sy), (rx, sy), (rx, lanev),
                                (u, lanev), (u, edge[r])])
            res.append([z[0], None, u, edge[r] + w / 2.0, 90, 1, 2, route])
        else:
            sx = cx + j * lane
            route = dedupe_pts([(cx, cy), (sx, cy), (sx, rx), (lanev, rx),
                                (lanev, u), (edge[r], u)])
            res.append([z[0], None, edge[r], u, 0, 0 if positive else 2, 2,
                        route])
    return res[::-1]


def ch_band_h(g, above, gap, lane, maxh, minX, maxX, minY, maxY, lines, cols):
    """水平带（上/下）不分块布线：文字旋转 90°，沿 x 排布。"""
    if not g:
        return None
    gs = ch_sort_order(0, g)
    pitch = maxh + gap
    per = (max(1, fix((len(gs) + lines - 1) / lines)) if lines > 0
           else max(1, fix((maxX - minX) / pitch)))
    lo, hi = minX, maxX
    if lines > 0:
        span = max(maxX - minX, per * pitch)
        mid = (minX + maxX) / 2.0
        lo, hi = mid - span / 2.0, mid + span / 2.0
    cursor = (maxY + gap) if above else (minY - gap)
    res = []
    for line in ch_chunks(gs, per):
        n = len(line)
        us = (ch_isotonic([z[0][0] for z in line], pitch)
              if (cols == 1 and lines == 0) else ch_spread(n, lo, hi))
        spans = [[min(z[0][0], us[k]), max(z[0][0], us[k])]
                 for k, z in enumerate(line)]
        assign, nt = ch_left_edge(spans)
        depth = max(z[2] for z in line)
        if above:
            textb = cursor + nt * lane + gap
        else:
            textt = cursor - nt * lane - gap
        for k, z in enumerate(line):
            u, w = us[k], z[2]
            cx, cy = z[0][0], z[0][1]
            ly = ((cursor + (assign[k] + 0.5) * lane) if above
                  else (cursor - (assign[k] + 0.5) * lane))
            if above:
                res.append([z[0], None, u, textb + w / 2.0, 90, 1, 2,
                            dedupe_pts([(cx, cy), (cx, ly), (u, ly),
                                        (u, textb)])])
            else:
                res.append([z[0], None, u, textt - w / 2.0, 90, 1, 2,
                            dedupe_pts([(cx, cy), (cx, ly), (u, ly),
                                        (u, textt)])])
        cursor = (textb + depth + gap) if above else (textt - depth - gap)
    return res[::-1]


def ch_band_v(g, right, gap, lane, maxh, minX, maxX, minY, maxY, lines, cols):
    """竖直带（右/左）不分块布线：文字水平，沿 y 排布。"""
    if not g:
        return None
    gs = ch_sort_order(1, g)
    pitch = maxh + gap
    per = (max(1, fix((len(gs) + lines - 1) / lines)) if lines > 0
           else max(1, fix((maxY - minY) / pitch)))
    lo, hi = minY, maxY
    if lines > 0:
        span = max(maxY - minY, per * pitch)
        mid = (minY + maxY) / 2.0
        lo, hi = mid - span / 2.0, mid + span / 2.0
    cursor = (maxX + gap) if right else (minX - gap)
    res = []
    for line in ch_chunks(gs, per):
        n = len(line)
        if cols == 1 and lines == 0:
            us = ch_isotonic([z[0][1] for z in line], pitch)
            if us[0] < minY:
                us = [v + (minY - us[0]) for v in us]
            if us[-1] > maxY:
                us = [v + (maxY - us[-1]) for v in us]
        else:
            us = ch_spread(n, lo, hi)
        spans = [[min(z[0][1], us[k]), max(z[0][1], us[k])]
                 for k, z in enumerate(line)]
        assign, nt = ch_left_edge(spans)
        depth = max(z[2] for z in line)
        if right:
            textl = cursor + nt * lane + gap
        else:
            textr = cursor - nt * lane - gap
        for k, z in enumerate(line):
            u, w = us[k], z[2]
            cx, cy = z[0][0], z[0][1]
            lx = ((cursor + (assign[k] + 0.5) * lane) if right
                  else (cursor - (assign[k] + 0.5) * lane))
            if right:
                res.append([z[0], None, textl, u, 0, 0, 2,
                            dedupe_pts([(cx, cy), (lx, cy), (lx, u),
                                        (textl, u)])])
            else:
                res.append([z[0], None, textr, u, 0, 2, 2,
                            dedupe_pts([(cx, cy), (lx, cy), (lx, u),
                                        (textr, u)])])
        cursor = (textl + depth + gap) if right else (textr - depth - gap)
    return res[::-1]


def layout8(inp, state=None):
    gap = inp.c("gap", 1.0)
    lane = inp.c("lane-gap", 1.0)
    if lane is None or lane <= 0:
        lane = gap
    lines = int(inp.c("channels-lines", 0) or 0)
    cols = inp.c("channels-columns", 1)
    if cols not in (0, 1):
        cols = 1
    blocks = int(inp.c("channels-blocks", 0) or 0)
    if blocks < 0:
        blocks = 0

    items = inp.items_cwh()
    if len(items) < 2:
        return None
    maxw, maxh = _dims(items)
    minX = min(m[0][0] for m in items)
    maxX = max(m[0][0] for m in items)
    minY = min(m[0][1] for m in items)
    maxY = max(m[0][1] for m in items)

    groups = [[], [], [], []]
    for z in items:
        cx, cy = z[0][0], z[0][1]
        d = [maxY - cy, cy - minY, cx - minX, maxX - cx]
        side = 0
        if d[1] < d[side]:
            side = 1
        if d[2] < d[side]:
            side = 2
        if d[3] < d[side]:
            side = 3
        groups[side].append(z)

    res = []
    order = ((groups[0], True, True), (groups[1], True, False),
             (groups[3], False, True), (groups[2], False, False))
    if blocks >= 2:
        for g, horiz, pos in order:
            r = ch_band_blocks(g, horiz, pos, gap, lane, blocks, maxh,
                               minX, maxX, minY, maxY)
            if r:
                res += r
    else:
        for g, horiz, pos in order:
            r = (ch_band_h(g, pos, gap, lane, maxh, minX, maxX, minY, maxY,
                           lines, cols) if horiz
                 else ch_band_v(g, pos, gap, lane, maxh, minX, maxX, minY, maxY,
                                lines, cols))
            if r:
                res += r
    return res


# ---------------------------------------------------------------------------
# 驱动
# ---------------------------------------------------------------------------

LAYOUTS = {
    0: ("本地错开", layout0),
    1: ("右侧表格", layout1),
    2: ("分区表格", layout2),
    3: ("二维分区", layout3),
    4: ("四向", layout4),
    5: ("分向轨道", layout5),
    6: ("芯片式左右分区", layout6),
    7: ("紧凑双侧", layout7),
    8: ("通道布线", layout8),
}


def normalize(res):
    """把各布局的记录统一成 (ctr ax ay rot hj vj route)。

    LISP 侧记录长度本来就不统一（1/2/3 是 6 元素、4/5/6/7/8 是 8 元素、0 是 6 元素），
    这里只做索引归一，不改数值。
    """
    out = []
    for r in res:
        n = len(r)
        if n == 8:
            out.append((r[0], r[2], r[3], r[4], r[5], r[6], r[7]))
        elif n == 7:                       # 布局 4/5：少一个 ent 占位
            out.append((r[0], r[1], r[2], r[3], r[4], r[5], r[6]))
        elif n == 5:                       # 布局 1/2/3：(ctr lx ly 0 route)
            out.append((r[0], r[1], r[2], 0, 0, 0, r[4]))
        else:                              # 布局 0 后处理结果：(ctr ax ay rot route)
            out.append((r[0], r[1], r[2], r[3], 0, 0, r[4]))
    return out


def stats(res):
    """引线总长、平均折点数、盒重叠数（供对比参考，不参与耗时统计）。"""
    if not res:
        return dict(n=0, avg_len=0.0, bends=0)
    tot, nb = 0.0, 0
    for r in res:
        route = r[-1]
        tot += route_length(route)
        nb += route_bends(route)
    return dict(n=len(res), avg_len=round(tot / len(res), 2), bends=nb)


def subsample(data, limit):
    """按原顺序等间隔取前 limit 个匹配点（用于测规模曲线，不改算法）。"""
    if not limit or limit >= len(data["matched"]):
        return data
    m = data["matched"]
    step = len(m) / float(limit)
    picked = [m[min(len(m) - 1, int(i * step))] for i in range(limit)]
    out = dict(data)
    out["matched"] = picked
    return out


def run_one(fn, inp, repeat=1):
    state = {}
    best, res = None, None
    for _ in range(repeat):
        t0 = time.perf_counter()
        res = fn(inp, state)
        dt = time.perf_counter() - t0
        best = dt if best is None else min(best, dt)
    return best, res, state


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("data", nargs="?", default="data.json")
    ap.add_argument("--only", type=int, default=None)
    ap.add_argument("--out", default=None)
    ap.add_argument("--repeat", type=int, default=1)
    ap.add_argument("--limit", type=int, default=0,
                    help="只取前 N 个匹配点（测规模曲线）")
    ap.add_argument("--scale", default="",
                    help="逗号分隔的 N 列表，逐个测并输出 JSON")
    ap.add_argument("--json", action="store_true", help="只输出 JSON 结果")
    a = ap.parse_args()

    raw = json.load(open(a.data, encoding="utf-8"))

    if a.scale:
        scale = {}
        for n in [int(x) for x in a.scale.split(",")]:
            inp = Inp(subsample(raw, n))
            row = {}
            for lid in sorted(LAYOUTS):
                name, fn = LAYOUTS[lid]
                sec, res, state = run_one(fn, inp, a.repeat)
                row[lid] = dict(name=name, n_in=len(inp.matched), sec=sec,
                                error=state.get("error"))
                print(f"  n={len(inp.matched):4d} L{lid} {name:16s} "
                      f"{sec * 1000:9.1f} ms"
                      + (f"  err={state.get('error')}"
                         if state.get("error") else ""), flush=True)
            scale[n] = row
        print(json.dumps(scale, ensure_ascii=False))
        return

    inp = Inp(subsample(raw, a.limit))
    ids = [a.only] if a.only is not None else sorted(LAYOUTS)
    report = {}
    for lid in ids:
        name, fn = LAYOUTS[lid]
        sec, res, state = run_one(fn, inp, a.repeat)
        st = stats(res)
        report[lid] = dict(name=name, sec=round(sec, 4), **st,
                           error=state.get("error"))
        if a.out and lid == a.only:
            json.dump(res, open(a.out, "w", encoding="utf-8"))
        if not a.json:
            err = f"  err={state.get('error')}" if state.get("error") else ""
            print(f"L{lid} {name:16s} {sec * 1000:9.1f} ms  "
                  f"n={st['n']:4d} avg_len={st['avg_len']:7.1f} "
                  f"bends={st['bends']:5d}{err}", flush=True)
    if a.json:
        print(json.dumps(report, ensure_ascii=False))


if __name__ == "__main__":
    main()
