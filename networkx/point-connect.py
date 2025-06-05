import math
from typing import List
 
def point_connect(pots: List[list]):
    """
    完成所有点的连接，返回所有线段的list(线段中包含两个点的索引)和最短路径值，要求所有线路之和最短。
    例如输入 pots = [[0, 0], [2, 0], [3, 2], [3, -2]]
    返回 paths = [[0, 1], [1, 2], [1, 3]], length =
    """
    l = len(pots)
    if l <= 1:
        return [], 0
 
    con = [pots[0]]     # 已经连线的点集，先随便放一个点进去
    not_con = pots[1:]  # 还没连线的点集
    paths = []          # 所有连线
    length_total = 0    # 总连线长度
    for _ in range(l - 1):  # 共 l-1 条连线
        # 得到下一条连线的两点a、b 及其距离length_ab
        a, b = con[0], not_con[0]  # 先任意选两个点
        length_ab = math.sqrt((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2)
        for m in con:
            for n in not_con:
                lg = math.sqrt((m[0] - n[0]) ** 2 + (m[1] - n[1]) ** 2)
                if lg < length_ab:  # 如果有更短的
                    length_ab = lg
                    a, b = m, n
 
        # 记录
        paths.append([pots.index(a), pots.index(b)])   # 记录连线ab
        con.append(b)      # 已连接点集中记录点b
        not_con.remove(b)  # 未连接点集中删除点b
        length_total += length_ab  # 记录总长度
 
    return paths, length_total
