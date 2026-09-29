"""CAD MCP 服务器 HTTP 客户端（JSON-RPC 2.0 over HTTP POST /mcp）。

服务器地址用环境变量 CAD_MCP_URL 覆盖，默认主 192.168.1.53:8110、备 192.168.1.51:8110
（首次 connect 超时自动切到备机）。
"""
import json
import os
import socket
import urllib.error
import urllib.request

HOSTS = [os.environ["CAD_MCP_URL"]] if os.environ.get("CAD_MCP_URL") else [
    "http://192.168.1.53:8110",   # 主
    "http://192.168.1.55:8110",   # 备
    "http://192.168.1.51:8110",   # 备2（常被占用，容易 stalled）
]
BASE = ""
_id = 0


def _post(base, payload, timeout):
    req = urllib.request.Request(
        base, data=json.dumps(payload).encode("utf-8"),
        headers={"Content-Type": "application/json",
                 "Accept": "application/json, text/event-stream"},
    )
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return json.loads(r.read().decode("utf-8"))


def _pick_base():
    """选一个可达的服务器：先 ping TCP 端口，再实测一次 tools/list。"""
    global BASE, _id
    if BASE:
        return BASE
    for h in HOSTS:
        host, _, port = h.split("//", 1)[1].split("/", 1)[0].partition(":")
        try:
            with socket.create_connection((host, int(port or 80)), timeout=2):
                pass
        except OSError:
            continue
        base = h + "/mcp"
        try:
            d = _post(base, {"jsonrpc": "2.0", "id": 1, "method": "tools/list",
                             "params": {}}, 20)
            if isinstance(d, dict) and d.get("result", {}).get("tools"):
                BASE = base
                return BASE
        except (urllib.error.URLError, OSError, ValueError, AttributeError):
            continue
    raise RuntimeError(f"没有可达的 CAD MCP 服务器: {HOSTS}")


TRANSIENT = ("No open document", "Worker reset", "cad.stall", "cad.stallTimeout")


def _once(base, name, arguments, timeout, rid):
    d = _post(base, {"jsonrpc": "2.0", "id": rid, "method": "tools/call",
                     "params": {"name": name, "arguments": arguments or {}}},
             timeout)
    if "error" in d:
        raise RuntimeError(f"MCP error: {d['error']}")
    res = d.get("result", {})
    if res.get("isError"):
        raise RuntimeError(f"tool error: {res['content'][0]['text']}")
    content = res.get("content") or [{}]
    first = content[0]
    if isinstance(first, dict) and "text" in first:
        return first["text"]
    return json.dumps(first, ensure_ascii=False)


def call(name, arguments=None, timeout=300, retries=3):
    """调用 MCP 工具；对 CAD 瞬时故障(No open document/Worker reset/stall)自动重试。"""
    import time
    _pick_base()
    for k in range(retries):
        try:
            return _once(BASE, name, arguments, timeout, 1)
        except (RuntimeError, OSError) as e:
            transient = any(t in str(e) for t in TRANSIENT)
            if not transient or k == retries - 1:
                raise
            time.sleep(2 + 2 * k)


def lisp_balance(code):
    """给 LISP 片段自动补足右括号，返回 (补平后的源码, 缺少的括号数)。"""
    depth, i, n, instr = 0, 0, len(code), False
    while i < n:
        c = code[i]
        if instr:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                instr = False
            i += 1
            continue
        if c == ";":
            while i < n and code[i] != "\n":
                i += 1
            continue
        if c == '"':
            instr = True
        elif c == "\\":
            i += 2
            continue
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
        i += 1
    return code + ")" * max(0, depth), depth


def lisp_check(code):
    """检查 AutoLISP 源码括号是否平衡（忽略字符串与 ; 注释），不平衡抛异常。

    CAD 的括号错误信息是 GBK 编码且不指出位置，本地先查一遍省事。
    """
    depth, i, n, instr = 0, 0, len(code), False
    line = 1
    while i < n:
        c = code[i]
        if c == "\n":
            line += 1
            i += 1
            continue
        if instr:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                instr = False
            i += 1
            continue
        if c == ";":
            while i < n and code[i] != "\n":
                i += 1
            continue
        if c == '"':
            instr = True
        elif c == "'":
            pass
        elif c == "\\":
            i += 2
            continue
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth < 0:
                raise ValueError(f"AutoLISP 源码第 {line} 行多余的右括号")
        i += 1
    if instr:
        raise ValueError("AutoLISP 源码字符串未闭合")
    if depth:
        raise ValueError(f"AutoLISP 源码缺少 {depth} 个右括号")
    return code


def eval_lisp(code, encoding="utf-8", timeout=300):
    lisp_check(code)
    return call("eval_lisp_with_result",
                {"code": code, "encoding": encoding}, timeout=timeout)


SCENE_DWG = "C:/Users/Public/bench-scene.dwg"
# CAD 主机的 W:/atlisp/ 与本机 ~/atlisp/ 是同一份共享目录，故可直接打开仓库里的 dwg
HOST_ROOT = "W:"
HOST_PKG = HOST_ROOT + "/atlisp/packages/dim-leader"


def host_path(local_path):
    """把本机路径换算成 CAD 主机上的路径（共享目录：W: <-> ~/）。"""
    import os
    p = os.path.abspath(local_path).replace("\\", "/")
    home = os.path.expanduser("~").replace("\\", "/").rstrip("/")
    if p == home:
        return HOST_ROOT
    if p.startswith(home + "/"):
        return HOST_ROOT + "/" + p[len(home) + 1:]
    return p


def open_scene(arcs=None, tries=8, pkg_dir=None, settle=2.0, create=False):
    """打开 CAD 主机上的基准场景（507 点）并加载包。

    CAD 是共用的，活动文档可能被别人切走；每次测量前先打开本场景并核对
    ARC 数量，保证测量对象确定。开新文档会重置 LISP 命名空间，故随后重载包。
    返回当前文档名。
    """
    import time
    probe = ("(if (ssget \"_X\" '((0 . \"ARC\"))) "
             "(itoa (sslength (ssget \"_X\" '((0 . \"ARC\"))))) \"0\")")

    def ready():
        try:
            return int(eval_lisp(probe, timeout=120).strip())
        except (RuntimeError, ValueError):
            return -1

    if arcs is None or ready() != arcs:
        for k in range(tries):
            if open_scene_doc() or (create and k == 0 and ready() == -1):
                time.sleep(settle)
                if ready() == arcs:
                    break
            print(f"  场景未就绪，重试 {k + 1}/{tries}")
            time.sleep(settle)
        else:
            raise RuntimeError(
                f"CAD 当前文档不是基准场景(需要 {arcs} 条 ARC)")
    if pkg_dir:
        ensure_package(pkg_dir)
    return eval_lisp('(getvar "DWGNAME")', timeout=60).strip()


def open_scene_doc():
    """打开 CAD 主机上的基准场景；没有该文件则返回 False（可在当前文档现建）。"""
    try:
        call("open_dwg", {"filePath": SCENE_DWG}, timeout=300)
        return True
    except RuntimeError:
        return False


def ensure_package(pkg_dir, force=False):
    """把本地 .lsp 源码发给 CAD 加载；若已加载则跳过（force=1 强制重载）。"""
    if not force and eval_lisp("(boundp 'dim-leader:find-centers)",
                              timeout=120).strip() == "T":
        return False
    for f in ("pkg.lsp", "dim-leader.lsp", "chip.lsp", "channels.lsp"):
        eval_lisp(open(os.path.join(pkg_dir, f), encoding="utf-8").read(),
                  timeout=300)
    assert eval_lisp("(boundp 'dim-leader:find-centers)", timeout=120).strip() == "T"
    return True
