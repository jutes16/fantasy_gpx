"""
A local MCP server for the Bet Tracker's "Refresh my Kalshi bets" button.

The button (only in the Claude desktop app, for the dashboard's owner) calls this
server's one tool, which runs on your Mac:

    python3 kalshi_mine.py     # your Kalshi bets, read-only GETs (the key never leaves your Mac)
    python3 hq.py              # the dashboard's data

and hands back the datasets your Kalshi bets feed (bets, timeline, summary). The
page then saves them to the dashboard itself. Setup (once):

    claude mcp add --scope user kalshi_hq -- \\
        /Users/jgilbert/anaconda3/envs/neuron_mpi_arm64/bin/python3 \\
        /Users/jgilbert/Documents/GitHub/fantasy_gpx/nfl_bets/kalshi_mcp.py

The scripts run in a login + interactive zsh so they see KALSHI_API_KEY_ID,
KALSHI_PRIVATE_KEY_PATH and MY_UNIT_DOLLARS from ~/.zshrc. Speaks MCP's stdio
transport (one JSON-RPC message per line) with no extra packages.
"""

import json
import os
import subprocess
import sys
from datetime import datetime, timezone

HERE = os.path.dirname(os.path.abspath(__file__))
PYTHON = sys.executable
OUT = os.path.join(HERE, "data", "dashboard_hq")
DATASETS = ("bets", "timeline", "summary")          # what your Kalshi bets change
TOOL = {
    "name": "refresh_my_kalshi",
    "description": "Pull your Kalshi NFL bets (read-only) and rebuild the Bet Tracker's bets, timeline and summary.",
    "inputSchema": {"type": "object", "properties": {}, "additionalProperties": False},
    "annotations": {"title": "Refresh my Kalshi bets", "readOnlyHint": False, "idempotentHint": True,
                    "openWorldHint": True, "destructiveHint": False},
}


def refresh():
    cmd = f'cd "{HERE}" && "{PYTHON}" kalshi_mine.py && "{PYTHON}" hq.py'
    run = subprocess.run(["/bin/zsh", "-lic", cmd], capture_output=True, text=True, timeout=300)
    log = (run.stdout + run.stderr).strip().splitlines()
    if run.returncode != 0:
        raise RuntimeError("\n".join(log[-8:]) or f"exit {run.returncode}")
    data = {k: json.load(open(os.path.join(OUT, f"{k}.json"))) for k in DATASETS}
    return {"at": datetime.now(timezone.utc).isoformat(timespec="seconds"), "log": log[-6:], **data}


def reply(id_, result=None, error=None):
    msg = {"jsonrpc": "2.0", "id": id_}
    msg.update({"error": error} if error else {"result": result})
    sys.stdout.write(json.dumps(msg, default=str) + "\n")
    sys.stdout.flush()


def handle(msg):
    method, id_ = msg.get("method"), msg.get("id")
    if id_ is None:                                   # a notification: nothing to answer
        return
    if method == "initialize":
        reply(id_, {"protocolVersion": msg.get("params", {}).get("protocolVersion", "2025-06-18"),
                    "capabilities": {"tools": {}}, "serverInfo": {"name": "kalshi_hq", "version": "1.0"}})
    elif method == "ping":
        reply(id_, {})
    elif method == "tools/list":
        reply(id_, {"tools": [TOOL]})
    elif method == "tools/call":
        if msg.get("params", {}).get("name") != TOOL["name"]:
            reply(id_, error={"code": -32602, "message": "unknown tool"})
            return
        try:
            out = refresh()
            reply(id_, {"content": [{"type": "text", "text": json.dumps(out, default=str)}], "structuredContent": out})
        except Exception as e:                        # the scripts' own message, e.g. a missing key
            reply(id_, {"content": [{"type": "text", "text": str(e)}], "isError": True})
    else:
        reply(id_, error={"code": -32601, "message": f"no method {method}"})


def main():
    for line in sys.stdin:
        if line.strip():
            try:
                handle(json.loads(line))
            except json.JSONDecodeError:
                pass


if __name__ == "__main__":
    main()
