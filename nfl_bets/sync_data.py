"""
Copy the cloud snapshots (props, Sleeper projections, Kalshi, ELWAY) from the
private data repo into this checkout. Run before grading or `props.py sheet <wk> --cached`.
ELWAY: the week's files are taken when the cloud's pull is newer than the
one here, and the bet sheets the cloud priced when ELWAY updated are logged
(bets.py import_sheets).

    python3 sync_data.py

The data repo is cloned next to this one (../../fantasy_gpx-data) on first use,
or set FANTASY_DATA_REPO. Files already here are kept, except the Sleeper
snapshots, which are merged with the cloud's rows winning (they were taken
closest to kickoff).
"""

import glob
import os
import re
import shutil
import subprocess

import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.environ.get("FANTASY_DATA_REPO", os.path.join(HERE, "..", "..", "fantasy_gpx-data"))
PROPS = os.path.join(HERE, "..", "nfl_fantasy", "data", "props")
KALSHI = os.path.join(HERE, "data", "kalshi")
ELWAY = os.path.join(HERE, "data", "elway")
POOL_ELWAY = os.path.join(HERE, "..", "nfl_model", "data", "elway")


def _feed_time(path):
    if not os.path.exists(path):
        return ""
    m = re.search(r"updated (\d{4}-\d\d-\d\dT[\d:.]+)", open(path).readline())
    return m[1] if m else ""


def copy_elway(src):
    """The cloud's ELWAY week files where its pull is newer than ours, then its bet sheets."""
    took = []
    for a in glob.glob(os.path.join(src, "*_wk*.txt")):
        f = os.path.basename(a)
        b = os.path.join(ELWAY, f)
        if open(a).read() == (open(b).read() if os.path.exists(b) else None) or _feed_time(a) <= _feed_time(b):
            continue
        os.makedirs(ELWAY, exist_ok=True)
        shutil.copy2(a, b)
        shutil.copy2(a, os.path.join(POOL_ELWAY, f))
        d = os.path.join(src, "dist_" + f.replace(".txt", ".csv"))
        if os.path.exists(d):
            shutil.copy2(d, os.path.join(ELWAY, os.path.basename(d)))
        took.append(f)
    print(f"elway: {', '.join(sorted(took)) if took else 'nothing newer'}")
    if os.path.isdir(os.path.join(src, "sheets")):
        import bets
        bets.import_sheets(os.path.join(src, "sheets"))


def pull():
    if os.path.isdir(os.path.join(REPO, ".git")):
        subprocess.run(["git", "-C", REPO, "pull", "--ff-only", "-q"], check=True)
    else:
        subprocess.run(["gh", "repo", "clone", "jutes16/fantasy_gpx-data", REPO, "--", "-q"], check=True)


def copy_tree(src, dst):
    """Copy files missing in dst; merge *_sleeper_at_log.csv with src rows winning."""
    new = merged = 0
    for root, _, files in os.walk(src):
        for f in files:
            if f.startswith("."):                       # .gitkeep and the like
                continue
            a = os.path.join(root, f)
            b = os.path.join(dst, os.path.relpath(a, src))
            os.makedirs(os.path.dirname(b), exist_ok=True)
            if not os.path.exists(b):
                shutil.copy2(a, b)
                new += 1
            elif f.endswith("_sleeper_at_log.csv"):
                cloud, local = pd.read_csv(a), pd.read_csv(b)
                pd.concat([local[~local.key.isin(cloud.key)], cloud]).to_csv(b, index=False)
                merged += 1
    return new, merged


def main():
    pull()
    for name, dst in (("props", PROPS), ("kalshi", KALSHI)):
        src = os.path.join(REPO, name)
        if os.path.isdir(src):
            n, m = copy_tree(src, dst)
            print(f"{name}: {n} new file(s)" + (f", {m} Sleeper snapshot(s) merged" if m else ""))
    if os.path.isdir(os.path.join(REPO, "elway")):
        copy_elway(os.path.join(REPO, "elway"))


if __name__ == "__main__":
    main()
