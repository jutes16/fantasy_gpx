"""
Copy the cloud snapshots (props, Sleeper projections, Kalshi) from the private
data repo into this checkout. Run before grading or `props.py sheet <wk> --cached`.

    python3 sync_data.py

The data repo is cloned next to this one (../../fantasy_gpx-data) on first use,
or set FANTASY_DATA_REPO. Files already here are kept, except the Sleeper
snapshots, which are merged with the cloud's rows winning (they were taken
closest to kickoff).
"""

import os
import shutil
import subprocess

import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.environ.get("FANTASY_DATA_REPO", os.path.join(HERE, "..", "..", "fantasy_gpx-data"))
PROPS = os.path.join(HERE, "..", "nfl_fantasy", "data", "props")
KALSHI = os.path.join(HERE, "data", "kalshi")


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


if __name__ == "__main__":
    main()
