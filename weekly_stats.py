# %%
import requests
import dtale
import pandas as pd
import difflib
# %%
def get_sleeper_weekly_stats(year=2024, week=1):
    url = f"https://api.sleeper.app/v1/stats/nfl/regular/{year}/{week}"
    response = requests.get(url)
    response.raise_for_status()
    data = response.json()
    df = pd.DataFrame.from_dict(data, orient='index')
    df.index.name = 'player_id'
    df = df.reset_index()
    return df

stats_df = get_sleeper_weekly_stats(2024, 6)
print(stats_df.head())
print(stats_df.columns)
# %%
def get_sleeper_players(active_only=True):
    url = "https://api.sleeper.app/v1/players/nfl"
    response = requests.get(url)
    response.raise_for_status()
    data = response.json()
    players_df = pd.DataFrame.from_dict(data, orient='index')
    if 'player_id' in players_df.columns:
        players_df = players_df.reset_index(drop=True)
    else:
        players_df.index.name = 'player_id'
        players_df = players_df.reset_index()
    # Filter out inactive players: keep only players where team and position are not null
    if active_only:
        players_df = players_df[players_df['team'].notnull() & players_df['position'].notnull()]
        print(f"Filtered players count: {len(players_df)}")
    return players_df[['player_id', 'full_name', 'position', 'team']]

players_df = get_sleeper_players()
stats_df = get_sleeper_weekly_stats(2025, 6)

merged = stats_df.merge(players_df, on='player_id', how='left')
print(merged[['full_name', 'position', 'team', 'pts_half_ppr']].head())

# %% Calculate lineup points

import string

def _normalize_name(name):
    # Lowercase, remove punctuation and spaces
    return ''.join(c for c in name.lower() if c not in string.punctuation and not c.isspace())

def calculate_lineup_points(lineup_names, year=2025, week=6, scoring_col='pts_half_ppr'):
    stats_df = get_sleeper_weekly_stats(year, week)
    players_df = get_sleeper_players(active_only=False)
    merged = players_df.merge(stats_df, on='player_id', how='left')
    merged[scoring_col] = merged[scoring_col].fillna(0)

    # Normalize names for matching
    normalized_lineup = [_normalize_name(n) for n in lineup_names]
    merged['normalized_full_name'] = merged['full_name'].apply(lambda n: _normalize_name(str(n)) if pd.notnull(n) else '')

    # Find matches: direct normalized matches
    mask = merged['normalized_full_name'].isin(normalized_lineup)
    lineup_df = merged[mask]

    # Fuzzy matching for missing players
    found_normalized = set(merged['normalized_full_name'])
    missing = []
    suggestions = {}
    for orig, norm in zip(lineup_names, normalized_lineup):
        if norm not in found_normalized:
            # Try fuzzy match
            close = difflib.get_close_matches(norm, list(found_normalized), n=2, cutoff=0.7)
            suggestion_names = []
            for c in close:
                # Find back the original full_name(s)
                suggestion_names.extend(merged.loc[merged['normalized_full_name']==c, 'full_name'].unique())
            suggestions[orig] = suggestion_names
            missing.append(orig)

    total = lineup_df[scoring_col].sum()
    print(lineup_df[['full_name', 'position', 'team', scoring_col]])
    print(f"\nTotal {scoring_col} points: {total:.2f}")
    if missing:
        print("⚠️ Not found:", missing)
        for m in missing:
            if suggestions[m]:
                print(f"  Suggestions for '{m}': {suggestions[m]}")
            else:
                print(f"  No suggestions for '{m}'.")
    return total

my_lineup = ["Jaxson Dart", "C.J. Stroud", "J.K. Dobbins"]
calculate_lineup_points(my_lineup, year=2025, week=5)

# %%
