# %%
import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup, Comment
import os
import time
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import pearsonr

def get_bbref_advanced(season):
    cache_path = f"data/bbref_adv_{season}.parquet"
    if os.path.exists(cache_path):
        df = pd.read_parquet(cache_path)
        return df

    url = f"https://www.basketball-reference.com/leagues/NBA_{season}_advanced.html"
    headers = {
        "User-Agent": "Mozilla/5.0 (compatible; FantasyGPXBot/1.0; +https://github.com/jgilbert/fantasy_gpx)"
    }
    response = requests.get(url, headers=headers)
    response.raise_for_status()
    soup = BeautifulSoup(response.text, 'html.parser')

    # Attempt 1: Find table by id containing 'advanced'
    table = None
    for tbl in soup.find_all('table'):
        tbl_id = tbl.get('id', '')
        caption = tbl.find('caption')
        caption_text = caption.text.strip().lower() if caption else ''
        if 'advanced' in tbl_id.lower() or 'advanced' in caption_text:
            table = tbl
            print("Found advanced stats table directly in HTML.")
            break

    # Attempt 2: If not found, search in comments
    if table is None:
        comments = soup.find_all(string=lambda text: isinstance(text, Comment))
        for comment in comments:
            comment_soup = BeautifulSoup(comment, 'html.parser')
            for tbl in comment_soup.find_all('table'):
                # Check if table html contains 'Advanced' or columns like 'Player' and 'BPM'
                tbl_html_lower = str(tbl).lower()
                if ('advanced' in tbl_html_lower) or \
                   (tbl.find('th', string=lambda x: x and 'player' in x.lower()) and tbl.find('th', string=lambda x: x and 'bpm' in x.lower())):
                    table = tbl
                    print("Found advanced stats table inside HTML comment.")
                    break
            if table is not None:
                break

    if table is None:
        raise ValueError("Advanced stats table not found on the page")

    df = pd.read_html(str(table))[0]

    # Clean column names
    df.columns = [col.lower().replace(' ', '_') for col in df.columns]

    # Keep only relevant columns
    keep_cols = ['player', 'tm', 'bpm', 'vorp']
    df = df[keep_cols].copy()
    df.columns = ['player_name', 'team', 'bpm', 'vorp']

    # Drop rows where team is 'TOT'
    df = df[df['team'] != 'TOT']

    # Coerce bpm and vorp to numeric
    df['bpm'] = pd.to_numeric(df['bpm'], errors='coerce')
    df['vorp'] = pd.to_numeric(df['vorp'], errors='coerce')

    # Standardize team abbreviations
    team_map = {
        'BRK': 'BKN',
        'CHO': 'CHA',
        'NJN': 'BKN',
        'NOH': 'NOP',
        'PHO': 'PHX',
        'SAH': 'SAS',
        'UTH': 'UTA',
        'WSH': 'WAS'
    }
    df['team'] = df['team'].replace(team_map)

    # Drop rows with missing bpm or vorp
    df = df.dropna(subset=['bpm', 'vorp'])

    # Save to parquet cache
    os.makedirs('data', exist_ok=True)
    df.to_parquet(cache_path, index=False)

    # Polite sleep
    time.sleep(3)

    return df

def main(rating_type='bpm'):
    season = 2025

    # Step 1: Get player ratings (BPM or VORP)
    adv_stats = get_bbref_advanced(season)
    adv_stats = adv_stats[['player_name', 'team', rating_type]].copy()
    adv_stats = adv_stats.dropna(subset=[rating_type])

    # Step 2: Get NBA game data
    from basketball_reference_scraper.seasons import get_schedule, get_standings
    schedule = get_schedule(season)
    standings = get_standings(season)
    # We will use schedule for game data: home_team, away_team, home_score, away_score
    games = schedule[['HOME', 'VISITOR', 'HOME_PTS', 'VISITOR_PTS']].copy()
    games.columns = ['home_team', 'away_team', 'home_score', 'away_score']

    # Step 3: Compute strongest and weakest link differences
    # For each team, determine top 5 and bottom 5 players by chosen rating_type
    top_players = adv_stats.groupby('team').apply(lambda x: x.nlargest(5, rating_type)).reset_index(drop=True)
    bottom_players = adv_stats.groupby('team').apply(lambda x: x.nsmallest(5, rating_type)).reset_index(drop=True)

    # Compute mean rating for top and bottom players per team
    top_mean = top_players.groupby('team')[rating_type].mean()
    bottom_mean = bottom_players.groupby('team')[rating_type].mean()

    # Prepare lists to store results
    strong_diffs = []
    weak_diffs = []
    score_diffs = []

    # For each game, calculate differences
    for idx, row in games.iterrows():
        home = row['home_team']
        away = row['away_team']
        home_score = row['home_score']
        away_score = row['away_score']

        if home not in top_mean.index or away not in top_mean.index:
            continue
        if home not in bottom_mean.index or away not in bottom_mean.index:
            continue

        strong_diff = top_mean[home] - top_mean[away]
        weak_diff = bottom_mean[home] - bottom_mean[away]
        score_diff = home_score - away_score

        strong_diffs.append(strong_diff)
        weak_diffs.append(weak_diff)
        score_diffs.append(score_diff)

    df = pd.DataFrame({
        'strong_diff': strong_diffs,
        'weak_diff': weak_diffs,
        'score_diff': score_diffs
    })

    # Calculate correlation coefficients
    r_strong, p_strong = pearsonr(df['score_diff'], df['strong_diff'])
    r_weak, p_weak = pearsonr(df['score_diff'], df['weak_diff'])

    print(f"Correlation between score difference and strong link {rating_type.upper()} difference:")
    print(f"r = {r_strong:.3f}, p-value = {p_strong:.3e}")
    print()
    print(f"Correlation between score difference and weak link {rating_type.upper()} difference:")
    print(f"r = {r_weak:.3f}, p-value = {p_weak:.3e}")

    # Plot strong link vs score difference
    plt.figure(figsize=(8,6))
    sns.regplot(x='strong_diff', y='score_diff', data=df, scatter_kws={'alpha':0.5})
    plt.title(f'Strong Link {rating_type.upper()} Difference vs Score Difference')
    plt.xlabel(f'Strong Link {rating_type.upper()} Difference (Home - Away)')
    plt.ylabel('Score Difference (Home - Away)')
    plt.tight_layout()
    plt.savefig(f'strong_link_vs_score_{rating_type}.png')
    plt.close()

    # Plot weak link vs score difference
    plt.figure(figsize=(8,6))
    sns.regplot(x='weak_diff', y='score_diff', data=df, scatter_kws={'alpha':0.5})
    plt.title(f'Weak Link {rating_type.upper()} Difference vs Score Difference')
    plt.xlabel(f'Weak Link {rating_type.upper()} Difference (Home - Away)')
    plt.ylabel('Score Difference (Home - Away)')
    plt.tight_layout()
    plt.savefig(f'weak_link_vs_score_{rating_type}.png')
    plt.close()
# %% 
if __name__ == "__main__":
    main()

# %%
