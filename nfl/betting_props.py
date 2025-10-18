import requests

ODDS_API_KEY = "d0307f8198c755da36fe5b8a23aad7c9"

def get_sports():
    url = "https://api.the-odds-api.com/v4/sports"
    params = {"apiKey": ODDS_API_KEY}
    response = requests.get(url, params=params)
    response.raise_for_status()
    return response.json()

def get_odds(sport):
    url = f"https://api.the-odds-api.com/v4/sports/{sport}/odds"
    params = {
        "apiKey": ODDS_API_KEY,
        "regions": "us",
        "markets": "h2h",
        "oddsFormat": "american"
    }
    response = requests.get(url, params=params)
    response.raise_for_status()
    return response.json()

sports = get_sports()
print("Available sports:")
for sport in sports:
    print(f"{sport['key']}: {sport['title']}")

SPORT = "americanfootball_nfl"
odds = get_odds(SPORT)
print(f"\nOdds for {SPORT}:")
for game in odds:
    print(f"{game['home_team']} vs {game['away_team']}")
    for bookmaker in game['bookmakers']:
        print(f"  {bookmaker['title']}: {bookmaker['markets'][0]['outcomes']}")
