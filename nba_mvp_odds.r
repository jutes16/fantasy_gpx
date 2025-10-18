# --- Package Setup ---
required_packages <- c("dplyr", "jsonlite")

# Install any missing packages
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load the packages
lapply(required_packages, library, character.only = TRUE)

# --- Data Fetch Script ---
today <- Sys.Date()

if (!dir.exists("data")) dir.create("data")

url <- "https://www.rotowire.com/betting/nba/tables/player-futures.php?future=MVP"

result <- jsonlite::fromJSON(txt = url)

df <- dplyr::mutate(result, date = today)

write.csv(df, paste0("data/mvp_odds_", gsub("-", "_", today), ".csv"), row.names = FALSE)