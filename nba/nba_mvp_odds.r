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
invisible(lapply(required_packages, library, character.only = TRUE))

# --- Data Fetch Script ---
today <- Sys.Date()

if (!dir.exists("data")) dir.create("data")

# MVP_ODDS_URL overrides the source (used for testing)
url <- Sys.getenv("MVP_ODDS_URL",
                  "https://www.rotowire.com/betting/nba/tables/player-futures.php?future=MVP")

# Fetch, retrying a couple of times on network errors
result <- NULL
for (attempt in 1:3) {
  result <- tryCatch(jsonlite::fromJSON(txt = url), error = function(e) e)
  if (!inherits(result, "error")) break
  message("attempt ", attempt, " failed: ", conditionMessage(result))
  if (attempt < 3) Sys.sleep(10 * attempt)
}
if (inherits(result, "error")) {
  stop("could not fetch MVP odds after 3 attempts: ", conditionMessage(result))
}

# RotoWire returns an empty list ([]) when no MVP futures are posted, e.g.
# between seasons. That's not an error: skip today's file and keep the run
# green, with a visible warning in the Actions log.
if (!is.data.frame(result) || nrow(result) == 0) {
  if (is.list(result) && !is.null(result$error)) {
    stop("RotoWire returned an error: ", result$error)   # e.g. a bad FUTURE value
  }
  cat("::warning::No NBA MVP odds posted on RotoWire today; no file written.\n")
  quit(save = "no", status = 0)
}

df <- dplyr::mutate(result, date = today)

out <- paste0("data/mvp_odds_", gsub("-", "_", today), ".csv")
write.csv(df, out, row.names = FALSE)
message("wrote ", nrow(df), " players to ", out)
