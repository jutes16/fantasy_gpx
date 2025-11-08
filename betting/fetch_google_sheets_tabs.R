# fetch_google_sheets_tabs.R
suppressPackageStartupMessages({
  library(googlesheets4)
  library(janitor)
  library(dplyr)
  library(readr)
  library(glue)
  library(stringr)
})

# ========= CONFIG =========
sheet_url <- "https://docs.google.com/spreadsheets/d/1aZ1nFAx8YUrkzPniF1Kehj7pDeoZwxiYVElHHID-rN8"
base_dir  <- "/Users/jgilbert/Documents/GitHub/fantasy_gpx/betting"
out_dir   <- file.path(base_dir, "data")
out_stub  <- "google_sheet_pull"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Choose one of the following selection methods:
# 1) NULL means download all visible tabs
selected_tabs <- NULL
# 2) Character vector of tab names, for example:
selected_tabs <- c("Picks by Person", "GAMES")
# 3) Numeric vector of gid values, for example:
# selected_tabs <- c(0, 1844294633)

# If you passed gids, set this to "gid". Otherwise leave as "name".
select_by <- "name"   # "name" or "gid"

# Optional: use a regex to filter tab names when selected_tabs is NULL
# For example, only tabs containing "2025":
name_regex <- NULL     # e.g., "2025"

# ========= AUTH =========
sa_json <- Sys.getenv("GDRIVE_SERVICE_ACCOUNT_JSON", unset = "")
if (nzchar(sa_json) && file.exists(sa_json)) {
  gs4_auth(path = sa_json)
} else {
  gs4_auth(cache = TRUE)
}

# ========= DISCOVER TABS =========
props <- googlesheets4::sheet_properties(ss = sheet_url)

visible_props <- props %>% 
  # if you need to respect hidden tabs, uncomment next line once available in props
  # filter(!hidden)
  mutate(gid = as.integer(id)) %>% 
  select(name, gid, index)

message("Available tabs:")
print(visible_props)

# Compute the set of tabs to download
tabs_to_get <- NULL

if (is.null(selected_tabs)) {
  tabs_to_get <- visible_props
  if (!is.null(name_regex)) {
    tabs_to_get <- tabs_to_get %>% filter(str_detect(name, name_regex))
  }
} else if (select_by == "gid") {
  sel_gids <- as.integer(selected_tabs)
  tabs_to_get <- visible_props %>% filter(gid %in% sel_gids)
  if (nrow(tabs_to_get) == 0) stop("No matching gids found.")
} else {
  sel_names <- as.character(selected_tabs)
  tabs_to_get <- visible_props %>% filter(name %in% sel_names)
  if (nrow(tabs_to_get) == 0) stop("No matching tab names found.")
}

if (nrow(tabs_to_get) == 0) stop("No tabs selected to download.")

# ========= DOWNLOAD TABS =========
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
saved_files <- c()
dfs <- list()

for (i in seq_len(nrow(tabs_to_get))) {
  tab_name <- tabs_to_get$name[i]
  message("Reading tab: ", tab_name)

  df <- googlesheets4::read_sheet(ss = sheet_url, sheet = tab_name, .name_repair = "minimal") %>%
    janitor::clean_names()

  safe_tab <- gsub("[^A-Za-z0-9_]+", "_", tab_name)
  out_csv  <- file.path(out_dir, glue("{out_stub}_{safe_tab}_{ts}.csv"))

  readr::write_csv(df, out_csv)
  message("Saved: ", normalizePath(out_csv, mustWork = FALSE))

  saved_files <- c(saved_files, out_csv)
  dfs[[tab_name]] <- df
}

invisible(dfs)