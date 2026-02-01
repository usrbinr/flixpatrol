# flixpatrol — interactive testing & examples
# Run devtools::document() and load_all() before using these.

library(devtools)
devtools::document()
load_all()

# --- Lookup Examples ---
lookup_country_and_return_id("United States")
lookup_platform_and_return_id("netflix")
lookup_franchise_and_return_id("Indiana Jones")
lookup_title_id("Squid Game")

# --- Reverse Lookups ---
get_company_name("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX")
get_franchise_name("frn_KGBes9dtvkV710raDBpXoKRc")
get_title_name("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")

# --- Search Titles (returns all matches) ---
search_titles("Squid Game")

# --- Resolve IDs in bulk (vectorized, NA-safe) ---
resolve_id(c("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX", "frn_KGBes9dtvkV710raDBpXoKRc"))

# --- Daily Top 10 ---
create_single_daily_top_ten_tbl(
  platform_name = "netflix",
  country_name  = "United States",
  date          = as.Date("2025-12-15"),
  flix_type     = "movies"
)

create_daily_top_ten_tbl(
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2025-12-01",
  end_date      = "2025-12-07",
  flix_type     = "movies"
)

# --- Hours Viewed ---
create_hours_viewed_tbl(
  language_name   = "english",
  media_type_name = "movie",
  start_date      = "2025-12-15",
  end_date        = "2025-12-21"
)

# --- Official Rankings (Netflix only) ---
create_official_ranking_tbl(
  country_name = "United States",
  start_date   = "2025-12-15",
  end_date     = "2025-12-21",
  media_type   = "movie"
)

# --- Fans Rankings ---
fetch_fans_ranking_tbl(
  country_name = "United States",
  start_date   = "2025-12-15",
  end_date     = "2025-12-21"
)

# --- Social Fans ---
create_social_fans_tbl(
  social_platform = "Instagram",
  start_date      = "2025-12-15",
  end_date        = "2025-12-21"
)

# --- Torrent Rankings ---
torrent_id_vec <- lookup_torrent_site_and_return_id("*")

create_torrent_ranking_tbl(
  torrent_site = "1337x",
  start_date   = "2025-12-01",
  end_date     = "2025-12-01"
)

# --- Premieres ---
create_premiere_tbl(
  start_date = "2025-12-15",
  end_date   = "2025-12-31"
)

# --- Title Details ---
get_title_details("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")

# --- Franchise Lookup & Batch Resolution ---
franchise_tbl <- search_titles("Star Wars")
franchise_tbl

# --- Compare Platforms ---
# How does "Squid Game" rank across Netflix, Disney+, etc. on the same day?
compare_platforms_tbl(
  title    = "Squid Game",
  date     = "2025-12-15"
)

# --- Title History ---
# Track a single title's rank over time
create_title_history_tbl(
  title      = "Squid Game",
  start_date = "2025-12-01",
  end_date   = "2025-12-14"
)

# --- Weekly Movers ---
# Who gained, lost, entered, or exited the chart?
create_weekly_movers_tbl(
  date_before = "2025-12-14",
  date_after  = "2025-12-15"
)

# --- Top Titles Summary ---
# Most frequent Top 10 titles over a date range
create_top_titles_summary_tbl(
  start_date = "2025-12-01",
  end_date   = "2025-12-14",
  n          = 10
)

# --- Global Ranking ---
# Where does a title rank across 15 countries?
create_global_ranking_tbl(
  title = "Squid Game",
  date  = "2025-12-15"
)

# --- Franchise Performance ---
# Aggregate all Marvel titles on Netflix over a date range
create_franchise_performance_tbl(
  franchise_title = "Marvel",
  start_date      = "2025-12-01",
  end_date        = "2025-12-14"
)

# --- Enrich a tibble with resolved names ---
# tibble with FlixPatrol IDs gets a "name" column automatically
my_tbl <- tibble::tibble(fp_id = c("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX", "frn_KGBes9dtvkV710raDBpXoKRc"))
my_tbl |> fp_enrich()

# --- Cache IDs for fast lookups ---
# Hit the API once and save results to data/
fp_cache_ids(
  country_names  = c("United States", "United Kingdom", "Germany", "France", "Brazil"),
  platform_names = c("Netflix", "Disney+", "HBO Max", "Amazon Prime", "Apple TV+")
)
