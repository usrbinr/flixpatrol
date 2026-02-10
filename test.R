# flixpatrol — interactive testing & examples
# Run devtools::document() and load_all() before using these.

library(devtools)
devtools::document()
load_all()

# --- Package Options ---
flixpatrol_options()
options(flixpatrol.silent = TRUE)  # suppress messages globally

# --- Lookup Functions ---
lookup_country("United States")
lookup_platform("Netflix")
lookup_franchise("Indiana Jones")
lookup_title("Squid Game")

# --- Reverse Lookups (ID to Name) ---
get_title_name("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX")  # company
get_title_name("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")  # title
get_title_name(c("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX", "ttl_yPCJU2UzROTNVv5JZ7Hu4m8M"))  # vectorized

# --- Top 10 Rankings ---
get_top_ten(
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2025-12-01",
  end_date      = "2025-12-07",
  flix_type     = "movies"
)

# --- Hours Viewed ---
get_hours_viewed(
  language_name   = "english",
  media_type_name = "movie",
  start_date      = "2025-12-15",
  end_date        = "2025-12-21"
)

# --- Official Rankings (Netflix only) ---
get_official_ranking(
  country_name = "United States",
  start_date   = "2025-12-15",
  end_date     = "2025-12-21",
  media_type   = "movie"
)

# --- Torrent Rankings ---
get_torrent_ranking(
  torrent_site = "1337x",
  start_date   = "2025-12-01",
  end_date     = "2025-12-01"
)

# --- Social Fans ---
get_social_fans(
  social_platform = "Instagram",
  start_date      = "2025-12-15",
  end_date        = "2025-12-21"
)

# --- Premieres ---
get_premieres(
  start_date = "2025-12-15",
  end_date   = "2025-12-31"
)

# --- Title Details ---
get_title_details("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")

# --- Compare Platforms ---
# How does a title rank across Netflix, Disney+, etc. on the same day?
compare_platforms_tbl(
  title = "Squid Game",
  date  = "2025-12-15"
)

# --- Title History ---
# Track a single title's rank over time
get_title_history(
  title      = "Squid Game",
  start_date = "2025-12-01",
  end_date   = "2025-12-14"
)

# --- Weekly Movers ---
# Who gained, lost, entered, or exited the chart?
get_weekly_movers(
  date_before = "2025-12-14",
  date_after  = "2025-12-15"
)

# --- Top Titles Summary ---
# Most frequent Top 10 titles over a date range
get_top_titles_summary(
  start_date = "2025-12-01",
  end_date   = "2025-12-14",
  n          = 10
)

# --- Global Ranking ---
# Where does a title rank across 15 countries?
get_global_ranking(
  title = "Squid Game",
  date  = "2025-12-15"
)

# --- Franchise Performance ---
# Aggregate all Marvel titles on Netflix over a date range
get_franchise_performance(
  franchise_title = "Marvel",
  start_date      = "2025-12-01",
  end_date        = "2025-12-14"
)

# --- Vectorized ID resolution ---
# get_title_name works on vectors directly
get_title_name(c("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX", "ttl_yPCJU2UzROTNVv5JZ7Hu4m8M"))
