# flixpatrol — interactive testing
# Run devtools::document() and load_all() before using these.

library(devtools)
devtools::document()
devtools::load_all()
library(tidyverse)

# ============================================================================
# DIAGNOSTICS & OPTIONS
# ============================================================================

# Situation report (shows auth, quota, options)
flixpatrol_sitrep()

# View all package options
flixpatrol_options()

# Set options
options(flixpatrol.silent = TRUE)

# Get API quota
get_quota()

# ============================================================================
# LOOKUP FUNCTIONS (Name -> ID)
# ============================================================================

# Country lookup (returns named character vector)
lookup_country("United States")
lookup_country(c("United States", "Germany", "Japan"))

# Platform lookup (exact match required)
lookup_platform("Netflix")

# Franchise lookup
lookup_franchise("Star Wars")
lookup_franchise("Harry Potter")

# Title lookup
lookup_title("Squid Game")
lookup_title("Stranger Things")

# Flix type lookup (local table)
lookup_flix_type("movies")
lookup_flix_type("tv_shows")

# Language lookup (local table)
lookup_language("english")
lookup_language("non_english")

# Media type lookup (local table)
lookup_media_type("movie")
lookup_media_type("tv_show")

# Date type lookup (local table)
lookup_date_type("day")
lookup_date_type("week")

# Torrent site lookup (local table)
lookup_torrent_site("PirateBay")
lookup_torrent_site("1337x")

# ============================================================================
# REVERSE LOOKUP FUNCTIONS (ID -> Name)
# ============================================================================

# Get title name (auto-detects ID type by prefix)
get_title_name("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")  # title ID
get_title_name("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX")  # company ID (Netflix)

# Get company name
get_company_name("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX")

# Get franchise name
get_franchise_name("frn_yBGU2UzROTNVv5JZ7Hu4m8MY")  # Star Wars

# ============================================================================
# VALIDATION FUNCTIONS
# ============================================================================

# Validate date format (returns TRUE/FALSE)
validate_date_format("2026-02-14")
validate_date_format("02-14-2026")  # FALSE - wrong format

# Validate date range (returns TRUE or errors)
validate_date_range(
  start_date        = "2026-02-10",
  end_date          = "2026-02-16",
  start_date_wday_abb = "mon",
  range_length      = 7
)

# ============================================================================
# DATA RETRIEVAL FUNCTIONS
# ============================================================================

# --- Top 10 Rankings ---
get_top_ten(
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2026-02-10",
  end_date      = "2026-02-14",
  flix_type     = "movies"
)

# --- Hours Viewed (Netflix weekly, must start Monday) ---
get_hours_viewed(
  language_name   = "english",
  media_type_name = "movie",
  start_date      = "2026-02-03",
  end_date        = "2026-02-09"
)

# --- Official Rankings (Netflix weekly, must start Monday) ---
get_official_ranking(
  country_name = "United States",
  start_date   = "2026-02-03",
  end_date     = "2026-02-09",
  media_type   = "movie"
)

# --- Fans Rankings (social media) ---
get_fans_ranking(
  social_platform = "instagram",
  start_date      = "2026-02-14",
  end_date        = "2026-02-14"
)

# --- Torrent Rankings ---
get_torrent_ranking(
  torrent_site = "PirateBay",
  start_date   = "2026-02-14",
  end_date     = "2026-02-14"
)

# --- Premieres ---
get_premieres(
  platform_name = "netflix",
  start_date    = "2026-02-14",
  end_date      = "2026-02-28"
)

# --- Title Details ---
get_title_details("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")

# ============================================================================
# ANALYTICS FUNCTIONS
# ============================================================================

# --- Compare Platforms (same title across platforms) ---
compare_platforms(
  title         = "Bridgerton",
  country_name  = "United States",
  date          = "2026-02-14",
  flix_type     = "tv_shows"
)

# --- Title History (track rank over time) ---
get_title_history(
  title         = "The Lincoln Lawyer",
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2026-02-01",
  end_date      = "2026-02-14",
  flix_type     = "tv_shows"
)

# --- Weekly Movers (gainers, losers, new entries, exits) ---
get_weekly_movers(
  platform_name = "netflix",
  country_name  = "United States",
  date_before   = "2026-02-07",
  date_after    = "2026-02-14",
  flix_type     = "movies"
)

# --- Top Titles Summary (most frequent in Top 10) ---
get_top_titles_summary(
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2026-02-01",
  end_date      = "2026-02-14",
  flix_type     = "movies",
  n             = 10
)

# --- Global Ranking (same title across countries) ---
get_global_ranking(
  title         = "Bridgerton",
  platform_name = "netflix",
  date          = "2026-02-14",
  flix_type     = "tv_shows"
)

# --- Franchise Performance (all titles in franchise) ---
get_franchise_performance(
  franchise_title = "Star Wars",
  platform_name   = "netflix",
  country_name    = "United States",
  start_date      = "2026-01-01",
  end_date        = "2026-02-14",
  flix_type       = "movies"
)

# ============================================================================
# AUTHENTICATION (returns httr2 request object, not tibble)
# ============================================================================

# Create authenticated request object
req <- authenticate()
req
