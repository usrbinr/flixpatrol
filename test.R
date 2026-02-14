# flixpatrol — interactive testing
# Run devtools::document() and load_all() before using these.

library(devtools)
devtools::document()
devtools::load_all()
library(tidyverse)
# --- Package Options ---
flixpatrol_options()
options(flixpatrol.silent = TRUE)

# --- Lookup Functions ---
lookup_country("United States")
lookup_platform("Netflix")
lookup_flix_type("movies")

# --- Top 10 Rankings ---
get_top_ten(
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2026-02-14",
  end_date      = "2026-02-14",
  flix_type     = "movies"
)

# --- Hours Viewed (Netflix weekly data, must start on Monday) ---
get_hours_viewed(
  language_name   = "english",
  media_type_name = "movie",
  start_date      = "2026-02-10",
  end_date        = "2026-02-16"
)

# --- Official Rankings (Netflix only, weekly, must start on Monday) ---
get_official_ranking(
  country_name = "United States",
  start_date   = "2026-02-10",
  end_date     = "2026-02-16",
  media_type   = "movie"
)

# --- Fans Rankings (social media: instagram, twitter, facebook) ---
# Franchise names are automatically resolved via batch API lookup
get_fans_ranking(
  social_platform = "tiktok",
  start_date      = "2026-02-14",
  end_date        = "2026-02-14"
)
fans |> arrange(rank)

# Filter by type
fans |> filter(type == "title") |> arrange(rank)
fans |> filter(type == "franchise")

# --- Torrent Rankings (available sites: PirateBay, eztv, 1337x, etc.) ---
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

# --- Title History (track a title over time) ---
get_title_history(
  title         = "Squid Game",
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2026-02-01",
  end_date      = "2026-02-14",
  flix_type     = "tv"
)

# --- Top Titles Summary (most frequent Top 10 titles) ---
get_top_titles_summary(
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2026-02-01",
  end_date      = "2026-02-14",
  flix_type     = "movies",
  n             = 10
)
