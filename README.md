# flixpatrol

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

The `flixpatrol` package is an R interface for the [FlixPatrol API V2](https://flixpatrol.com/api/v2/). It allows for the programmatic retrieval of VOD rankings, streaming Top 10 lists, official viewing hour statistics, torrent charts, social media fan metrics, and premiere schedules across major global platforms.

## Features

- **Fuzzy Metadata Lookup**: Resolve country, platform, franchise, and title names into internal FlixPatrol IDs automatically.
- **Historical Data Retrieval**: Fetch daily Top 10 charts over custom date ranges.
- **Official Metrics**: Access "Hours Viewed" and "Official Ranking" tables as analysis-ready tibbles.
- **Torrent Rankings**: Pull torrent chart data by site and date range.
- **Social Media Fans**: Retrieve fan count rankings from Instagram, Facebook, and Twitter.
- **Premieres**: Get upcoming premiere schedules by platform and country.
- **Title Search & Details**: Search titles with fuzzy matching and retrieve full metadata.
- **Vectorized ID Resolution**: Resolve FlixPatrol IDs back to human-readable names in bulk, safe for use inside `dplyr::mutate()`.
- **Robust Validation**: Built-in checks for date ranges and API-specific weekday requirements (e.g., weekly charts starting on Mondays).
- **Rate Limiting & Retry**: All API calls are throttled (10 req/min) with automatic retry on transient failures.

---

## Installation

You can install the development version of `flixpatrol` from GitHub:

```r
# install.packages("pak")
pak::pak("git::https://codeberg.org/usrbinr/flixpatrol")
```

---

## Authentication

The package uses basic authentication via an API key stored in your R environment. Set it once:

```r
usethis::edit_r_environ()
# Add this line to the file that opens:
# FLIX_PATROL=your_api_key_here
```

Restart R after saving. All functions default to reading the `FLIX_PATROL` environment variable.

---

## Usage

### Lookup Functions

Translate human-readable names into FlixPatrol internal IDs:

```r
library(flixpatrol)

lookup_country_and_return_id("United States")
lookup_platform_and_return_id("Netflix")
lookup_franchise_and_return_id("Indiana Jones")
lookup_title_id("Squid Game")
```

Reverse lookups (ID to name):

```r
get_company_name("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX")
get_franchise_name("frn_KGBes9dtvkV710raDBpXoKRc")
get_title_name("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")
```

### Search Titles

Returns all matches as a tibble instead of just the first ID:

```r
search_titles("Squid Game")
```

### Daily Top 10

```r
# Single day
create_single_daily_top_ten_tbl(
  platform_name = "netflix",
  country_name  = "United States",
  date          = as.Date("2025-12-15"),
  flix_type     = "movies"
)

# Date range
create_daily_top_ten_tbl(
  platform_name = "netflix",
  country_name  = "United States",
  start_date    = "2025-12-01",
  end_date      = "2025-12-07",
  flix_type     = "movies"
)
```

### Hours Viewed

Requires a 7-day range starting on a Monday:

```r
create_hours_viewed_tbl(
  language_name   = "english",
  media_type_name = "movie",
  start_date      = "2025-12-15",
  end_date        = "2025-12-21"
)
```

### Official Rankings (Netflix Only)

```r
create_official_ranking_tbl(
  country_name = "United States",
  start_date   = "2025-12-15",
  end_date     = "2025-12-21",
  media_type   = "movie"
)
```

### Fan Rankings

```r
fetch_fans_ranking_tbl(
  country_name = "United States",
  start_date   = "2025-12-15",
  end_date     = "2025-12-21"
)
```

### Social Media Fans

```r
create_social_fans_tbl(
  social_platform = "Instagram",
  start_date      = "2025-12-15",
  end_date        = "2025-12-21"
)
```

### Torrent Rankings

```r
create_torrent_ranking_tbl(
  torrent_site = "1337x",
  start_date   = "2025-12-01",
  end_date     = "2025-12-01"
)
```

### Premieres

```r
create_premiere_tbl(
  start_date = "2025-12-15",
  end_date   = "2025-12-31"
)
```

### Title Details

```r
get_title_details("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")
```

### Bulk ID Resolution

Resolve a vector of mixed IDs inside a pipeline:

```r
library(dplyr)

my_data |>
  mutate(name = resolve_id(fp_id_column))
```

### Compare Platforms

See how a title ranks across multiple platforms on the same day:

```r
compare_platforms_tbl(
  title = "Squid Game",
  date  = "2025-12-15"
)
```

### Title History

Track a single title's rank over time:

```r
create_title_history_tbl(
  title      = "Squid Game",
  start_date = "2025-12-01",
  end_date   = "2025-12-14"
)
```

### Weekly Movers

Identify gainers, losers, new entries, and exits between two chart dates:

```r
create_weekly_movers_tbl(
  date_before = "2025-12-14",
  date_after  = "2025-12-15"
)
```

### Top Titles Summary

Most frequent Top 10 titles over a date range with best/average rank:

```r
create_top_titles_summary_tbl(
  start_date = "2025-12-01",
  end_date   = "2025-12-14",
  n          = 10
)
```

### Global Ranking

See where a title ranks across 15 countries:

```r
create_global_ranking_tbl(
  title = "Squid Game",
  date  = "2025-12-15"
)
```

### Franchise Performance

Aggregate all titles in a franchise over a date range:

```r
create_franchise_performance_tbl(
  franchise_title = "Marvel",
  start_date      = "2025-12-01",
  end_date        = "2025-12-14"
)
```

### Enrich a Tibble

Auto-detect a FlixPatrol ID column and append resolved names:

```r
my_tbl |> fp_enrich()
# or specify the column explicitly:
my_tbl |> fp_enrich(id_col = fp_id)
```

### Cache IDs

Populate the local lookup tables with real API-resolved IDs:

```r
fp_cache_ids(
  country_names  = c("United States", "United Kingdom", "Germany"),
  platform_names = c("Netflix", "Disney+", "HBO Max")
)
```

---

## Lookup Tables

The package ships with pre-built reference tables to avoid unnecessary API calls:

| Table | Contents |
|---|---|
| `flix_type_tbl` | Content types (movie, tv, anime, kids, docs, etc.) |
| `language_name` | Language options (english, non_english) |
| `media_type_name` | Media categories (movie, tv_show) |
| `date_type_name` | Date granularities (day, week, month, quarter, half_year, year) |
| `social_media_name` | Social platforms with company IDs |
| `torrent_sites` | Torrent site name-to-ID mappings |
| `platform_name` | Common streaming platforms with cached IDs |
| `country_name` | Common countries with cached IDs |

---

## Function Reference

### Lookup (Name to ID)

| Function | Endpoint |
|---|---|
| `lookup_country_and_return_id()` | `/countries` |
| `lookup_platform_and_return_id()` | `/companies` |
| `lookup_franchise_and_return_id()` | `/franchises` |
| `lookup_title_id()` | `/titles` |
| `lookup_flix_type_and_return_id()` | Local table |
| `lookup_language_name_and_return_id()` | Local table |
| `lookup_media_type_name_and_return_id()` | Local table |
| `lookup_date_type_name_and_return_id()` | Local table |
| `lookup_torrent_site_and_return_id()` | Local table |

### Reverse Lookup (ID to Name)

| Function | Endpoint |
|---|---|
| `get_company_name()` | `/companies/:id` |
| `get_franchise_name()` | `/franchises/:id` |
| `get_title_name()` | Auto-detects `cmp_`, `ttl_`, `frn_` prefixes |
| `resolve_id()` | Vectorized, NA-safe wrapper around `get_title_name()` |

### Data Retrieval

| Function | Endpoint | Description |
|---|---|---|
| `create_single_daily_top_ten_tbl()` | `/top10s` | Single-day Top 10 |
| `create_daily_top_ten_tbl()` | `/top10s` | Top 10 over a date range |
| `create_hours_viewed_tbl()` | `/hoursviewed` | Weekly viewing hours |
| `create_official_ranking_tbl()` | `/rankingsofficial` | Official Netflix rankings |
| `fetch_fans_ranking_tbl()` | `/fans` | Fan rankings by platform |
| `create_social_fans_tbl()` | `/fans` | Social media fan counts |
| `create_torrent_ranking_tbl()` | `/torrents` | Torrent chart rankings |
| `create_premiere_tbl()` | `/premieres` | Upcoming premiere dates |
| `search_titles()` | `/titles` | Fuzzy title search (all matches) |
| `get_title_details()` | `/titles/:id` | Full title metadata |

### Analytics & Comparison

| Function | Description |
|---|---|
| `compare_platforms_tbl()` | Compare a title's rank across multiple platforms on the same day |
| `create_title_history_tbl()` | Track a single title's rank over a date range |
| `create_weekly_movers_tbl()` | Gainers, losers, new entries, and exits between two chart dates |
| `create_top_titles_summary_tbl()` | Most frequent Top 10 titles with best/avg rank over a period |
| `create_global_ranking_tbl()` | A title's rank across 15+ countries on a single date |
| `create_franchise_performance_tbl()` | Aggregate chart performance for all titles in a franchise |

### Pipeline Helpers

| Function | Description |
|---|---|
| `fp_enrich()` | Auto-detect ID column in a tibble and append resolved names |
| `fp_cache_ids()` | Populate local lookup tables with API-resolved country/platform IDs |

### Validation

| Function | Purpose |
|---|---|
| `validate_date_range()` | Enforces weekday and range-length constraints |
