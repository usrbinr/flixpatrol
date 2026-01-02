# flixpatrol

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
The `flixpatrol` package is an R interface for the [FlixPatrol API V2](https://flixpatrol.com/api/v2/). It allows for the programmatic retrieval of VOD rankings, streaming Top 10 lists, and official viewing hour statistics across major global platforms.

## 🚀 Features

* **Fuzzy Metadata Lookup**: Resolve country and platform names (e.g., "Netflix", "USA") into internal FlixPatrol IDs automatically.
* **Historical Data Scraping**: Fetch daily charts over custom date ranges.
* **Official Metrics**: Access "Hours Viewed" and "Official Ranking" tables directly into R data frames (tibbles).
* **Robust Validation**: Built-in checks for date ranges and API-specific weekday requirements (e.g., weekly charts starting on Mondays).

---

## 📦 Installation

You can install the development version of `flixpatrol` from GitHub:

```r
# install.packages("pak")
pak::pak("git::https://codeberg.org/usrbinr/flixpatrol")
