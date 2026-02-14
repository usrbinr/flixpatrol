# Developer's Guide: flixpatrol

This guide is for developers maintaining or contributing to the flixpatrol R package.

## Package Overview

flixpatrol is an R interface for the [FlixPatrol API V2](https://flixpatrol.com/api2/). It provides functions to retrieve streaming platform rankings, viewing statistics, and metadata.

## Codebase Structure

```
flixpatrol/
├── R/                      # Source code
│   ├── hello.R             # Core functions (auth, lookups, data retrieval)
│   ├── utils-api.R         # API utility functions
│   └── utils-new.R         # Analytics and helper functions
├── data/                   # Pre-built lookup tables (.rda)
├── data-raw/               # Scripts to regenerate lookup tables
├── man/                    # Roxygen2-generated documentation
├── tests/testthat/         # Unit tests
├── docs/                   # Rendered website (qrtdown output)
├── reference/              # Quarto reference pages
├── index.qmd               # Package homepage
├── reference.qmd           # Function reference index
└── CLAUDE.md               # AI assistant instructions
```

## Function Naming Conventions

### Prefixes

| Prefix | Purpose | Example |
|--------|---------|---------|
| `get_*` | Fetch data from API | `get_top_ten()`, `get_premieres()` |
| `lookup_*` | Resolve names to IDs | `lookup_country()`, `lookup_platform()` |
| `fp_*` | Pipeline helpers | `fp_enrich()`, `fp_cache_ids()` |

### Guidelines

- Use `get_*` for API data retrieval, not `create_*` or `fetch_*`
- Use `lookup_*` for name-to-ID translation (returns character IDs)
- Avoid redundant suffixes like `_tbl` or `_and_return_id`
- All functions should be vectorized where possible

## API Integration Patterns

### Authentication

All API calls go through `authenticate()` which:
- Reads the API key from `Sys.getenv("FLIX_PATROL")`
- Uses httr2 basic auth
- Applies rate limiting (10 requests/minute)
- Enables automatic retry (3 attempts)

```r
authenticate(site = "https://api.flixpatrol.com/v2/endpoint")
```

### Query Parameters

The FlixPatrol API uses bracket notation for filters:

| Operator | Usage | Example |
|----------|-------|---------|
| `[eq]` | Exact match | `company[eq]=cmp_xxx` |
| `[in]` | Match any in list | `name[in]=Netflix` |
| `[nin]` | Not in list | `type[nin]=1` |
| `[gte]` | Greater than or equal | `premiere[gte]=2025-01-01` |
| `[lte]` | Less than or equal | `premiere[lte]=2025-12-31` |

**Important**: The API does NOT support `[like]` for fuzzy matching.

### Response Handling

Use `unnest_flixpatrol_response()` to flatten nested JSON:

```r
body <- httr2::resp_body_json(resp)
unnest_flixpatrol_response(body$data)
```

This helper conditionally unnests columns that exist, handling varying response structures.

## Adding New Functions

### 1. API Data Retrieval Function

```r
#' Get Something from API
#'
#' @description
#' Brief description of what it fetches.
#'
#' @param param1 Description.
#' @param param2 Description.
#'
#' @return A tibble with columns...
#' @export
get_something <- function(param1, param2) {

    # Resolve IDs if needed
    id <- lookup_platform(param1, silent = TRUE)

    # Build query
    resp <- authenticate(site = "https://api.flixpatrol.com/v2/endpoint") |>
        httr2::req_url_query(
            `param[eq]` = id,
            `date[gte]` = as.character(start_date)
        ) |>
        httr2::req_perform()

    body <- httr2::resp_body_json(resp)

    # Handle empty response
    if (length(body$data) == 0) {
        cli::cli_alert_warning("No data returned.")
        return(tibble::tibble())
    }

    unnest_flixpatrol_response(body$data)
}
```

### 2. Lookup Function

```r
#' Lookup Something
#'
#' @param name Character vector of names to look up.
#' @param silent Logical. Suppress success messages.
#'
#' @return Character vector of IDs.
#' @export
lookup_something <- function(name, silent = getOption("flixpatrol.silent", FALSE)) {

    token <- Sys.getenv("FLIX_PATROL")

    find_single_id <- function(n) {
        resp <- authenticate(site = "https://api.flixpatrol.com/v2/endpoint") |>
            httr2::req_url_query(`name[in]` = n) |>
            httr2::req_perform()

        body <- httr2::resp_body_json(resp)

        if (length(body$data) == 0) {
            cli::cli_abort("Not found: {.val {n}}")
        }

        id <- body$data[[1]]$data$id

        if (!silent) {
            cli::cli_alert_success("Found: {.val {n}} ({.val {id}})")
        }

        id
    }

    purrr::map_chr(name, find_single_id)
}
```

## Lookup Tables

### Static Tables (in `data/`)

Used for values that don't change:
- `flix_type_tbl` - Content types
- `language_name` - Languages
- `media_type_name` - Media types
- `date_type_name` - Date granularities
- `torrent_sites` - Torrent site mappings
- `social_media_name` - Social platform IDs

### Regenerating Tables

Scripts in `data-raw/` generate the `.rda` files:

```r
source("data-raw/torrent.R")  # Fetches from API
source("data-raw/type.R")     # Hardcoded values
```

## Package Options

Users can set global options:

```r
options(flixpatrol.silent = TRUE)  # Suppress lookup messages
```

Add new options to `flixpatrol_options()` in `hello.R`.

## Testing

### Running Tests

```r
devtools::test()
```

### Test Structure

- Tests use `testthat` edition 3
- API tests use `skip_if_no_api_key()` to skip when no key is set
- Use `withr::local_envvar()` to mock environment variables

### Writing Tests

```r
describe("get_something()", {

    it("returns a tibble for valid input", {
        skip_if_no_api_key()
        result <- get_something("value")
        expect_s3_class(result, "tbl_df")
    })

    it("handles empty results gracefully", {
        skip_if_no_api_key()
        result <- get_something("nonexistent")
        expect_equal(nrow(result), 0)
    })
})
```

## Documentation

### Roxygen2

All exported functions must have:
- `@description` - What it does
- `@param` - All parameters documented
- `@return` - Return value description
- `@export` - To export the function
- `@examples` - Wrapped in `\dontrun{}` for API calls

### Quarto Website

1. Update `index.qmd` for homepage content
2. Update `reference.qmd` for function index
3. Individual function pages go in `reference/`

### Rendering

```r
qrtdown::render_qrtdown()
```

Or from terminal:
```bash
quarto render
```

## Error Handling

Use the `cli` package for user-facing messages:

```r
# Errors
cli::cli_abort("Message with {.val {value}}")

# Warnings
cli::cli_alert_warning("Warning message")

# Success
cli::cli_alert_success("Found: {.val {name}}")

# Info
cli::cli_alert_info("Processing {length(x)} items")
```

## Dependencies

### Required (Imports)

- `cli` - User messaging
- `dplyr` - Data manipulation
- `httr2` - HTTP requests
- `janitor` - Data cleaning
- `lubridate` - Date handling
- `purrr` - Functional programming
- `rlang` - Tidy evaluation
- `tibble` - Data frames
- `tidyr` - Reshaping

### Testing (Suggests)

- `testthat` (>= 3.0.0)
- `withr`

## API Reference

FlixPatrol API documentation: https://flixpatrol.com/api2/

Key endpoints:
- `/top10s` - Daily Top 10 charts
- `/hoursviewed` - Viewing hours
- `/rankingsofficial` - Official Netflix rankings
- `/premieres` - Upcoming releases
- `/torrents` - Torrent charts
- `/fans` - Social media fans
- `/titles` - Title metadata
- `/companies` - Platform/company data
- `/countries` - Country data
- `/franchises` - Franchise data

## Release Checklist

1. Update version in DESCRIPTION using `qrtdown::use_qrtdown_version()`
2. Run `devtools::check(remote = TRUE, manual = TRUE)`
3. Run `devtools::test()`
4. Update documentation: `devtools::document()`
5. Render website: `qrtdown::render_qrtdown()`
6. Commit and push to Codeberg
