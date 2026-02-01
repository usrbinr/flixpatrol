#' Create Torrent Ranking Table
#'
#' @description
#' Fetches torrent chart rankings for a given site and date range.
#'
#' @param token Character. API environment variable name.
#' @param torrent_site Character. Torrent site name (e.g., "1337x") or `"*"` for all.
#' @param start_date Date/Character. Start of the date range.
#' @param end_date Date/Character. End of the date range.
#' @param date_type Character. Date granularity (default `"day"`).
#' @param media_type Character. Optional media type filter.
#'
#' @return A tibble of torrent rankings.
#' @export
create_torrent_ranking_tbl <- function(token = "FLIX_PATROL",
                                       torrent_site,
                                       start_date,
                                       end_date,
                                       date_type = "day",
                                       media_type = NULL) {

  site_ids     <- lookup_torrent_site_and_return_id(torrent_site)
  date_type_id <- lookup_date_type_name_and_return_id(date_type)

  fetch_single_site <- function(site_id) {
    query_params <- list(
      `site[eq]`       = site_id,
      `date[type][eq]` = date_type_id,
      `date[from][eq]` = as.character(start_date),
      `date[to][eq]`   = as.character(end_date)
    )

    if (!is.null(media_type)) {
      media_type_id <- lookup_media_type_name_and_return_id(media_type)
      query_params[["type[eq]"]] <- media_type_id
    }

    resp <- authenticate(site = "https://api.flixpatrol.com/v2/torrents", token = token) |>
      httr2::req_url_query(!!!query_params) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(resp)

    if (length(body$data) == 0) {
      return(tibble::tibble())
    }

    unnest_flixpatrol_response(body$data)
  }

  out <- purrr::map(site_ids, fetch_single_site) |>
    purrr::list_rbind()

  if (nrow(out) == 0) {
    cli::cli_alert_warning("No torrent data returned for these parameters.")
  }

  return(out)
}


#' Create Social Fans Table
#'
#' @description
#' Fetches social media fan count data using the social media platform lookup table.
#'
#' @param token Character. API environment variable name.
#' @param social_platform Character. Social media platform name
#'   (e.g., "Instagram", "Facebook", "Twitter").
#' @param start_date Date/Character. Start date.
#' @param end_date Date/Character. End date.
#' @param date_type Character. Date granularity (default `"day"`).
#'
#' @return A tibble of social fan rankings.
#' @export
create_social_fans_tbl <- function(token = "FLIX_PATROL",
                                   social_platform,
                                   start_date,
                                   end_date,
                                   date_type = "day") {

  platform_lower <- tolower(social_platform)
  tbl <- flixpatrol::social_media_name
  valid <- tolower(tbl$social_media_name)

  if (!all(platform_lower %in% valid)) {
    cli::cli_abort("{.arg social_platform} must be one of {.val {tbl$social_media_name}}, not {.val {social_platform}}.")
  }

  social_id    <- tbl$social_media_id[tolower(tbl$social_media_name) %in% platform_lower]
  date_type_id <- lookup_date_type_name_and_return_id(date_type)

  resp <- authenticate(site = "https://api.flixpatrol.com/v2/fans", token = token) |>
    httr2::req_url_query(
      `company[eq]`    = social_id,
      `date[type][eq]` = date_type_id,
      `date[from][eq]` = as.character(start_date),
      `date[to][eq]`   = as.character(end_date)
    ) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)

  if (length(body$data) == 0) {
    cli::cli_alert_warning("No social fan data returned for these parameters.")
    return(tibble::tibble())
  }

  unnest_flixpatrol_response(body$data)
}


#' Create Premiere Table
#'
#' @description
#' Fetches upcoming premiere dates from the FlixPatrol API.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. Platform name (e.g., "netflix").
#' @param country_name Character. Country name.
#' @param start_date Date/Character. Start of the date range.
#' @param end_date Date/Character. End of the date range.
#'
#' @return A tibble of upcoming premieres.
#' @export
create_premiere_tbl <- function(token = "FLIX_PATROL",
                                platform_name = "netflix",
                                country_name = "United States",
                                start_date,
                                end_date) {

  platform_id <- lookup_platform_and_return_id(platform_name, token = token, silent = TRUE)
  country_id  <- lookup_country_and_return_id(country_name, token = token, silent = TRUE)

  resp <- authenticate(site = "https://api.flixpatrol.com/v2/premieres", token = token) |>
    httr2::req_url_query(
      `company[eq]`    = platform_id,
      `country[eq]`    = country_id,
      `date[from][eq]` = as.character(start_date),
      `date[to][eq]`   = as.character(end_date)
    ) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)

  if (length(body$data) == 0) {
    cli::cli_alert_warning("No premiere data returned for these parameters.")
    return(tibble::tibble())
  }

  unnest_flixpatrol_response(body$data)
}


#' Get Title Details
#'
#' @description
#' Retrieves full metadata for a single title by its FlixPatrol ID, including
#' runtime, genre, release date, IMDB rating, and other details.
#'
#' @param title_id Character. The FlixPatrol title ID (e.g., `"ttl_..."`).
#' @param token Character. API environment variable name.
#'
#' @return A named list of title metadata.
#' @export
get_title_details <- function(title_id, token = "FLIX_PATROL") {

  if (missing(title_id) || !is.character(title_id)) {
    cli::cli_abort("{.arg title_id} must be a character string.")
  }

  endpoint_url <- paste0("https://api.flixpatrol.com/v2/titles/", title_id)

  resp <- authenticate(site = endpoint_url, token = token) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)
  data <- purrr::pluck(body, "data", .default = NULL)

  if (is.null(data)) {
    cli::cli_abort("No data found for title ID: {.val {title_id}}.")
  }

  return(data)
}


#' Search Titles
#'
#' @description
#' Searches for titles matching a query string and returns all matches as a
#' tibble, rather than just the first match ID.
#'
#' @param query Character. The search string (supports fuzzy matching).
#' @param token Character. API environment variable name.
#'
#' @return A tibble with columns `id`, `title`, and any other fields returned.
#' @export
search_titles <- function(query, token = "FLIX_PATROL") {

  resp <- authenticate(site = "https://api.flixpatrol.com/v2/titles", token = token) |>
    httr2::req_url_query(`title[like]` = query) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)

  if (length(body$data) == 0) {
    cli::cli_alert_warning("No titles found matching: {.val {query}}.")
    return(tibble::tibble())
  }

  purrr::map_dfr(body$data, function(item) {
    d <- item$data
    tibble::tibble(
      id    = purrr::pluck(d, "id", .default = NA_character_),
      title = purrr::pluck(d, "title", .default = NA_character_)
    )
  })
}


#' Resolve FlixPatrol IDs to Names (Vectorized)
#'
#' @description
#' Vectorized version of [get_title_name()] that works inside `dplyr::mutate()`.
#' Uses `purrr::map_chr()` internally and wraps errors with `purrr::possibly()`
#' to return `NA` for any ID that cannot be resolved.
#'
#' @param fp_ids Character vector. FlixPatrol IDs (can be a mix of `cmp_`, `ttl_`, `frn_` prefixes).
#' @param token Character. API environment variable name.
#'
#' @return A character vector of resolved names, with `NA` for failures.
#' @export
resolve_id <- function(fp_ids, token = "FLIX_PATROL") {
  safe_get <- purrr::possibly(get_title_name, otherwise = NA_character_)
  purrr::map_chr(fp_ids, \(id) safe_get(fp_id = id, token = token))
}


#' Compare a Title's Ranking Across Platforms
#'
#' @description
#' Given a title, fetches its Top 10 ranking on multiple platforms for the same
#' date, returning a single tibble for easy side-by-side comparison.
#'
#' @param title Character. The title to search for in each platform's chart.
#' @param platforms Character vector. Platform names to compare
#'   (default: Netflix, Disney+, HBO Max, Amazon Prime, Apple TV+).
#' @param country_name Character. Country name.
#' @param date Date/Character. The chart date.
#' @param flix_type Character. Content type (default `"movies"`).
#' @param token Character. API environment variable name.
#'
#' @return A tibble with columns `platform`, `rank`, `title`, `date`, and chart metadata.
#'   Platforms where the title does not appear return a row with `rank = NA`.
#' @export
compare_platforms_tbl <- function(title,
                                  platforms = c("netflix", "disney+", "hbo max", "amazon prime", "apple tv+"),
                                  country_name = "United States",
                                  date,
                                  flix_type = "movies",
                                  token = "FLIX_PATROL") {

  safe_fetch <- purrr::possibly(create_single_daily_top_ten_tbl, otherwise = tibble::tibble())

  out <- purrr::map(platforms, function(plat) {
    chart <- safe_fetch(
      token         = token,
      platform_name = plat,
      country_name  = country_name,
      date          = as.Date(date),
      flix_type     = flix_type
    )

    if (nrow(chart) == 0) {
      return(tibble::tibble(
        platform = plat,
        rank     = NA_integer_,
        title    = title,
        date     = as.character(date)
      ))
    }

    # Case-insensitive match
    match <- chart |>
      dplyr::filter(tolower(.data$title) == tolower(.env$title))

    if (nrow(match) == 0) {
      return(tibble::tibble(
        platform = plat,
        rank     = NA_integer_,
        title    = title,
        date     = as.character(date)
      ))
    }

    match |>
      dplyr::mutate(platform = plat) |>
      dplyr::select("platform", dplyr::everything())
  }) |>
    purrr::list_rbind()

  return(out)
}


#' Track a Single Title's Rank Over Time
#'
#' @description
#' Fetches the daily Top 10 for a date range and filters to a single title,
#' returning a time series of that title's rank, days on chart, and position
#' changes.
#'
#' @param title Character. The title to track.
#' @param platform_name Character. Platform name.
#' @param country_name Character. Country name.
#' @param start_date Date/Character. Start of the range.
#' @param end_date Date/Character. End of the range.
#' @param flix_type Character. Content type (default `"movies"`).
#' @param token Character. API environment variable name.
#'
#' @return A tibble with one row per date the title appeared in the Top 10.
#' @export
create_title_history_tbl <- function(title,
                                     platform_name = "netflix",
                                     country_name = "United States",
                                     start_date,
                                     end_date,
                                     flix_type = "movies",
                                     token = "FLIX_PATROL") {

  full_chart <- create_daily_top_ten_tbl(
    token         = token,
    platform_name = platform_name,
    country_name  = country_name,
    start_date    = start_date,
    end_date      = end_date,
    flix_type     = flix_type
  )

  if (nrow(full_chart) == 0) {
    cli::cli_alert_warning("No chart data returned for the given parameters.")
    return(tibble::tibble())
  }

  out <- full_chart |>
    dplyr::filter(tolower(.data$title) == tolower(.env$title))

  if (nrow(out) == 0) {
    cli::cli_alert_warning("{.val {title}} did not appear in the Top 10 during this period.")
  }

  return(out)
}


#' Weekly Movers: Gainers, Losers, New Entries, and Exits
#'
#' @description
#' Compares two dates (e.g., consecutive days or weeks) and classifies each
#' title as a gainer, loser, new entry, or exit.
#'
#' @param platform_name Character. Platform name.
#' @param country_name Character. Country name.
#' @param date_before Date/Character. The earlier chart date.
#' @param date_after Date/Character. The later chart date.
#' @param flix_type Character. Content type (default `"movies"`).
#' @param token Character. API environment variable name.
#'
#' @return A tibble with columns `title`, `rank_before`, `rank_after`, `change`,
#'   and `status` (one of "new_entry", "exit", "gainer", "loser", "unchanged").
#' @export
create_weekly_movers_tbl <- function(platform_name = "netflix",
                                     country_name = "United States",
                                     date_before,
                                     date_after,
                                     flix_type = "movies",
                                     token = "FLIX_PATROL") {

  before <- create_single_daily_top_ten_tbl(
    token = token, platform_name = platform_name,
    country_name = country_name, date = as.Date(date_before), flix_type = flix_type
  ) |>
    dplyr::select(title, rank_before = "rank")

  after <- create_single_daily_top_ten_tbl(
    token = token, platform_name = platform_name,
    country_name = country_name, date = as.Date(date_after), flix_type = flix_type
  ) |>
    dplyr::select(title, rank_after = "rank")

  out <- dplyr::full_join(before, after, by = "title") |>
    dplyr::mutate(
      change = .data$rank_before - .data$rank_after,
      status = dplyr::case_when(
        is.na(.data$rank_before) ~ "new_entry",
        is.na(.data$rank_after)  ~ "exit",
        .data$change > 0         ~ "gainer",
        .data$change < 0         ~ "loser",
        TRUE                     ~ "unchanged"
      )
    ) |>
    dplyr::arrange(.data$status, dplyr::desc(.data$change))

  return(out)
}


#' Summarize Top Titles Over a Date Range
#'
#' @description
#' Aggregates daily Top 10 data over a date range and returns a summary of
#' which titles appeared most frequently, along with their best rank and
#' average rank.
#'
#' @param platform_name Character. Platform name.
#' @param country_name Character. Country name.
#' @param start_date Date/Character. Start of the range.
#' @param end_date Date/Character. End of the range.
#' @param flix_type Character. Content type (default `"movies"`).
#' @param n Integer. Number of top titles to return (default 20).
#' @param token Character. API environment variable name.
#'
#' @return A tibble with columns `title`, `days_on_chart`, `best_rank`,
#'   `avg_rank`, `first_seen`, `last_seen`.
#' @export
create_top_titles_summary_tbl <- function(platform_name = "netflix",
                                          country_name = "United States",
                                          start_date,
                                          end_date,
                                          flix_type = "movies",
                                          n = 20,
                                          token = "FLIX_PATROL") {

  full_chart <- create_daily_top_ten_tbl(
    token         = token,
    platform_name = platform_name,
    country_name  = country_name,
    start_date    = start_date,
    end_date      = end_date,
    flix_type     = flix_type
  )

  if (nrow(full_chart) == 0) {
    cli::cli_alert_warning("No chart data returned.")
    return(tibble::tibble())
  }

  out <- full_chart |>
    dplyr::group_by(.data$title) |>
    dplyr::summarise(
      days_on_chart = dplyr::n(),
      best_rank     = min(.data$rank, na.rm = TRUE),
      avg_rank      = round(mean(.data$rank, na.rm = TRUE), 1),
      first_seen    = min(.data$date, na.rm = TRUE),
      last_seen     = max(.data$date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$days_on_chart), .data$best_rank) |>
    utils::head(n)

  return(out)
}


#' Global Ranking for a Title Across Countries
#'
#' @description
#' For a given title and date, fetches its Top 10 rank in every country provided,
#' showing geographic reach and relative performance.
#'
#' @param title Character. The title to look up.
#' @param platform_name Character. Platform name (default `"netflix"`).
#' @param date Date/Character. The chart date.
#' @param countries Character vector. Country names to check. Defaults to a broad
#'   set of 15 major markets.
#' @param flix_type Character. Content type (default `"movies"`).
#' @param token Character. API environment variable name.
#'
#' @return A tibble with columns `country`, `rank`, `title`, `date`.
#'   Countries where the title is not in the Top 10 return `rank = NA`.
#' @export
create_global_ranking_tbl <- function(title,
                                      platform_name = "netflix",
                                      date,
                                      countries = c(
                                        "United States", "United Kingdom", "Canada",
                                        "Australia", "Germany", "France", "Brazil",
                                        "India", "Japan", "South Korea", "Mexico",
                                        "Spain", "Italy", "Netherlands", "Sweden"
                                      ),
                                      flix_type = "movies",
                                      token = "FLIX_PATROL") {

  safe_fetch <- purrr::possibly(create_single_daily_top_ten_tbl, otherwise = tibble::tibble())

  out <- purrr::map(countries, function(cty) {
    chart <- safe_fetch(
      token         = token,
      platform_name = platform_name,
      country_name  = cty,
      date          = as.Date(date),
      flix_type     = flix_type
    )

    if (nrow(chart) == 0) {
      return(tibble::tibble(country = cty, rank = NA_integer_, title = title, date = as.character(date)))
    }

    match <- chart |>
      dplyr::filter(tolower(.data$title) == tolower(.env$title))

    if (nrow(match) == 0) {
      return(tibble::tibble(country = cty, rank = NA_integer_, title = title, date = as.character(date)))
    }

    match |>
      dplyr::mutate(country = cty) |>
      dplyr::select("country", dplyr::everything())
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$rank)

  return(out)
}


#' Franchise Performance Rollup
#'
#' @description
#' Looks up all titles in a franchise, then aggregates their Top 10 appearances
#' across a date range into a single summary table.
#'
#' @param franchise_title Character. The franchise name (e.g., "Marvel", "Star Wars").
#' @param platform_name Character. Platform name.
#' @param country_name Character. Country name.
#' @param start_date Date/Character. Start of the range.
#' @param end_date Date/Character. End of the range.
#' @param flix_type Character. Content type (default `"movies"`).
#' @param token Character. API environment variable name.
#'
#' @return A tibble with columns `title`, `days_on_chart`, `best_rank`, `avg_rank`,
#'   plus a `franchise` column.
#' @export
create_franchise_performance_tbl <- function(franchise_title,
                                             platform_name = "netflix",
                                             country_name = "United States",
                                             start_date,
                                             end_date,
                                             flix_type = "movies",
                                             token = "FLIX_PATROL") {

  # Look up the franchise to get its ID
  franchise_id <- lookup_franchise_and_return_id(franchise_title, token = token, silent = TRUE)

  # Get the franchise details to find associated title names
  endpoint_url <- paste0("https://api.flixpatrol.com/v2/franchises/", franchise_id)
  resp <- authenticate(site = endpoint_url, token = token) |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp)

  # Extract titles from the franchise response
  titles_data <- purrr::pluck(body, "data", "titles", .default = NULL)

  if (is.null(titles_data) || length(titles_data) == 0) {
    cli::cli_alert_warning("No titles found for franchise {.val {franchise_title}}.")
    return(tibble::tibble())
  }

  franchise_titles <- purrr::map_chr(titles_data, \(x) purrr::pluck(x, "data", "title", .default = NA_character_))
  franchise_titles <- franchise_titles[!is.na(franchise_titles)]

  if (length(franchise_titles) == 0) {
    cli::cli_alert_warning("Could not extract title names from franchise {.val {franchise_title}}.")
    return(tibble::tibble())
  }

  cli::cli_alert_info("Found {length(franchise_titles)} title{?s} in {.val {franchise_title}}: {.val {franchise_titles}}")

  # Fetch the full chart for the date range
  full_chart <- create_daily_top_ten_tbl(
    token         = token,
    platform_name = platform_name,
    country_name  = country_name,
    start_date    = start_date,
    end_date      = end_date,
    flix_type     = flix_type
  )

  if (nrow(full_chart) == 0) {
    cli::cli_alert_warning("No chart data returned.")
    return(tibble::tibble())
  }

  # Filter to franchise titles and summarise
  out <- full_chart |>
    dplyr::filter(tolower(.data$title) %in% tolower(franchise_titles)) |>
    dplyr::group_by(.data$title) |>
    dplyr::summarise(
      days_on_chart = dplyr::n(),
      best_rank     = min(.data$rank, na.rm = TRUE),
      avg_rank      = round(mean(.data$rank, na.rm = TRUE), 1),
      first_seen    = min(.data$date, na.rm = TRUE),
      last_seen     = max(.data$date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(franchise = franchise_title) |>
    dplyr::arrange(.data$best_rank)

  if (nrow(out) == 0) {
    cli::cli_alert_warning("No titles from {.val {franchise_title}} appeared in the Top 10 during this period.")
  }

  return(out)
}


#' Enrich a Tibble with Resolved Names
#'
#' @description
#' Pipe-friendly helper that detects a column of FlixPatrol IDs (mixed `cmp_`,
#' `ttl_`, `frn_` prefixes) and appends a `name` column with the resolved
#' human-readable names.
#'
#' @param .data A data frame or tibble.
#' @param id_col Tidy-select. The column containing FlixPatrol IDs.
#'   If not supplied, the function searches for the first column whose values
#'   start with `"cmp_"`, `"ttl_"`, or `"frn_"`.
#' @param name_col Character. Name of the new column (default `"name"`).
#' @param token Character. API environment variable name.
#'
#' @return The input tibble with an additional column of resolved names.
#' @export
fp_enrich <- function(.data, id_col = NULL, name_col = "name", token = "FLIX_PATROL") {

  if (is.null(rlang::enexpr(id_col))) {
    # Auto-detect: find first column with fp ID prefixes
    detected <- NULL
    for (col_name in names(.data)) {
      vals <- .data[[col_name]]
      if (is.character(vals) && any(grepl("^(cmp|ttl|frn)_", vals, perl = TRUE), na.rm = TRUE)) {
        detected <- col_name
        break
      }
    }

    if (is.null(detected)) {
      cli::cli_abort("No column with FlixPatrol IDs (cmp_, ttl_, frn_ prefixes) found. Specify {.arg id_col} explicitly.")
    }

    cli::cli_alert_info("Auto-detected ID column: {.val {detected}}")
    id_values <- .data[[detected]]
  } else {
    id_col_name <- rlang::as_name(rlang::enexpr(id_col))
    id_values <- .data[[id_col_name]]
  }

  .data[[name_col]] <- resolve_id(id_values, token = token)

  return(.data)
}


#' Cache Country and Platform IDs to Lookup Tables
#'
#' @description
#' Hits the API once for each provided country and platform name, resolves them
#' to IDs, and saves the results to the package's `country_name` and
#' `platform_name` lookup tables in `data/`.
#'
#' This is useful for populating the cached lookup tables with your own
#' commonly-used values so subsequent lookups avoid API calls.
#'
#' @param country_names Character vector. Country names to cache (or `NULL` to skip).
#' @param platform_names Character vector. Platform names to cache (or `NULL` to skip).
#' @param token Character. API environment variable name.
#' @param pkg_path Character. Path to the package root directory.
#'
#' @return Invisible `TRUE` on success.
#' @export
fp_cache_ids <- function(country_names = NULL,
                         platform_names = NULL,
                         token = "FLIX_PATROL",
                         pkg_path = ".") {

  if (!is.null(country_names)) {
    cli::cli_alert_info("Caching {length(country_names)} country ID{?s}...")
    country_ids <- purrr::map_chr(country_names, \(x) {
      tryCatch(
        lookup_country_and_return_id(x, token = token, silent = TRUE),
        error = function(e) {
          cli::cli_alert_warning("Failed to resolve country: {.val {x}}")
          NA_character_
        }
      )
    })

    country_name <- tibble::tibble(
      country_name = tolower(country_names),
      country_id   = country_ids
    ) |>
      dplyr::filter(!is.na(.data$country_id))

    save(country_name, file = file.path(pkg_path, "data", "country_name.rda"))
    cli::cli_alert_success("Saved {nrow(country_name)} countries to data/country_name.rda")
  }

  if (!is.null(platform_names)) {
    cli::cli_alert_info("Caching {length(platform_names)} platform ID{?s}...")
    platform_ids <- purrr::map_chr(platform_names, \(x) {
      tryCatch(
        lookup_platform_and_return_id(x, token = token, silent = TRUE),
        error = function(e) {
          cli::cli_alert_warning("Failed to resolve platform: {.val {x}}")
          NA_character_
        }
      )
    })

    platform_name <- tibble::tibble(
      platform_name = tolower(platform_names),
      platform_id   = platform_ids
    ) |>
      dplyr::filter(!is.na(.data$platform_id))

    save(platform_name, file = file.path(pkg_path, "data", "platform_name.rda"))
    cli::cli_alert_success("Saved {nrow(platform_name)} platforms to data/platform_name.rda")
  }

  invisible(TRUE)
}
