#' Authenticate and Create Base Request
#'
#' @description
#' Initializes an authenticated `httr2` request object for the FlixPatrol API using
#' basic authentication.
#'
#' @details
#' This function serves as the foundation for all API calls in the package. It
#' checks for a valid access token stored in the user's system environment.
#' If the token is missing or invalid, it provides instructions on how to set it
#' using `usethis::edit_r_environ()`.
#'
#' @param site Character. The specific API endpoint URL. Defaults to the Top 10s endpoint.
#' @param token Character. The name of the environment variable containing the API key.
#' Default is "FLIX_PATROL".
#'
#' @return A `httr2_request` object with basic authentication headers.
#' @export
authenticate <- function(site="https://api.flixpatrol.com/v2/top10s",token="FLIX_PATROL"){


    if(!is.character(token)){
        cli::cli_abort("{.arg token} must be a character string.")
    }

    if(!is.character(site)){
        cli::cli_abort("{.arg site} must be a character string.")
    }

    access_token <- Sys.getenv(token)

    if(!nchar(access_token) > 1){
        cli::cli_abort("{.val {token}} must be set in your environment. Use {.fn usethis::edit_r_environ} to set your environment variable to {.val FLIX_PATROL}.")
    }

    throttle_rate <- getOption("flixpatrol.throttle_rate", 10)

    req <- httr2::request(site) |>
        httr2::req_auth_basic(username = access_token, password = "")

    # Apply throttling if rate is finite
    if (is.finite(throttle_rate) && throttle_rate > 0) {
        req <- req |> httr2::req_throttle(rate = throttle_rate / 60)
    }

    req <- req |> httr2::req_retry(max_tries = 3)

    return(req)


}


#' FlixPatrol Package Options
#'
#' @description
#' Lists all available package options, their descriptions, and current values.
#'
#' @details
#' Set options using `options(flixpatrol.option_name = value)`.
#' Add to your `~/.Rprofile` to make permanent.
#'
#' @return A tibble of available options (invisibly). Prints a formatted list.
#' @export
#'
#' @examples
#' flixpatrol_options()
#'
#' # Set an option
#' options(flixpatrol.silent = TRUE)
flixpatrol_options <- function() {

    opts <- tibble::tibble(
        option = c("flixpatrol.silent", "flixpatrol.return_ids", "flixpatrol.throttle_rate"),
        description = c(
            "Suppress success messages from lookup functions",
            "Include ID columns (title_id, franchise_id, etc.) in output",
            "API requests per 60 seconds (e.g., 10 = 10 req/min, Inf = no limit)"
        ),
        default = c("FALSE", "FALSE", "10"),
        current = c(
            as.character(getOption("flixpatrol.silent", FALSE)),
            as.character(getOption("flixpatrol.return_ids", FALSE)),
            as.character(getOption("flixpatrol.throttle_rate", 10))
        )
    )

    cli::cli_h1("FlixPatrol Package Options")
    cli::cli_text("Set with {.code options(option_name = value)}")
    cli::cli_text("")

    for (i in seq_len(nrow(opts))) {
        cli::cli_h3("{.val {opts$option[i]}}")
        cli::cli_bullets(c(
            " " = opts$description[i],
            "*" = "Default: {.val {opts$default[i]}}",
            "*" = "Current: {.val {opts$current[i]}}"
        ))
    }

    invisible(opts)
}


#' Lookup Country IDs
#'
#' @description
#' Translates country names into FlixPatrol internal country IDs.
#'
#' @details
#' This function queries the `/countries` endpoint using exact matching. It is
#' vectorized via `purrr::map_chr`, meaning you can pass a single country name
#' or a vector of names. It will return the matching ID for each name provided.
#'
#' @param country_name Character vector. The name(s) of countries to lookup (e.g., "United States", "France").
#' @param silent Logical. If `FALSE` (default), prints success messages using `cli`.
#'
#' @return A named character vector of country IDs.
#' @export
lookup_country <- function(country_name, silent = getOption("flixpatrol.silent", FALSE)) {

    token <- Sys.getenv("FLIX_PATROL")

    # Internal helper to find a single ID
    find_single_id <- function(name) {
        req <-
            authenticate(site = "https://api.flixpatrol.com/v2/countries") |>
            httr2::req_url_query(`name[in]` = name)

        resp <- req |> httr2::req_perform()
        body_lst <- resp |> httr2::resp_body_json()

        # Check if any data was returned
        if (length(body_lst$data) == 0) {
            cli::cli_abort("No country found matching: {.val {name}}")
        }

        # Extract ID from the first match
        id <- body_lst$data[[1]]$data$id

        if (!silent) {
            cli::cli_alert_success("Found: {.val {body_lst$data[[1]]$data$name}} ({.val {id}})")
        }

        return(id)
    }

    # Use map_chr to handle vectors (e.g., c("United States", "France"))
    ids <- purrr::map_chr(country_name, find_single_id)

    return(ids)
}


#' Lookup Platform (Company) IDs
#'
#' @description
#' Translates platform names (e.g., "Netflix", "HBO Max") into internal company IDs.
#'
#' @details
#' This function queries the `/companies` endpoint using exact matching and is
#' fully vectorized.
#'
#' @param platform_name Character vector. The name(s) of platforms to lookup.
#' @param silent Logical. If `FALSE`, prints success messages.
#'
#' @return A named character vector of company IDs.
#' @export
lookup_platform <- function(platform_name, silent = getOption("flixpatrol.silent", FALSE)) {

    token <- Sys.getenv("FLIX_PATROL")

    # Internal helper to find a single ID
    find_single_id <- function(name) {

        body_lst <-
            authenticate(site = "https://api.flixpatrol.com/v2/companies") |>
            httr2::req_url_query(`name[in]` = name) |>
            httr2::req_perform() |>
            httr2::resp_body_json()

        if (length(body_lst$data) == 0) {
            cli::cli_abort("No platform found matching: {.val {name}}")
        }

        id <- body_lst$data[[1]]$data$id

        if (!silent) {
            cli::cli_alert_success("Found: {.val {body_lst$data[[1]]$data$name}} ({.val {id}})")
        }

        return(id)
    }

    # Use map_chr to run the helper for every name in your vector
    ids <- purrr::map_chr(platform_name, find_single_id)

    return(ids)
}



#' Lookup Flix Type IDs
#'
#' @description
#' Validates and returns the internal ID for a Flix type (e.g., "movie" vs "tv").
#'
#' @details
#' This function compares the input against a pre-defined table `flixpatrol::flix_type_tbl`.
#' It ensures that only valid API types are requested, preventing unnecessary API errors.
#'
#' @param flix_type Character. The type of content, typically "movie" or "tv".
#'
#' @return An integer representing the internal type ID.
#' @export
lookup_flix_type <- function(flix_type){

    flix_type_lower <- tolower(flix_type)

    validate_flix_types <- flixpatrol::flix_type_tbl$type_label

    if(!any(flix_type_lower %in% validate_flix_types)){

        cli::cli_abort("Please ensure {.val {flix_type}} is one of {.val {validate_flix_types}}")
    }


    flixpatrol::flix_type_tbl$type_id[flixpatrol::flix_type_tbl$type_label %in% flix_type_lower]

}


#' Internal helper: fetches Top 10 for a single date
#' Used by compare_platforms_tbl, get_global_ranking, get_weekly_movers
#' @noRd
get_single_top_ten <- function(token = "FLIX_PATROL", platform_name, country_name, date, flix_type) {

    company_string   <- lookup_platform(platform_name = platform_name, silent = TRUE)
    country_string   <- lookup_country(country_name = country_name, silent = TRUE)
    flix_type_string <- lookup_flix_type(flix_type = flix_type)

    resp <- authenticate(token = token) |>
        httr2::req_url_query(
            `company[eq]`    = company_string,
            `country[eq]`    = country_string,
            `type[eq]`       = flix_type_string,
            `date[type][eq]` = 1,
            `date[from][eq]` = as.character(date),
            `date[to][eq]`   = as.character(date)
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

    if (length(resp$data) == 0) {
        return(tibble::tibble())
    }

    resp$data |>
        purrr::map_df(function(item) {
            d <- item$data

            tibble::tibble(
                rank       = purrr::pluck(d, "ranking", .default = NA_integer_),
                title      = purrr::pluck(d, "movie", "data", "title", .default = purrr::pluck(d, "note")),
                type_id    = purrr::pluck(d, "type", .default = NA_integer_),
                days_total = purrr::pluck(d, "daysTotal", .default = NA_integer_),
                rank_last  = purrr::pluck(d, "rankingLast", .default = NA_integer_),
                date       = purrr::pluck(d, "date", "from", .default = NA_character_),
                imdb_id    = purrr::pluck(d, "movie", "data", "imdbId", .default = NA_integer_)
            )
        })
}


#' Create a Daily Top 10 Table for a Date Range
#'
#' @description
#' Retrieves daily Top 10 rankings for a platform, country, and content type
#' over a specified date range.
#'
#' @details
#' The `/top10s` endpoint only returns data for a single day per request,
#' so this function iterates over each date in the range. Lookups are
#' performed once upfront to minimize API calls.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. Name of the platform (e.g., "netflix", "disney+").
#' @param country_name Character. Name of the country (e.g., "United States").
#' @param start_date Character. Start of the range in yyyy-mm-dd format.
#' @param end_date Character. End of the range in yyyy-mm-dd format.
#' @param flix_type Character. Content type ("movies" or "tv").
#' @param return_ids Logical. Include ID columns in output. Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, title, type_id, days_total, rank_last, date, imdb_id.
#'   If `return_ids = TRUE`, also includes title_id.
#' @export
#'
#' @examples
#' \dontrun{
#' get_top_ten(
#'   platform_name = "netflix",
#'   country_name  = "United States",
#'   start_date    = "2026-02-01",
#'   end_date      = "2026-02-07",
#'   flix_type     = "movies"
#' )
#' }
get_top_ten <- function(token = "FLIX_PATROL", platform_name, country_name, start_date, end_date, flix_type,
                        return_ids = getOption("flixpatrol.return_ids", FALSE)) {

    validate_date_format(start_date)
    validate_date_format(end_date)

    company_string   <- lookup_platform(platform_name = platform_name, silent = TRUE)
    country_string   <- lookup_country(country_name = country_name, silent = TRUE)
    flix_type_string <- lookup_flix_type(flix_type = flix_type)

    date_vec <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")

    fetch_single_day <- function(date) {
        resp <- authenticate(token = token) |>
            httr2::req_url_query(
                `company[eq]`    = company_string,
                `country[eq]`    = country_string,
                `type[eq]`       = flix_type_string,
                `date[type][eq]` = 1,
                `date[from][eq]` = as.character(date),
                `date[to][eq]`   = as.character(date)
            ) |>
            httr2::req_perform() |>
            httr2::resp_body_json()

        if (length(resp$data) == 0) {
            return(tibble::tibble())
        }

        resp$data |>
            purrr::map_df(function(item) {
                d <- item$data

                tibble::tibble(
                    rank       = purrr::pluck(d, "ranking", .default = NA_integer_),
                    title_id   = purrr::pluck(d, "movie", "data", "id", .default = NA_character_),
                    title      = purrr::pluck(d, "movie", "data", "title", .default = purrr::pluck(d, "note")),
                    type_id    = purrr::pluck(d, "type", .default = NA_integer_),
                    days_total = purrr::pluck(d, "daysTotal", .default = NA_integer_),
                    rank_last  = purrr::pluck(d, "rankingLast", .default = NA_integer_),
                    date       = purrr::pluck(d, "date", "from", .default = NA_character_),
                    imdb_id    = purrr::pluck(d, "movie", "data", "imdbId", .default = NA_integer_)
                )
            })
    }

    out <- purrr::map(date_vec, fetch_single_day) |>
        purrr::list_rbind()

    if (nrow(out) == 0) {
        cli::cli_alert_warning("No Top 10 data returned for these parameters.")
    }

    if (!return_ids && "title_id" %in% names(out)) {
        out <- out |> dplyr::select(-"title_id")
    }

    return(out)
}



#' Lookup Values from a Reference Table
#'
#' @description
#' Generic helper that resolves human-readable names to internal FlixPatrol IDs
#' using a pre-built lookup table. Supports `"*"` wildcard to return all IDs.
#'
#' @param input Character vector. Values to look up, or `"*"` for all.
#' @param tbl Data frame. The reference table to search.
#' @param name_col Character. Column name containing the human-readable names.
#' @param id_col Character. Column name containing the IDs.
#' @param label Character. Label used in error messages.
#'
#' @return A vector of IDs matching the input.
#' @keywords internal
lookup_from_table <- function(input, tbl, name_col, id_col, label) {
  if (all(input == "*")) {
    return(tbl[[id_col]])
  }

  name_lower <- tolower(input)
  valid_names <- unique(tbl[[name_col]])
  valid_names_lower <- tolower(valid_names)

  if (!all(name_lower %in% valid_names_lower)) {
    cli::cli_abort("{.arg {label}} must be one of {.val {valid_names}}, not {.val {input}}.")
  }

  tbl[[id_col]][tolower(tbl[[name_col]]) %in% name_lower]
}


#' Lookup Torrent Site IDs
#'
#' @description
#' Resolves torrent site names to internal FlixPatrol IDs.
#'
#' @param x Character vector. Torrent site names or `"*"` for all.
#'
#' @return A character vector of IDs.
#' @export
lookup_torrent_site <- function(x) {
  lookup_from_table(
    input    = x,
    tbl      = flixpatrol::torrent_sites,
    name_col = "torrent_site_name",
    id_col   = "torrent_site_id",
    label    = "torrent_site"
  )
}


#' Lookup Language IDs
#'
#' @description
#' Resolves language names to internal FlixPatrol IDs.
#'
#' @param language_name Character vector. Language names or `"*"` for all.
#'
#' @return An integer vector of IDs.
#' @export
lookup_language <- function(language_name) {
  lookup_from_table(
    input    = language_name,
    tbl      = flixpatrol::language_name,
    name_col = "language_name",
    id_col   = "language_id",
    label    = "language_name"
  )
}


#' Lookup Media Type IDs
#'
#' @description
#' Resolves media type labels to internal FlixPatrol IDs.
#'
#' @param media_type_name Character vector. Media types or `"*"` for all.
#'
#' @return An integer vector of IDs.
#' @export
lookup_media_type <- function(media_type_name) {
  lookup_from_table(
    input    = media_type_name,
    tbl      = flixpatrol::media_type_name,
    name_col = "media_type_name",
    id_col   = "media_type_id",
    label    = "media_type_name"
  )
}


#' Lookup Date Type IDs
#'
#' @description
#' Resolves date types (Day, Week, Month) to internal API IDs.
#'
#' @param x Character vector. Date types (e.g., "week") or `"*"` for all.
#'
#' @return An integer vector of IDs.
#' @export
lookup_date_type <- function(x) {
  lookup_from_table(
    input    = x,
    tbl      = flixpatrol::date_type_name,
    name_col = "date_type_name",
    id_col   = "date_type_id",
    label    = "date_type_name"
  )
}


#' Validate Date Ranges for API Compatibility
#'
#' @description
#' Ensures that the provided start and end dates meet specific API requirements
#' (e.g., starting on a Monday for weekly views).
#'
#' @param start_date Date/Character.
#' @param end_date Date/Character.
#' @param start_date_wday_abb Character. Three-letter weekday abbreviation (e.g., "mon").
#' @param range_length Integer. Expected number of days in the range. Default is 7.
#'
#' @return Logical `TRUE` if valid, otherwise aborts with a `cli` error.
#' @export
validate_date_range <- function(start_date, end_date, start_date_wday_abb, range_length = 7){

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  valid_wday_abb <- c("mon","tue","wed","thu","fri","sat","sun")

  if(!any(tolower(start_date_wday_abb) %in% valid_wday_abb)){
    cli::cli_abort("{.arg start_date_wday_abb} must be one of {.val {valid_wday_abb}}, not {.val {start_date_wday_abb}}.")
  }

  if(!all(tolower(lubridate::wday(start_date, label = TRUE)) == start_date_wday_abb)){
    cli::cli_abort("{.val {start_date}} must start on {.val {start_date_wday_abb}}, not {.val {tolower(lubridate::wday(start_date, label = TRUE))}}.")
  }

  date_vec <- seq.Date(from = start_date, to = end_date, by = "day")

  if(length(date_vec) != range_length){
    cli::cli_abort("{.val {start_date}} and {.val {end_date}} must be {.val {range_length}} days apart.")
  }

  return(TRUE)
}









#' Unnest a FlixPatrol API Response
#'
#' @description
#' Shared helper that flattens the nested JSON structure returned by most
#' FlixPatrol list endpoints into a wide, analysis-ready tibble.
#'
#' @param data_lst List. The `data` element from a parsed API response.
#'
#' @return A wide tibble with all nested columns expanded.
#' @keywords internal
unnest_flixpatrol_response <- function(data_lst) {

    # Helper to conditionally unnest a column if it exists
    unnest_if_exists <- function(.data, col, names_sep = "_") {
        if (col %in% names(.data)) {
            tidyr::unnest_wider(.data, tidyr::all_of(col), names_sep = names_sep)
        } else {
            .data
        }
    }

    data_lst |>
        tibble::tibble(raw = _) |>
        tidyr::unnest_wider("raw") |>
        unnest_if_exists("data") |>
        unnest_if_exists("movie") |>
        unnest_if_exists("movie_data") |>
        unnest_if_exists("company") |>
        unnest_if_exists("company_data") |>
        unnest_if_exists("company_legacy") |>
        unnest_if_exists("country") |>
        unnest_if_exists("country_data") |>
        unnest_if_exists("country_legacy") |>
        unnest_if_exists("date") |>
        janitor::clean_names()
}


#' Create Hours Viewed Table
#'
#' @description
#' Retrieves the "Hours Viewed" metric for a platform, language, and content type.
#'
#' @details
#' This function specifically handles the `/hoursviewed` endpoint. It defaults
#' to "week" views (API type 3) and requires a 7-day range starting on a Monday.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. Platform name (defaults to "netflix").
#' @param media_type_name Character. Media type ("movie" or "tv_show").
#' @param language_name Character. Language (e.g., "english").
#' @param start_date Character. Must be a Monday (yyyy-mm-dd format).
#' @param end_date Character. Must be 7 days from start (yyyy-mm-dd format).
#' @param return_ids Logical. Include ID columns in output. Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, title, hours, views, runtime, streak, date_from, date_to.
#'   If `return_ids = TRUE`, also includes title_id.
#' @export
#'
#' @examples
#' \dontrun{
#' get_hours_viewed(
#'   language_name   = "english",
#'   media_type_name = "movie",
#'   start_date      = "2026-02-10",
#'   end_date        = "2026-02-16"
#' )
#' }
get_hours_viewed <- function(token = "FLIX_PATROL",
                             platform_name = "netflix",
                             media_type_name = "movie",
                             language_name = "english",
                             start_date,
                             end_date,
                             return_ids = getOption("flixpatrol.return_ids", FALSE)) {

  platform_name_vec <- lookup_platform(platform_name = platform_name, silent = TRUE)
  language_name_vec <- lookup_language(language_name = language_name)
  media_type_vec    <- lookup_media_type(media_type_name = media_type_name)

  validate_date_range(start_date = start_date, end_date = end_date, start_date_wday_abb = "mon", range_length = 7)

  resp <- authenticate(token = token, site = "https://api.flixpatrol.com/v2/hoursviewed") |>
    httr2::req_url_query(
      `company[eq]`    = platform_name_vec,
      `language[eq]`   = language_name_vec,
      `number[eq]`     = media_type_vec,
      `date[type][eq]` = 3,
      `date[from][eq]` = as.character(start_date),
      `date[to][eq]`   = as.character(end_date)
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (length(resp$data) == 0) {
    cli::cli_alert_warning("No hours viewed data returned for these parameters.")
    return(tibble::tibble())
  }

  out <- resp$data |>
    purrr::map_df(function(item) {
      d <- item$data

      tibble::tibble(
        rank       = purrr::pluck(d, "ranking", .default = NA_integer_),
        title_id   = purrr::pluck(d, "movie", "data", "id", .default = NA_character_),
        title      = purrr::pluck(d, "movie", "data", "title", .default = NA_character_),
        hours      = purrr::pluck(d, "value", .default = NA_integer_),
        views      = purrr::pluck(d, "views", .default = NA_integer_),
        runtime    = purrr::pluck(d, "runtime", .default = NA_integer_),
        streak     = purrr::pluck(d, "streak", .default = NA_integer_),
        date_from  = purrr::pluck(d, "date", "from", .default = NA_character_),
        date_to    = purrr::pluck(d, "date", "to", .default = NA_character_)
      )
    })

  if (!return_ids && "title_id" %in% names(out)) {
    out <- out |> dplyr::select(-"title_id")
  }

  return(out)
}




#' Create Official Ranking Table
#'
#' @description
#' Fetches the official weekly rankings (currently Netflix exclusive).
#'
#' @details
#' Queries the `/rankingsofficial` endpoint. Note that as of the current
#' package version, FlixPatrol only provides official lists for Netflix.
#' Requires a 7-day range starting on a Monday.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. Must be "netflix".
#' @param country_name Character. Name of the country (e.g., "United States").
#' @param start_date Character. Range start, must be a Monday (yyyy-mm-dd format).
#' @param end_date Character. Range end, must be Sunday (yyyy-mm-dd format).
#' @param media_type Character. Content type ("movie" or "tv_show").
#' @param date_type Character. Defaults to "week".
#' @param language Character. Language for lookup (e.g., "english").
#' @param return_ids Logical. Include ID columns in output. Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, title, hours, streak, date_from, date_to.
#'   If `return_ids = TRUE`, also includes title_id.
#' @export
#'
#' @examples
#' \dontrun{
#' get_official_ranking(
#'   country_name = "United States",
#'   start_date   = "2026-02-10",
#'   end_date     = "2026-02-16",
#'   media_type   = "movie"
#' )
#' }
get_official_ranking <- function(token = "FLIX_PATROL",
                                 platform_name = "netflix",
                                 country_name,
                                 start_date,
                                 end_date,
                                 media_type,
                                 date_type = "week",
                                 language = "english",
                                 return_ids = getOption("flixpatrol.return_ids", FALSE)) {

  platform_valid_name <- "netflix"

  if (!all(tolower(platform_name) %in% platform_valid_name)) {
    cli::cli_abort("Currently flixpatrol only supports Netflix for the official top ten list, you supplied {.val {platform_name}}.")
  }

  platform_id   <- lookup_platform(platform_name = platform_name, silent = TRUE)
  media_type_id <- lookup_media_type(media_type_name = media_type)
  country_id    <- lookup_country(country_name = country_name, silent = TRUE)
  date_type_id  <- lookup_date_type(date_type)
  language_id   <- lookup_language(language)

  validate_date_range(start_date = start_date, end_date = end_date, start_date_wday_abb = "mon", range_length = 7)

  resp <- authenticate(site = "https://api.flixpatrol.com/v2/rankingsofficial", token = token) |>
    httr2::req_url_query(
      `country[eq]`     = country_id,
      `company[eq]`     = platform_id,
      `language[eq]`    = language_id,
      `number[eq]`      = media_type_id,
      `date[type][eq]`  = date_type_id,
      `date[from][eq]`  = as.character(start_date),
      `date[to][eq]`    = as.character(end_date)
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (length(resp$data) == 0) {
    cli::cli_alert_warning("No official ranking data returned for these parameters.")
    return(tibble::tibble())
  }

  out <- resp$data |>
    purrr::map_df(function(item) {
      d <- item$data

      tibble::tibble(
        rank      = purrr::pluck(d, "ranking", .default = NA_integer_),
        title_id  = purrr::pluck(d, "movie", "data", "id", .default = NA_character_),
        title     = purrr::pluck(d, "movie", "data", "title", .default = NA_character_),
        hours     = purrr::pluck(d, "value", .default = NA_integer_),
        streak    = purrr::pluck(d, "streak", .default = NA_integer_),
        date_from = purrr::pluck(d, "date", "from", .default = NA_character_),
        date_to   = purrr::pluck(d, "date", "to", .default = NA_character_)
      )
    })

  if (!return_ids && "title_id" %in% names(out)) {
    out <- out |> dplyr::select(-"title_id")
  }

  return(out)
}




#' Get Fans Rankings
#'
#' @description
#' Retrieves social media fan ranking data for titles from a specific
#' social platform and date range.
#'
#' @details
#' The `/fans` endpoint tracks title popularity on social networks.
#' The `social_platform` parameter filters by social media site
#' (Instagram, Twitter, Facebook), not streaming platforms.
#' Franchise IDs are automatically resolved to names via batch API lookup.
#' Title types (movie/tv_show) are resolved via batch API lookup.
#'
#' @param token Character. API environment variable name.
#' @param social_platform Character. Social media platform: "instagram", "twitter", or "facebook".
#' @param start_date Character. Start date in yyyy-mm-dd format.
#' @param end_date Character. End date in yyyy-mm-dd format.
#' @param return_ids Logical. Include ID columns in output. Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, name, type (movie/tv_show/franchise), value, value_total, source, date.
#'   If `return_ids = TRUE`, also includes title_id and franchise_id.
#' @export
#'
#' @examples
#' \dontrun{
#' get_fans_ranking(
#'   social_platform = "instagram",
#'   start_date      = "2026-02-14",
#'   end_date        = "2026-02-14"
#' )
#' }
get_fans_ranking <- function(token = "FLIX_PATROL",
                             social_platform = "instagram",
                             start_date,
                             end_date,
                             return_ids = getOption("flixpatrol.return_ids", FALSE)) {

    validate_date_format(start_date)
    validate_date_format(end_date)

    # Social platform IDs (these are in the companies endpoint)
    social_ids <- c(
        instagram = "cmp_Di2u9cvFOpmwE3Zhn1SozXJN",
        twitter   = "cmp_bBrpGTvopBHPVtIKhR2CF68W",
        facebook  = "cmp_JeAe7ZCyHJKz3fYOrtq6En4s"
    )

    platform_lower <- tolower(social_platform)
    if (!platform_lower %in% names(social_ids)) {
        cli::cli_abort("{.arg social_platform} must be one of {.val {names(social_ids)}}, not {.val {social_platform}}.")
    }

    platform_id <- social_ids[[platform_lower]]

    date_vec <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")

    fetch_single_day <- function(date) {
        resp <- authenticate(site = "https://api.flixpatrol.com/v2/fans", token = token) |>
            httr2::req_url_query(
                `company[eq]`    = platform_id,
                `date[type][eq]` = 1,
                `date[from][eq]` = as.character(date),
                `date[to][eq]`   = as.character(date)
            ) |>
            httr2::req_perform() |>
            httr2::resp_body_json()

        if (length(resp$data) == 0) {
            return(tibble::tibble())
        }

        resp$data |>
            purrr::map_df(function(item) {
                d <- item$data

                title_id     <- purrr::pluck(d, "movie", "data", "id", .default = NULL)
                title_name   <- purrr::pluck(d, "movie", "data", "title", .default = NULL)
                franchise_id <- purrr::pluck(d, "franchise", "data", "id", .default = NULL)

                tibble::tibble(
                    rank            = purrr::pluck(d, "ranking", .default = NA_integer_),
                    title_id        = title_id %||% NA_character_,
                    title           = title_name %||% NA_character_,
                    franchise_id    = franchise_id %||% NA_character_,
                    type            = if (!is.null(title_name)) "title" else if (!is.null(franchise_id)) "franchise" else NA_character_,
                    value           = purrr::pluck(d, "value", .default = NA_integer_),
                    value_total     = purrr::pluck(d, "valueTotal", .default = NA_integer_),
                    source          = social_platform,
                    date            = purrr::pluck(d, "date", "from", .default = NA_character_)
                )
            })
    }

    out <- purrr::map(date_vec, fetch_single_day) |>
        purrr::list_rbind()

    if (nrow(out) == 0) {
        cli::cli_alert_warning("No fan data returned for these parameters.")
        return(out)
    }

    # Resolve franchise names (batch lookup)
    franchise_ids <- unique(out$franchise_id[!is.na(out$franchise_id)])

    if (length(franchise_ids) > 0) {
        franchise_names <- get_franchise_name(franchise_ids, token = token)

        franchise_lookup <- tibble::tibble(
            franchise_id = franchise_ids,
            franchise_name = franchise_names
        )

        out <- out |>
            dplyr::left_join(franchise_lookup, by = "franchise_id") |>
            dplyr::mutate(
                name = dplyr::coalesce(.data$title, .data$franchise_name)
            )
    } else {
        out <- out |>
            dplyr::mutate(name = .data$title)
    }

    # Resolve title types (movie/tv_show) via batch lookup
    unique_title_ids <- unique(out$title_id[!is.na(out$title_id)])

    if (length(unique_title_ids) > 0) {
        ids_string <- paste(unique_title_ids, collapse = ",")

        type_resp <- tryCatch(
            authenticate(site = "https://api.flixpatrol.com/v2/titles", token = token) |>
                httr2::req_url_query(`id[in]` = ids_string) |>
                httr2::req_perform() |>
                httr2::resp_body_json(),
            error = function(e) NULL
        )

        if (!is.null(type_resp) && length(type_resp$data) > 0) {
            type_lookup <- purrr::map_dfr(type_resp$data, function(item) {
                tibble::tibble(
                    title_id = purrr::pluck(item, "data", "id", .default = NA_character_),
                    type_int = purrr::pluck(item, "data", "type", .default = NA_integer_)
                )
            })

            out <- out |>
                dplyr::left_join(type_lookup, by = "title_id") |>
                dplyr::mutate(
                    type = dplyr::case_when(
                        !is.na(.data$franchise_id) ~ "franchise",
                        .data$type_int == 1 ~ "movie",
                        .data$type_int == 2 ~ "tv_show",
                        TRUE ~ NA_character_
                    )
                ) |>
                dplyr::select(-"type_int")
        }
    }

    # Select final columns
    if (return_ids) {
        out <- out |>
            dplyr::select("rank", "name", "title_id", "franchise_id", "type", "value", "value_total", "source", "date")
    } else {
        out <- out |>
            dplyr::select("rank", "name", "type", "value", "value_total", "source", "date")
    }

    return(out)
}
