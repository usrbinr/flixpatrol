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

    req <- httr2::request(site) |>
        httr2::req_auth_basic(username = access_token, password = "") |>
        httr2::req_throttle(rate = 10 / 60) |>
        httr2::req_retry(max_tries = 3)

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
        option = c("flixpatrol.silent"),
        description = c("Suppress success messages from lookup functions"),
        default = c("FALSE"),
        current = c(as.character(getOption("flixpatrol.silent", FALSE)))
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



# Internal helper: fetches Top 10 for a single date
# @noRd
get_single_top_ten <- function(token = "FLIX_PATROL", platform_name, country_name, date, flix_type) {


    company_string        <- lookup_platform(platform_name = platform_name, silent = TRUE)
    country_string        <- lookup_country(country_name = country_name, silent = TRUE)
    flix_type_string      <- lookup_flix_type(flix_type = flix_type)


    x <- authenticate(token = token) |>
        httr2::req_url_query(
            `company[eq]` = company_string,
            `country[eq]` = country_string,
            `type[eq]`=flix_type_string,
            `date[type][eq]` = 1,           # 1 = Daily Chart
            `date[from][eq]` = date,
            `date[to][eq]` = date
        ) |>
        httr2::req_perform() |>
      httr2::resp_body_json()

    out <-
        x$data |>
        purrr::map_df(function(item) {
            # Extracting the inner 'data' block
            d <- item$data

            tibble::tibble(
                rank        = purrr::pluck(d, "ranking", .default = NA_integer_),
                title       = purrr::pluck(d, "movie", "data", "title", .default = purrr::pluck(d, "note")),
                type_id     = purrr::pluck(d, "type", .default = NA_integer_),
                days_total  = purrr::pluck(d, "daysTotal", .default = NA_integer_),
                rank_last   = purrr::pluck(d, "rankingLast", .default = NA_integer_),
                date        = purrr::pluck(d, "date", "from", .default = NA_character_),
                imdb_id     = purrr::pluck(d, "movie", "data", "imdbId", .default = NA_integer_)
            )
        })


    return(out)

}



#' Create a Daily Top 10 Table for a Date Range
#'
#' @description
#' Iterates over a sequence of dates to build a historical Top 10 dataset.
#'
#' @details
#' This function generates a date vector between `start_date` and `end_date`
#' and maps `get_single_top_ten` over each date. The results
#' are bound together into a single master tibble.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. Name of the platform.
#' @param country_name Character. Name of the country.
#' @param start_date Date/Character. Start of the range.
#' @param end_date Date/Character. End of the range.
#' @param flix_type Character. Content type.
#'
#' @return A combined tibble for the specified date range.
#' @export
get_top_ten <- function(token="FLIX_PATROL",platform_name,country_name,start_date,end_date,flix_type){


    # test variables
    # start_date="2025-12-01"
    # end_date="2025-12-21"

    start_date <- as.Date(start_date)
    end_date   <- as.Date(end_date)
    date_vec   <- seq.Date(from = start_date, to = end_date, by = "day")


   out <-  purrr::map(.x=date_vec,.f = \(x) get_single_top_ten(token=token,platform_name=platform_name,country_name=country_name,date=x,flix_type=flix_type) ) |>
        purrr::list_rbind()

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

  if (!all(name_lower %in% valid_names)) {
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
#' The resulting JSON is heavily unnested to provide a wide, analysis-ready tibble.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. Platform name (defaults to "netflix").
#' @param media_type_name Character. Media type (e.g., "Movie").
#' @param language_name Character. Language (e.g., "English").
#' @param start_date Date/Character. Must be a Monday.
#' @param end_date Date/Character. Must be 7 days from start.
#'
#' @return A wide tibble containing viewing hours and metadata.
#' @export
get_hours_viewed <- function(token = "FLIX_PATROL",
                                    platform_name = "netflix",
                                    media_type_name = "movie",
                                    language_name = "english",
                                    start_date,
                                    end_date) {

  platform_name_vec <- lookup_platform(platform_name = platform_name, silent = TRUE)
  language_name_vec <- lookup_language(language_name = language_name)
  media_type_vec    <- lookup_media_type(media_type_name = media_type_name)

  validate_date_range(start_date = start_date, end_date = end_date, start_date_wday_abb = "mon", range_length = 7)

  body_lst <-
    authenticate(token = token, site = "https://api.flixpatrol.com/v2/hoursviewed") |>
    httr2::req_url_query(
      `company[eq]`    = platform_name_vec,
      `language[eq]`   = language_name_vec,
      `number[eq]`     = media_type_vec,
      `date[type][eq]` = 3,
      `date[from][eq]` = as.character(start_date),
      `date[to][eq]`   = as.character(end_date)
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("data")

  if (length(body_lst) == 0) {
    cli::cli_alert_warning("No hours viewed data returned for these parameters.")
    return(tibble::tibble())
  }

  out <- unnest_flixpatrol_response(body_lst)

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
#' This function enforces that restriction.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. Must be "netflix".
#' @param country_name Character. Name of the country.
#' @param start_date Date/Character. Range start (Monday).
#' @param end_date Date/Character. Range end (Sunday).
#' @param media_type Character. Content type.
#' @param date_type Character. Defaults to "week".
#' @param language_id Character. Language for lookup.
#'
#' @return A tibble of official rankings.
#' @export
get_official_ranking <- function(token = "FLIX_PATROL",
                                        platform_name = "netflix",
                                        country_name,
                                        start_date,
                                        end_date,
                                        media_type,
                                        date_type = "week",
                                        language = "english") {

  platform_valid_name <- "netflix"

  if (!all(tolower(platform_name) %in% platform_valid_name)) {
    cli::cli_abort("Currently flixpatrol only supports Netflix for the official top ten list, you supplied {.val {platform_name}}.")
  }

  platform_id   <- lookup_platform(platform_name = platform_name, silent = TRUE)
  media_type_id <- lookup_media_type(media_type = media_type)
  country_id    <- lookup_country(country_name = country_name, silent = TRUE)
  date_type_id <- lookup_date_type(date_type)
  language_id  <- lookup_language(language)

  validate_date_range(start_date = start_date, end_date = end_date, start_date_wday_abb = "mon", range_length = 7)

  body_lst <- authenticate(site = "https://api.flixpatrol.com/v2/rankingsofficial", token = token) |>
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
    httr2::resp_body_json() |>
    purrr::pluck("data")

  out <- unnest_flixpatrol_response(body_lst)

  return(out)
}




#' Fetch Fans Site Rankings
#'
#' @description
#' Retrieves the "Fans" ranking data for titles based on platform, country, and date range.
#'
#' @param token Character. API environment variable name.
#' @param platform_name Character. The platform (e.g., "Netflix").
#' @param country_name Character. The country name.
#' @param start_date Date/Character. Start date of the ranking.
#' @param end_date Date/Character. End date of the ranking.
#' @param media_type Character. "Movie" or "TvShow".
#' @param date_type Character. Defaults to "day".
#' @param language Character. Language name for filtering.
#'
#' @return A tidy tibble of fan rankings.
#' @export
fetch_fans_ranking_tbl <- function(token = "FLIX_PATROL",
                                   platform_name = "netflix",
                                   country_name = "United States",
                                   start_date,
                                   end_date,
                                   media_type = "Movie",
                                   date_type = "day",
                                   language = "English") {

  # 1. Input Validation & ID Lookups
  # Ensures we don't pass 'USA' if the API expects 'United States'
  platform_id   <- lookup_platform(platform_name, silent = TRUE)
  media_type_id <- lookup_media_type(media_type)
  country_id    <- lookup_country(country_name, silent = TRUE)
  date_type_id  <- lookup_date_type(date_type)
  language_id   <- lookup_language(language)

  # Check date validity (assuming weekly ranges if that's your requirement)
  if (date_type == "week") {
    validate_date_range(start_date, end_date, start_date_wday_abb = "mon", range_length = 7)
  }

  # 2. API Request
  resp <- authenticate(site = "https://api.flixpatrol.com/v2/fans", token = token) |>
    httr2::req_url_query(
      `company[eq]`   = platform_id,
      `country[eq]`   = country_id,
      `language[eq]`  = language_id,
      `number[eq]`    = media_type_id,
      `date[type][eq]` = date_type_id,
      `date[from][eq]` = as.character(start_date),
      `date[to][eq]`   = as.character(end_date)
    ) |>
    httr2::req_perform()

  # 3. Handle JSON Response
  body <- httr2::resp_body_json(resp)

  if (length(body$data) == 0) {
    cli::cli_alert_warning("No fan data returned for these parameters.")
    return(tibble::tibble())
  }

  # 4. Tidy Unnesting
  out <- unnest_flixpatrol_response(body$data)

  return(out)
}
