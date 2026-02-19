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
#' # Set options for repeated use
#' options(
#'   flixpatrol.country_name = "United States",
#'   flixpatrol.start_date   = "2024-01-01",
#'   flixpatrol.end_date     = "2026-12-31",
#'   flixpatrol.media_type   = "movie"
#' )
flixpatrol_options <- function() {

    # Package behavior options
    behavior_opts <- tibble::tibble(
        option = c("flixpatrol.silent", "flixpatrol.return_ids", "flixpatrol.throttle_rate", "flixpatrol.show_quota"),
        description = c(
            "Suppress success messages from lookup functions",
            "Include ID columns (title_id, franchise_id, etc.) in output",
            "API requests per 60 seconds (e.g., 10 = 10 req/min, Inf = no limit)",
            "Show API quota in flixpatrol_sitrep() output"
        ),
        default = c("FALSE", "FALSE", "10", "TRUE"),
        current = c(
            as.character(getOption("flixpatrol.silent", FALSE)),
            as.character(getOption("flixpatrol.return_ids", FALSE)),
            as.character(getOption("flixpatrol.throttle_rate", 10)),
            as.character(getOption("flixpatrol.show_quota", TRUE))
        )
    )

    # Query default options
    query_opts <- tibble::tibble(
        option = c(
            "flixpatrol.platform_name",
            "flixpatrol.country_name",
            "flixpatrol.start_date",
            "flixpatrol.end_date",
            "flixpatrol.media_type",
            "flixpatrol.language",
            "flixpatrol.social_platform",
            "flixpatrol.torrent_site"
        ),
        description = c(
            "Default platform (e.g., 'netflix', 'disney+')",
            "Default country (e.g., 'United States')",
            "Default start date (yyyy-mm-dd format)",
            "Default end date (yyyy-mm-dd format)",
            "Default content type ('movie', 'movies', 'tv', 'tv_show', etc.)",
            "Default language (e.g., 'english')",
            "Default social platform ('instagram', 'twitter', 'facebook')",
            "Default torrent site (e.g., 'PirateBay', '1337x')"
        ),
        default = c("NULL", "NULL", "NULL", "NULL", "NULL", "english", "instagram", "NULL"),
        current = c(
            as.character(getOption("flixpatrol.platform_name", "NULL")),
            as.character(getOption("flixpatrol.country_name", "NULL")),
            as.character(getOption("flixpatrol.start_date", "NULL")),
            as.character(getOption("flixpatrol.end_date", "NULL")),
            as.character(getOption("flixpatrol.media_type", "NULL")),
            as.character(getOption("flixpatrol.language", "english")),
            as.character(getOption("flixpatrol.social_platform", "instagram")),
            as.character(getOption("flixpatrol.torrent_site", "NULL"))
        )
    )

    cli::cli_h1("FlixPatrol Package Options")
    cli::cli_text("Set with {.code options(option_name = value)}")
    cli::cli_text("")

    cli::cli_h2("Behavior Options")
    for (i in seq_len(nrow(behavior_opts))) {
        cli::cli_h3("{.val {behavior_opts$option[i]}}")
        cli::cli_bullets(c(
            " " = behavior_opts$description[i],
            "*" = "Default: {.val {behavior_opts$default[i]}}",
            "*" = "Current: {.val {behavior_opts$current[i]}}"
        ))
    }

    cli::cli_h2("Query Default Options")
    cli::cli_text("Set these to avoid passing arguments on every call.")
    cli::cli_text("")
    for (i in seq_len(nrow(query_opts))) {
        cli::cli_h3("{.val {query_opts$option[i]}}")
        cli::cli_bullets(c(
            " " = query_opts$description[i],
            "*" = "Default: {.val {query_opts$default[i]}}",
            "*" = "Current: {.val {query_opts$current[i]}}"
        ))
    }

    invisible(dplyr::bind_rows(behavior_opts, query_opts))
}


#' Get API Quota Information
#'
#' @description
#' Retrieves current API usage quota from FlixPatrol, showing how many
#' calls have been used, how many remain, and when the quota resets.
#'
#' @return A tibble with quota information: used, available, limit,
#'   limit_extra, and reset_at.
#' @export
#'
#' @examples
#' \dontrun{
#' get_quota()
#' }
get_quota <- function() {

    resp <- authenticate(site = "https://api.flixpatrol.com/v2/quota") |>
        httr2::req_perform()

    data <- httr2::resp_body_json(resp)$data

    tibble::tibble(
        used = data$used %||% NA_integer_,
        available = data$available %||% NA_integer_,
        limit = data$limit %||% NA_integer_,
        limit_extra = data$limitExtra %||% 0L,
        reset_at = as.POSIXct(data$resetAt, format = "%Y-%m-%dT%H:%M:%S")
    )
}


#' FlixPatrol Situation Report
#'
#' @description
#' Displays a diagnostic overview of the flixpatrol package configuration,
#' including authentication status, package options, and API connectivity.
#'
#' @param test_api Logical. If `TRUE`, tests API connectivity by making a
#'   simple request. Default is `TRUE`.
#' @param show_quota Logical. If `TRUE`, displays API quota usage. Default
#'   is `TRUE`.
#'
#' @return Invisibly returns a list with diagnostic information.
#' @export
#'
#' @examples
#' flixpatrol_sitrep()
#'
#' # Skip API test
#' flixpatrol_sitrep(test_api = FALSE)
flixpatrol_sitrep <- function(test_api = TRUE,
                              show_quota = getOption("flixpatrol.show_quota", TRUE)) {

    cli::cli_h1("FlixPatrol Situation Report")

    # Package version
    pkg_version <- tryCatch(
        as.character(utils::packageVersion("flixpatrol")),
        error = function(e) "unknown"
    )
    cli::cli_h2("Package")
    cli::cli_bullets(c("*" = "Version: {.val {pkg_version}}"))

    # Authentication status
    cli::cli_h2("Authentication")
    token_name <- "FLIX_PATROL"
    token_value <- Sys.getenv(token_name)
    token_set <- nchar(token_value) > 0

    if (token_set) {
        # Mask the token for display (show first 4 and last 4 chars)
        if (nchar(token_value) > 10) {
            masked <- paste0(
                substr(token_value, 1, 4),
                "...",
                substr(token_value, nchar(token_value) - 3, nchar(token_value))
            )
        } else {
            masked <- "****"
        }
        cli::cli_bullets(c(
            "v" = "{.envvar {token_name}} is set: {.val {masked}}"
        ))
    } else {
        cli::cli_bullets(c(
            "x" = "{.envvar {token_name}} is {.strong not set}",
            " " = "Set with {.code usethis::edit_r_environ()} and add:",
            " " = "{.code FLIX_PATROL=your_api_key_here}"
        ))
    }

    # API connectivity test (uses quota endpoint since it's lightweight)
    api_ok <- FALSE
    if (test_api && token_set) {
        cli::cli_h2("API Connectivity")
        api_ok <- tryCatch({
            req <- httr2::request("https://api.flixpatrol.com/v2/quota") |>
                httr2::req_auth_basic(username = token_value, password = "")
            resp <- httr2::req_perform(req)
            httr2::resp_status(resp) == 200
        }, error = function(e) FALSE)

        if (api_ok) {
            cli::cli_bullets(c("v" = "API connection successful"))
        } else {
            cli::cli_bullets(c("x" = "API connection failed (check your API key)"))
        }
    } else if (test_api && !token_set) {
        cli::cli_h2("API Connectivity")
        cli::cli_bullets(c("!" = "Skipped (no API key configured)"))
    }

    # API Quota
    quota <- NULL
    if (show_quota && token_set) {
        cli::cli_h2("API Quota")
        quota <- tryCatch({
            q <- get_quota()
            total_limit <- q$limit + q$limit_extra
            pct_used <- round(100 * q$used / total_limit, 1)
            cli::cli_bullets(c(
                "*" = "Used: {.val {q$used}} / {.val {total_limit}} ({pct_used}%)",
                "*" = "Available: {.val {q$available}}",
                "*" = "Resets: {.val {format(q$reset_at, '%Y-%m-%d')}}"
            ))
            q
        }, error = function(e) {
            cli::cli_bullets(c("!" = "Could not retrieve quota"))
            NULL
        })
    } else if (show_quota && !token_set) {
        cli::cli_h2("API Quota")
        cli::cli_bullets(c("!" = "Skipped (no API key configured)"))
    }

    # Package options
    cli::cli_h2("Options")
    opts <- list(
        silent = getOption("flixpatrol.silent", FALSE),
        return_ids = getOption("flixpatrol.return_ids", FALSE),
        throttle_rate = getOption("flixpatrol.throttle_rate", 10),
        show_quota = getOption("flixpatrol.show_quota", TRUE)
    )

    cli::cli_bullets(c(
        "*" = "{.code flixpatrol.silent}: {.val {opts$silent}}",
        "*" = "{.code flixpatrol.return_ids}: {.val {opts$return_ids}}",
        "*" = "{.code flixpatrol.throttle_rate}: {.val {opts$throttle_rate}} req/min",
        "*" = "{.code flixpatrol.show_quota}: {.val {opts$show_quota}}"
    ))

    cli::cli_text("")
    cli::cli_text("Run {.code flixpatrol_options()} for option descriptions.")

    invisible(list(
        version = pkg_version,
        token_set = token_set,
        api_ok = api_ok,
        quota = quota,
        options = opts
    ))
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

    flix_type_normalized <- normalize_plural(flix_type)

    validate_flix_types <- flixpatrol::flix_type_tbl$type_label
    validate_flix_types_normalized <- normalize_plural(validate_flix_types)

    if(!any(flix_type_normalized %in% validate_flix_types_normalized)){

        cli::cli_abort("Please ensure {.val {flix_type}} is one of {.val {validate_flix_types}}")
    }


    flixpatrol::flix_type_tbl$type_id[validate_flix_types_normalized %in% flix_type_normalized]

}


#' Internal helper: fetches Top 10 for a single date
#' Used by compare_platforms, get_global_ranking, get_weekly_movers
#' @noRd
get_single_top_ten <- function(platform_name, country_name, date, media_type) {

    company_string   <- lookup_platform(platform_name = platform_name, silent = TRUE)
    country_string   <- lookup_country(country_name = country_name, silent = TRUE)
    flix_type_string <- resolve_content_type(media_type, "flix")

    resp <- authenticate() |>
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
            type_id <- purrr::pluck(d, "type", .default = NA_integer_)
            type_name <- dplyr::case_when(
                type_id == 2 ~ "movie",
                type_id == 3 ~ "tv_show",
                TRUE ~ NA_character_
            )

            tibble::tibble(
                rank       = purrr::pluck(d, "ranking", .default = NA_integer_),
                title      = purrr::pluck(d, "movie", "data", "title", .default = purrr::pluck(d, "note")),
                type       = type_name,
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
#' The `/top10s` endpoint supports date range queries but limits results to
#' 300 records per call. This function automatically chunks large date ranges
#' into 30-day periods and combines the results.
#'
#' ## Global Options
#'
#' Parameters can be set globally using options:
#' - `options(flixpatrol.platform_name = "netflix")`
#' - `options(flixpatrol.country_name = "United States")`
#' - `options(flixpatrol.start_date = "2024-01-01")`
#' - `options(flixpatrol.end_date = "2026-12-31")`
#' - `options(flixpatrol.media_type = "movie")`
#' - `options(flixpatrol.return_ids = TRUE)`
#'
#' @param platform_name Character. Name of the platform (e.g., "netflix", "disney+").
#'   Default from `getOption("flixpatrol.platform_name")`.
#' @param country_name Character. Name of the country (e.g., "United States").
#'   Default from `getOption("flixpatrol.country_name")`.
#' @param start_date Character. Start of the range in yyyy-mm-dd format.
#'   Default from `getOption("flixpatrol.start_date")`.
#' @param end_date Character. End of the range in yyyy-mm-dd format.
#'   Default from `getOption("flixpatrol.end_date")`.
#' @param media_type Character. Content type (e.g., "movie", "movies", "tv", "tv_show").
#'   Default from `getOption("flixpatrol.media_type")`.
#' @param return_ids Logical. Include ID columns in output.
#'   Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, title, type (movie/tv_show), days_total, rank_last, date, imdb_id.
#'   If `return_ids = TRUE`, also includes title_id and type_id.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set global options
#' options(
#'   flixpatrol.platform_name = "netflix",
#'   flixpatrol.country_name  = "United States",
#'   flixpatrol.start_date    = "2024-01-01",
#'   flixpatrol.end_date      = "2026-02-14",
#'   flixpatrol.media_type    = "movie"
#' )
#'
#' # Call without arguments
#' get_top_ten()
#'
#' # Override specific options
#' get_top_ten(media_type = "tv")
#' }
get_top_ten <- function(platform_name = getOption("flixpatrol.platform_name"),
                        country_name = getOption("flixpatrol.country_name"),
                        start_date = getOption("flixpatrol.start_date"),
                        end_date = getOption("flixpatrol.end_date"),
                        media_type = getOption("flixpatrol.media_type"),
                        return_ids = getOption("flixpatrol.return_ids", FALSE)) {

    # Validate required parameters
    if (is.null(platform_name)) {
        cli::cli_abort("platform_name is required. Set it via argument or {.code options(flixpatrol.platform_name = \"...\")}.")
    }
    if (is.null(country_name)) {
        cli::cli_abort("country_name is required. Set it via argument or {.code options(flixpatrol.country_name = \"...\")}.")
    }
    if (is.null(start_date)) {
        cli::cli_abort("start_date is required. Set it via argument or {.code options(flixpatrol.start_date = \"...\")}.")
    }
    if (is.null(end_date)) {
        cli::cli_abort("end_date is required. Set it via argument or {.code options(flixpatrol.end_date = \"...\")}.")
    }
    if (is.null(media_type)) {
        cli::cli_abort("media_type is required. Set it via argument or {.code options(flixpatrol.media_type = \"...\")}.")
    }

    validate_date_format(start_date)
    validate_date_format(end_date)

    company_string   <- lookup_platform(platform_name = platform_name, silent = TRUE)
    country_string   <- lookup_country(country_name = country_name, silent = TRUE)
    flix_type_string <- resolve_content_type(media_type, "flix")

    # API limits to 300 records (30 days), so chunk into 30-day periods
    start_dt <- as.Date(start_date)
    end_dt   <- as.Date(end_date)
    chunk_size <- 30

    # Generate chunk start dates
    chunk_starts <- seq.Date(from = start_dt, to = end_dt, by = paste(chunk_size, "days"))

    cli::cli_alert_info("Fetching {length(chunk_starts)} chunk{?s} ({length(chunk_starts)} API call{?s})")

    fetch_chunk <- function(chunk_start) {
        chunk_end <- min(chunk_start + chunk_size - 1, end_dt)

        resp <- authenticate() |>
            httr2::req_url_query(
                `company[eq]`     = company_string,
                `country[eq]`     = country_string,
                `type[eq]`        = flix_type_string,
                `date[type][eq]`  = 1,
                `date[from][gte]` = as.character(chunk_start),
                `date[to][lte]`   = as.character(chunk_end)
            ) |>
            httr2::req_perform() |>
            httr2::resp_body_json()

        if (length(resp$data) == 0) {
            return(tibble::tibble())
        }

        resp$data |>
            purrr::map_df(function(item) {
                d <- item$data
                type_id <- purrr::pluck(d, "type", .default = NA_integer_)
                type_name <- dplyr::case_when(
                    type_id == 2 ~ "movie",
                    type_id == 3 ~ "tv_show",
                    TRUE ~ NA_character_
                )

                tibble::tibble(
                    rank       = purrr::pluck(d, "ranking", .default = NA_integer_),
                    title_id   = purrr::pluck(d, "movie", "data", "id", .default = NA_character_),
                    title      = purrr::pluck(d, "movie", "data", "title", .default = purrr::pluck(d, "note")),
                    type_id    = type_id,
                    type       = type_name,
                    days_total = purrr::pluck(d, "daysTotal", .default = NA_integer_),
                    rank_last  = purrr::pluck(d, "rankingLast", .default = NA_integer_),
                    date       = purrr::pluck(d, "date", "from", .default = NA_character_),
                    imdb_id    = purrr::pluck(d, "movie", "data", "imdbId", .default = NA_integer_)
                )
            })
    }

    results <- vector("list", length(chunk_starts))
    for (i in cli::cli_progress_along(chunk_starts, "Fetching top 10")) {
        results[[i]] <- fetch_chunk(chunk_starts[[i]])
    }
    out <- purrr::list_rbind(results)

    if (nrow(out) == 0) {
        cli::cli_alert_warning("No Top 10 data returned for these parameters.")
        return(tibble::tibble())
    }

    if (!return_ids) {
        out <- out |> dplyr::select(-dplyr::any_of(c("title_id", "type_id")))
    }

    return(out)
}



#' Normalize Plural/Singular Forms
#'
#' @description
#' Converts input to a canonical singular form for matching.
#' Handles common patterns like "movies" -> "movie", "tv_shows" -> "tv_show",
#' "documentaries" -> "documentary".
#'
#' @param x Character vector. Input to normalize.
#'
#' @return Character vector with normalized forms.
#' @keywords internal
normalize_plural <- function(x) {
  x <- tolower(x)
  # Handle compound words with underscores
  x <- gsub("_shows$", "_show", x)
  x <- gsub("_movies$", "_movie", x)
  # Special case: "movies" -> "movie" (the "ie" is part of the root, not suffix)
  x <- gsub("^movies$", "movie", x)
  # Handle "ies" -> "y" for other cases (e.g., documentaries -> documentary)
  x <- gsub("ies$", "y", x)
  # Handle simple plurals (but not 'ss' endings like 'business')
  x <- gsub("([^s])s$", "\\1", x)
  x
}


#' Normalize Content Type Input
#'
#' @description
#' Normalizes various content type inputs to canonical forms.
#' Handles: "tv", "tvshow", "tv_show", "tv_shows", "movie", "movies", etc.
#'
#' @param x Character. Content type input.
#'
#' @return Character. Normalized content type ("movie" or "tv_show").
#' @keywords internal
normalize_content_type <- function(x) {
  x <- tolower(trimws(x))
  # Handle TV variations: tv, tvshow, tvshows, tv show, tv shows, tv_show, tv_shows
  x <- gsub("^tv[_\\s]?shows?$", "tv_show", x)
  x <- gsub("^tvshows?$", "tv_show", x)
  x <- gsub("^tv$", "tv_show", x)
  # Handle movie variations
  x <- gsub("^movies?$", "movie", x)
  x
}


#' Resolve Content Type to API ID
#'
#' @description
#' Resolves a content type string to the appropriate API ID for either
#' the flix_type (top10s) or media_type (rankingsofficial) endpoints.
#'
#' @param content_type Character. Content type (e.g., "movie", "tv_show", "anime").
#' @param target Character. Either "flix" or "media" to determine which ID mapping to use.
#'
#' @return Integer. The API ID for the content type.
#' @keywords internal
resolve_content_type <- function(content_type, target = c("flix", "media")) {
  target <- match.arg(target)

  # First normalize common variations

  normalized <- normalize_content_type(content_type)

  if (target == "media") {
    # media_type only supports movie (1) and tv_show (2)
    id <- dplyr::case_when(
      normalized == "movie" ~ 1L,
      normalized == "tv_show" ~ 2L,
      TRUE ~ NA_integer_
    )
    if (is.na(id)) {
      valid_types <- c("movie", "tv_show")
      cli::cli_abort(
        "For this endpoint, {.arg content_type} must be one of {.val {valid_types}}, not {.val {content_type}}."
      )
    }
    return(id)
  }

  if (target == "flix") {
    # flix_type supports many options - use lookup_flix_type
    # But first map normalized forms to flix_type_tbl labels
    flix_input <- dplyr::case_when(
      normalized == "movie" ~ "movies",
      normalized == "tv_show" ~ "tv_shows",
      TRUE ~ normalize_plural(content_type)
    )
    return(lookup_flix_type(flix_input))
  }
}

#' Lookup Values from a Reference Table
#'
#' @description
#' Generic helper that resolves human-readable names to internal FlixPatrol IDs
#' using a pre-built lookup table. Supports `"*"` wildcard to return all IDs.
#' Input is normalized to handle plural/singular variations (e.g., "movie" or
#' "movies" both work).
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

  input_normalized <- normalize_plural(input)
  valid_names <- unique(tbl[[name_col]])
  valid_names_normalized <- normalize_plural(valid_names)

  # Check if normalized input matches normalized valid names
  if (!all(input_normalized %in% valid_names_normalized)) {
    cli::cli_abort("{.arg {label}} must be one of {.val {valid_names}}, not {.val {input}}.")
  }

  # Return IDs where normalized table values match normalized input
  tbl[[id_col]][valid_names_normalized %in% input_normalized]
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


#' Lookup Social Platform IDs
#'
#' @description
#' Resolves social platform names to internal FlixPatrol company IDs.
#'
#' @param social_platform_name Character vector. Social platform names
#'   (e.g., "instagram", "twitter", "facebook") or `"*"` for all.
#'
#' @return A character vector of company IDs.
#' @export
lookup_social_platform <- function(social_platform_name) {
  lookup_from_table(
    input    = social_platform_name,
    tbl      = flixpatrol::social_media_name,
    name_col = "social_media_name",
    id_col   = "social_media_id",
    label    = "social_platform_name"
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
#' to "week" views (API type 3). The function accepts any date range and will
#' fetch all weekly data within that period, iterating through Monday-Sunday weeks.
#'
#' ## Global Options
#'
#' Parameters can be set globally using options:
#' - `options(flixpatrol.platform_name = "netflix")`
#' - `options(flixpatrol.media_type = "movie")`
#' - `options(flixpatrol.language = "english")`
#' - `options(flixpatrol.start_date = "2024-01-01")`
#' - `options(flixpatrol.end_date = "2026-12-31")`
#' - `options(flixpatrol.return_ids = TRUE)`
#'
#' @param platform_name Character. Platform name.
#'   Default from `getOption("flixpatrol.platform_name", "netflix")`.
#' @param media_type Character. Media type (e.g., "movie", "movies", "tv", "tv_show").
#'   Default from `getOption("flixpatrol.media_type", "movie")`.
#' @param language_name Character. Language (e.g., "english").
#'   Default from `getOption("flixpatrol.language", "english")`.
#' @param start_date Character. Range start in yyyy-mm-dd format. Will be adjusted
#'   to next Monday if not already a Monday.
#'   Default from `getOption("flixpatrol.start_date")`.
#' @param end_date Character. Range end in yyyy-mm-dd format.
#'   Default from `getOption("flixpatrol.end_date")`.
#' @param return_ids Logical. Include ID columns in output.
#'   Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, title, hours, views, runtime, streak, date_from, date_to.
#'   If `return_ids = TRUE`, also includes title_id.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set global options
#' options(
#'   flixpatrol.start_date = "2024-01-01",
#'   flixpatrol.end_date   = "2026-02-14"
#' )
#'
#' # Call without date arguments
#' get_hours_viewed()
#' }
get_hours_viewed <- function(
                             platform_name = getOption("flixpatrol.platform_name", "netflix"),
                             media_type = getOption("flixpatrol.media_type", "movie"),
                             language_name = getOption("flixpatrol.language", "english"),
                             start_date = getOption("flixpatrol.start_date"),
                             end_date = getOption("flixpatrol.end_date"),
                             return_ids = getOption("flixpatrol.return_ids", FALSE)) {

  # Validate required parameters
  if (is.null(start_date)) {
    cli::cli_abort("start_date is required. Set it via argument or {.code options(flixpatrol.start_date = \"...\")}.")
  }
  if (is.null(end_date)) {
    cli::cli_abort("end_date is required. Set it via argument or {.code options(flixpatrol.end_date = \"...\")}.")
  }

  validate_date_format(start_date)
  validate_date_format(end_date)

  platform_name_vec <- lookup_platform(platform_name = platform_name, silent = TRUE)
  language_name_vec <- lookup_language(language_name = language_name)
  media_type_vec    <- resolve_content_type(media_type, "media")

  # Convert to Date objects
  start_dt <- as.Date(start_date)
  end_dt   <- as.Date(end_date)

  # Adjust start_date to next Monday if not already Monday
  start_wday <- as.integer(format(start_dt, "%u"))
  if (start_wday != 1) {
    days_to_monday <- (8 - start_wday) %% 7
    if (days_to_monday == 0) days_to_monday <- 7
    start_dt <- start_dt + days_to_monday
    cli::cli_alert_info("Adjusted start_date to next Monday: {.val {start_dt}}")
  }

  # Generate sequence of Mondays
  monday_vec <- seq.Date(from = start_dt, to = end_dt, by = "7 days")

  if (length(monday_vec) == 0) {
    cli::cli_abort("No valid weeks found in date range. Ensure end_date is at least 7 days after start_date.")
  }

  cli::cli_alert_info("Fetching {length(monday_vec)} week{?s} ({length(monday_vec)} API call{?s})")

  fetch_single_week <- function(monday_date) {
    sunday_date <- monday_date + 6

    resp <- authenticate(site = "https://api.flixpatrol.com/v2/hoursviewed") |>
      httr2::req_url_query(
        `company[eq]`    = platform_name_vec,
        `language[eq]`   = language_name_vec,
        `number[eq]`     = media_type_vec,
        `date[type][eq]` = 3,
        `date[from][eq]` = as.character(monday_date),
        `date[to][eq]`   = as.character(sunday_date)
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
          title      = purrr::pluck(d, "movie", "data", "title", .default = NA_character_),
          hours      = purrr::pluck(d, "value", .default = NA_integer_),
          views      = purrr::pluck(d, "views", .default = NA_integer_),
          runtime    = purrr::pluck(d, "runtime", .default = NA_integer_),
          streak     = purrr::pluck(d, "streak", .default = NA_integer_),
          date_from  = purrr::pluck(d, "date", "from", .default = NA_character_),
          date_to    = purrr::pluck(d, "date", "to", .default = NA_character_)
        )
      })
  }

  results <- vector("list", length(monday_vec))
  for (i in cli::cli_progress_along(monday_vec, "Fetching hours viewed")) {
      results[[i]] <- fetch_single_week(monday_vec[[i]])
  }
  out <- purrr::list_rbind(results)

  if (nrow(out) == 0) {
    cli::cli_alert_warning("No hours viewed data returned for these parameters.")
  }

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
#'
#' ## Date Range Handling
#'
#' The function accepts any date range spanning days, months, or years and
#' automatically fetches all weekly rankings within that period. The API
#' requires 7-day Monday-to-Sunday periods, so the function handles this
#' internally:
#'
#' 1. If `start_date` is not a Monday, it is adjusted forward to the next Monday
#' 2. A sequence of Mondays is generated from start to end date
#' 3. For each Monday, the corresponding Sunday is calculated (Monday + 6 days)
#' 4. Each Monday-Sunday pair is queried from the API
#' 5. All results are combined into a single tibble
#'
#' For example, if you provide `start_date = "2024-01-01"` and
#' `end_date = "2024-01-31"`, the function will fetch rankings for:
#' - 2024-01-01 to 2024-01-07
#' - 2024-01-08 to 2024-01-14
#' - 2024-01-15 to 2024-01-21
#' - 2024-01-22 to 2024-01-28
#' - 2024-01-29 to 2024-02-04
#'
#' ## Global Options
#'
#' Parameters can be set globally using options so you don't have to specify
#' them on every call:
#' - `options(flixpatrol.country_name = "United States")`
#' - `options(flixpatrol.start_date = "2024-01-01")`
#' - `options(flixpatrol.end_date = "2026-12-31")`
#' - `options(flixpatrol.media_type = "movie")`
#' - `options(flixpatrol.language = "english")`
#' - `options(flixpatrol.return_ids = TRUE)`
#'
#' @param platform_name Character. Must be "netflix".
#' @param country_name Character. Name of the country (e.g., "United States").
#'   Default from `getOption("flixpatrol.country_name")`.
#' @param start_date Character. Range start in yyyy-mm-dd format. Does not need
#'   to be a Monday; if not, it will be adjusted to the next Monday automatically.
#'   Default from `getOption("flixpatrol.start_date")`.
#' @param end_date Character. Range end in yyyy-mm-dd format. The function will
#'   include any week where the Monday falls on or before this date.
#'   Default from `getOption("flixpatrol.end_date")`.
#' @param media_type Character. Content type ("movie" or "tv_show").
#'   Default from `getOption("flixpatrol.media_type")`.
#' @param date_type Character. Defaults to "week".
#' @param language Character. Language for lookup (e.g., "english").
#'   Default from `getOption("flixpatrol.language", "english")`.
#' @param return_ids Logical. Include ID columns in output.
#'   Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, title, hours, streak, date_from, date_to.
#'   If `return_ids = TRUE`, also includes title_id. Each row represents a title's
#'   ranking for a specific week. Multiple weeks will have multiple rows per title.
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch a single week
#' get_official_ranking(
#'   country_name = "United States",
#'   start_date   = "2024-01-01",
#'   end_date     = "2024-01-07",
#'   media_type   = "movie"
#' )
#'
#' # Fetch multiple years of weekly rankings
#' get_official_ranking(
#'   country_name = "United States",
#'   start_date   = "2022-01-01",
#'   end_date     = "2024-12-31",
#'   media_type   = "movie"
#' )
#'
#' # Set global options for repeated use
#' options(
#'   flixpatrol.country_name = "United States",
#'   flixpatrol.start_date   = "2024-01-01",
#'   flixpatrol.end_date     = "2026-02-14",
#'   flixpatrol.media_type   = "movie"
#' )
#'
#' # Now call without arguments
#' get_official_ranking()
#'
#' # Override specific options as needed
#' get_official_ranking(media_type = "tv_show")
#' get_official_ranking(country_name = "United Kingdom")
#' }
get_official_ranking <- function(
                                 platform_name = "netflix",
                                 country_name = getOption("flixpatrol.country_name"),
                                 start_date = getOption("flixpatrol.start_date"),
                                 end_date = getOption("flixpatrol.end_date"),
                                 media_type = getOption("flixpatrol.media_type"),
                                 date_type = "week",
                                 language = getOption("flixpatrol.language", "english"),
                                 return_ids = getOption("flixpatrol.return_ids", FALSE)) {

  platform_valid_name <- "netflix"

  if (!all(tolower(platform_name) %in% platform_valid_name)) {
    cli::cli_abort("Currently flixpatrol only supports Netflix for the official top ten list, you supplied {.val {platform_name}}.")
  }

  # Validate required parameters
  if (is.null(country_name)) {
    cli::cli_abort("country_name is required. Set it via argument or {.code options(flixpatrol.country_name = \"...\")}.")
  }
  if (is.null(start_date)) {
    cli::cli_abort("start_date is required. Set it via argument or {.code options(flixpatrol.start_date = \"...\")}.")
  }
  if (is.null(end_date)) {
    cli::cli_abort("end_date is required. Set it via argument or {.code options(flixpatrol.end_date = \"...\")}.")
  }
  if (is.null(media_type)) {
    cli::cli_abort("media_type is required. Set it via argument or {.code options(flixpatrol.media_type = \"...\")}.")
  }

  validate_date_format(start_date)
  validate_date_format(end_date)

  platform_id   <- lookup_platform(platform_name = platform_name, silent = TRUE)
  media_type_id <- resolve_content_type(media_type, "media")
  country_id    <- lookup_country(country_name = country_name, silent = TRUE)
  date_type_id  <- lookup_date_type(date_type)
  language_id   <- lookup_language(language)

  # Convert to Date objects
  start_dt <- as.Date(start_date)
  end_dt   <- as.Date(end_date)

  # Adjust start_date to next Monday if not already Monday
  start_wday <- as.integer(format(start_dt, "%u"))
  if (start_wday != 1) {
    days_to_monday <- (8 - start_wday) %% 7
    if (days_to_monday == 0) days_to_monday <- 7
    start_dt <- start_dt + days_to_monday
    cli::cli_alert_info("Adjusted start_date to next Monday: {.val {start_dt}}")
  }

  # Generate sequence of Mondays
  monday_vec <- seq.Date(from = start_dt, to = end_dt, by = "7 days")

  if (length(monday_vec) == 0) {
    cli::cli_abort("No valid weeks found in date range. Ensure end_date is at least 7 days after start_date.")
  }

  cli::cli_alert_info("Fetching {length(monday_vec)} week{?s} ({length(monday_vec)} API call{?s})")

  fetch_single_week <- function(monday_date) {
    sunday_date <- monday_date + 6

    resp <- authenticate(site = "https://api.flixpatrol.com/v2/rankingsofficial") |>
      httr2::req_url_query(
        `country[eq]`     = country_id,
        `company[eq]`     = platform_id,
        `language[eq]`    = language_id,
        `number[eq]`      = media_type_id,
        `date[type][eq]`  = date_type_id,
        `date[from][eq]`  = as.character(monday_date),
        `date[to][eq]`    = as.character(sunday_date)
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
          rank      = purrr::pluck(d, "ranking", .default = NA_integer_),
          title_id  = purrr::pluck(d, "movie", "data", "id", .default = NA_character_),
          title     = purrr::pluck(d, "movie", "data", "title", .default = NA_character_),
          hours     = purrr::pluck(d, "value", .default = NA_integer_),
          streak    = purrr::pluck(d, "streak", .default = NA_integer_),
          date_from = purrr::pluck(d, "date", "from", .default = NA_character_),
          date_to   = purrr::pluck(d, "date", "to", .default = NA_character_)
        )
      })
  }

  results <- vector("list", length(monday_vec))
  for (i in cli::cli_progress_along(monday_vec, "Fetching official rankings")) {
      results[[i]] <- fetch_single_week(monday_vec[[i]])
  }
  out <- purrr::list_rbind(results)

  if (nrow(out) == 0) {
    cli::cli_alert_warning("No official ranking data returned for these parameters.")
    return(out)
  }

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
#' ## Global Options
#'
#' Parameters can be set globally using options:
#' - `options(flixpatrol.social_platform = "instagram")`
#' - `options(flixpatrol.start_date = "2024-01-01")`
#' - `options(flixpatrol.end_date = "2026-12-31")`
#' - `options(flixpatrol.return_ids = TRUE)`
#'
#' @param social_platform Character. Social media platform: "instagram", "twitter", or "facebook".
#'   Default from `getOption("flixpatrol.social_platform", "instagram")`.
#' @param start_date Character. Start date in yyyy-mm-dd format.
#'   Default from `getOption("flixpatrol.start_date")`.
#' @param end_date Character. End date in yyyy-mm-dd format.
#'   Default from `getOption("flixpatrol.end_date")`.
#' @param return_ids Logical. Include ID columns in output.
#'   Default from `getOption("flixpatrol.return_ids", FALSE)`.
#'
#' @return A tibble with columns: rank, title, type (movie/tv_show/franchise), value, value_total, source, date.
#'   If `return_ids = TRUE`, also includes title_id and franchise_id.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set global options
#' options(
#'   flixpatrol.start_date = "2024-01-01",
#'   flixpatrol.end_date   = "2026-02-14"
#' )
#'
#' # Call without date arguments
#' get_fans_ranking()
#' }
get_fans_ranking <- function(
                             social_platform = getOption("flixpatrol.social_platform", "instagram"),
                             start_date = getOption("flixpatrol.start_date"),
                             end_date = getOption("flixpatrol.end_date"),
                             return_ids = getOption("flixpatrol.return_ids", FALSE)) {

    # Validate required parameters
    if (is.null(start_date)) {
        cli::cli_abort("start_date is required. Set it via argument or {.code options(flixpatrol.start_date = \"...\")}.")
    }
    if (is.null(end_date)) {
        cli::cli_abort("end_date is required. Set it via argument or {.code options(flixpatrol.end_date = \"...\")}.")
    }

    validate_date_format(start_date)
    validate_date_format(end_date)

    platform_id <- lookup_social_platform(social_platform)

    # API limits to 300 records, so chunk into 5-day periods (60 records/day typical)
    start_dt <- as.Date(start_date)
    end_dt   <- as.Date(end_date)
    chunk_size <- 5

    chunk_starts <- seq.Date(from = start_dt, to = end_dt, by = paste(chunk_size, "days"))

    cli::cli_alert_info("Fetching {length(chunk_starts)} chunk{?s} ({length(chunk_starts)} API call{?s})")

    fetch_chunk <- function(chunk_start) {
        chunk_end <- min(chunk_start + chunk_size - 1, end_dt)

        resp <- authenticate(site = "https://api.flixpatrol.com/v2/fans") |>
            httr2::req_url_query(
                `company[eq]`     = platform_id,
                `date[type][eq]`  = 1,
                `date[from][gte]` = as.character(chunk_start),
                `date[to][lte]`   = as.character(chunk_end)
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

    results <- vector("list", length(chunk_starts))
    for (i in cli::cli_progress_along(chunk_starts, "Fetching fans ranking")) {
        results[[i]] <- fetch_chunk(chunk_starts[[i]])
    }
    out <- purrr::list_rbind(results)

    if (nrow(out) == 0) {
        cli::cli_alert_warning("No fan data returned for these parameters.")
        return(tibble::tibble())
    }

    # Resolve franchise names (batch lookup)
    franchise_ids <- unique(out$franchise_id[!is.na(out$franchise_id)])

    if (length(franchise_ids) > 0) {
        franchise_names <- get_franchise_name(franchise_ids)

        franchise_lookup <- tibble::tibble(
            franchise_id = franchise_ids,
            franchise_name = franchise_names
        )

        out <- out |>
            dplyr::left_join(franchise_lookup, by = "franchise_id") |>
            dplyr::mutate(
                title = dplyr::coalesce(.data$title, .data$franchise_name)
            ) |>
            dplyr::select(-"franchise_name")
    }

    # Resolve title types (movie/tv_show) via batch lookup
    # Chunk IDs to avoid HTTP 414 URI Too Long errors
    unique_title_ids <- unique(out$title_id[!is.na(out$title_id)])

    if (length(unique_title_ids) > 0) {
        # Split into chunks of 50 IDs
        id_chunks <- split(unique_title_ids, ceiling(seq_along(unique_title_ids) / 50))

        type_lookup <- purrr::map_dfr(id_chunks, function(ids_chunk) {
            ids_string <- paste(ids_chunk, collapse = ",")

            type_resp <- tryCatch(
                authenticate(site = "https://api.flixpatrol.com/v2/titles") |>
                    httr2::req_url_query(`id[in]` = ids_string) |>
                    httr2::req_perform() |>
                    httr2::resp_body_json(),
                error = function(e) NULL
            )

            if (!is.null(type_resp) && length(type_resp$data) > 0) {
                purrr::map_dfr(type_resp$data, function(item) {
                    tibble::tibble(
                        title_id = purrr::pluck(item, "data", "id", .default = NA_character_),
                        type_int = purrr::pluck(item, "data", "type", .default = NA_integer_)
                    )
                })
            } else {
                tibble::tibble(title_id = character(), type_int = integer())
            }
        })

        if (nrow(type_lookup) > 0) {
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
            dplyr::select("rank", "title", "title_id", "franchise_id", "type", "value", "value_total", "source", "date")
    } else {
        out <- out |>
            dplyr::select("rank", "title", "type", "value", "value_total", "source", "date")
    }

    return(out)
}
