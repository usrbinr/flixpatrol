#' Get Company Name from ID
#'
#' @description
#' Retrieves the human-readable company name associated with a specific
#' FlixPatrol company ID.
#'
#' @details
#' This function hits the `/companies/:id` endpoint. It is specifically designed
#' to reverse a lookup where you have the ID (e.g., from a data table) and
#' need the display name (e.g., "Netflix").
#'
#' @param company_id Character. The FlixPatrol ID (e.g., "cmp_IA6TdMqwf6kuyQvxo9bJ4nKX").
#' @param token Character. The name of the environment variable containing the API key.
#'
#' @return A character string containing the company name.
#' @export
#'
#' @examples
#' \dontrun{
#' get_company_name("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX")
#' # [1] "Netflix"
#' }
get_company_name <- function(company_id, token = "FLIX_PATROL") {

    if (missing(company_id) || !is.character(company_id)) {
        cli::cli_abort("{.arg company_id} must be a character string.")
    }

    # 1. Build the specific URL for this ID
    # Endpoint format: https://api.flixpatrol.com/v2/companies/:id
    endpoint_url <- paste0("https://api.flixpatrol.com/v2/companies/", company_id)

    # 2. Authenticate and Perform Request
    resp <- authenticate(site = endpoint_url, token = token) |>
        httr2::req_perform()

    # 3. Parse Response
    body <- httr2::resp_body_json(resp)

    # 4. Extract Name
    # Based on the object structure: body$data$name
    company_name <- purrr::pluck(body, "data", "name", .default = NULL)

    if (is.null(company_name)) {
        cli::cli_abort("Could not find a company name for ID: {.val {company_id}}")
    }

    return(company_name)
}




#' Lookup Franchise IDs
#'
#' @description
#' Translates human-readable franchise titles (e.g., "Indiana Jones", "Marvel")
#' into FlixPatrol internal franchise IDs.
#'
#' @details
#' This function queries the `/franchises` endpoint using exact matching. It is
#' vectorized via `purrr::map_chr`, allowing you to search for multiple franchises
#' at once.
#'
#' @param franchise_title Character vector. The title(s) of the franchises to lookup.
#' @param silent Logical. If `FALSE` (default), prints success messages using `cli`.
#'
#' @return A named character vector of franchise IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' lookup_franchise("Indiana Jones")
#' # [1] "frn_STNa2GEw54ahWxJsXNgBMmt0"
#' }
lookup_franchise <- function(franchise_title, silent = getOption("flixpatrol.silent", FALSE)) {

    token <- Sys.getenv("FLIX_PATROL")

    # Internal helper to find a single ID via API
    find_single_id <- function(name) {

        resp <- authenticate(site = "https://api.flixpatrol.com/v2/franchises") |>
            httr2::req_url_query(`title[in]` = name) |>
            httr2::req_perform()

        body_lst <- httr2::resp_body_json(resp)

        # Check if any data was returned
        if (length(body_lst$data) == 0) {
            cli::cli_abort("No franchise found matching: {.val {name}}")
        }

        # Extract ID from the first match
        id <- purrr::pluck(body_lst, "data", 1, "data", "id", .default = NULL)

        if (is.null(id)) {
            cli::cli_abort("API returned data for {.val {name}} but no ID was found.")
        }

        if (!silent) {
            found_name <- purrr::pluck(body_lst, "data", 1, "data", "title")
            cli::cli_alert_success("Found: {.val {found_name}} ({.val {id}})")
        }

        return(id)
    }

    # Use map_chr to handle vectors (e.g., c("Star Wars", "Indiana Jones"))
    ids <- purrr::map_chr(franchise_title, find_single_id)

    return(ids)
}


#' Get Franchise Name from ID
#'
#' @description
#' Retrieves the human-readable franchise title associated with a specific
#' FlixPatrol franchise ID.
#'
#' @details
#' This function hits the `/franchises/:id` endpoint. It is used to resolve
#' IDs found in data tables back into readable titles for reporting or
#' visualization. Vectorized to handle multiple IDs.
#'
#' @param franchise_id Character vector. One or more FlixPatrol franchise IDs
#'   (e.g., "frn_STNa2GEw54ahWxJsXNgBMmt0").
#' @param token Character. The name of the environment variable containing the API key.
#'
#' @return A character vector of franchise titles (same length as input).
#' @export
#'
#' @examples
#' \dontrun{
#' get_franchise_name("frn_STNa2GEw54ahWxJsXNgBMmt0")
#' # [1] "Indiana Jones"
#'
#' # Vectorized
#' get_franchise_name(c("frn_abc123", "frn_def456"))
#' }
get_franchise_name <- function(franchise_id, token = "FLIX_PATROL") {

    if (missing(franchise_id) || !is.character(franchise_id)) {
        cli::cli_abort("{.arg franchise_id} must be a character vector.")
    }

    # Remove NAs
    franchise_id <- franchise_id[!is.na(franchise_id)]

    if (length(franchise_id) == 0) {
        return(character(0))
    }

    # Single ID - use direct endpoint
    if (length(franchise_id) == 1) {
        endpoint_url <- paste0("https://api.flixpatrol.com/v2/franchises/", franchise_id)

        resp <- tryCatch(
            authenticate(site = endpoint_url, token = token) |>
                httr2::req_perform() |>
                httr2::resp_body_json(),
            error = function(e) NULL
        )

        if (is.null(resp)) {
            return(NA_character_)
        }

        return(purrr::pluck(resp, "data", "title", .default = NA_character_))
    }

    # Multiple IDs - use batch lookup with [in] operator (single API call)
    ids_string <- paste(franchise_id, collapse = ",")

    resp <- tryCatch(
        authenticate(site = "https://api.flixpatrol.com/v2/franchises", token = token) |>
            httr2::req_url_query(`id[in]` = ids_string) |>
            httr2::req_perform() |>
            httr2::resp_body_json(),
        error = function(e) NULL
    )

    if (is.null(resp) || length(resp$data) == 0) {
        return(rep(NA_character_, length(franchise_id)))
    }

    # Build lookup table from response
    result_lookup <- purrr::map_dfr(resp$data, function(item) {
        tibble::tibble(
            id = purrr::pluck(item, "data", "id", .default = NA_character_),
            title = purrr::pluck(item, "data", "title", .default = NA_character_)
        )
    })

    # Match input IDs to results (preserving order)
    matched <- result_lookup$title[match(franchise_id, result_lookup$id)]
    matched[is.na(matched)] <- NA_character_

    return(matched)
}



#' Lookup Title ID
#'
#' @description
#' Translates movie or TV show titles into FlixPatrol internal title IDs.
#'
#' @details
#' Queries the `/titles` endpoint using exact matching. This is vectorized,
#' allowing for multiple title lookups at once.
#'
#' @param title Character vector. The name of the movie or TV show.
#' @param silent Logical. If FALSE, prints success messages.
#'
#' @return A named character vector of title IDs.
#' @export
lookup_title <- function(title, silent = getOption("flixpatrol.silent", FALSE)) {

    token <- Sys.getenv("FLIX_PATROL")

    find_single_id <- function(name) {
        resp <- authenticate(site = "https://api.flixpatrol.com/v2/titles") |>
            httr2::req_url_query(`title[in]` = name) |>
            httr2::req_perform()

        body <- httr2::resp_body_json(resp)

        if (length(body$data) == 0) {
            cli::cli_abort("No title found matching: {.val {name}}")
        }

        # Extract ID from the first match
        id <- purrr::pluck(body, "data", 1, "data", "id", .default = NULL)

        if (!silent) {
            found_name <- purrr::pluck(body, "data", 1, "data", "title")
            cli::cli_alert_success("Found: {.val {found_name}} ({.val {id}})")
        }

        return(id)
    }

    purrr::map_chr(title, find_single_id)
}


#' Get Name from FlixPatrol ID
#'
#' @description
#' Retrieves human-readable names for FlixPatrol IDs. Vectorized and works
#' inside `dplyr::mutate()`.
#'
#' @details
#' Automatically detects the ID type from prefix (`cmp_`, `ttl_`, `frn_`) and
#' queries the appropriate endpoint. Returns `NA` for IDs that cannot be resolved.
#'
#' @param fp_id Character vector. FlixPatrol IDs (e.g., "ttl_xxx", "cmp_xxx", "frn_xxx").
#'
#' @return A character vector of names, with `NA` for failures.
#' @export
#'
#' @examples
#' \dontrun{
#' get_title_name("cmp_IA6TdMqwf6kuyQvxo9bJ4nKX")
#' get_title_name(c("cmp_xxx", "ttl_yyy", "frn_zzz"))
#' }
get_title_name <- function(fp_id) {

    token <- Sys.getenv("FLIX_PATROL")

    lookup_config <- list(
        "cmp" = list(path = "companies",  field = "name"),
        "ttl" = list(path = "titles",     field = "title"),
        "frn" = list(path = "franchises", field = "title")
    )

    get_single_name <- function(id) {
        id <- trimws(id)
        id_prefix <- substr(id, 1, 3)
        config <- lookup_config[[id_prefix]]

        if (is.null(config)) {
            return(NA_character_)
        }

        req <- httr2::request("https://api.flixpatrol.com/v2") |>
            httr2::req_url_path_append(config$path) |>
            httr2::req_url_path_append(id) |>
            httr2::req_auth_basic(username = token, password = "") |>
            httr2::req_retry(max_tries = 3) |>
            httr2::req_error(is_error = \(resp) FALSE)

        resp <- httr2::req_perform(req)

        if (httr2::resp_status(resp) != 200) {
            return(NA_character_)
        }

        body <- httr2::resp_body_json(resp)
        purrr::pluck(body, "data", config$field, .default = NA_character_)
    }

    purrr::map_chr(fp_id, get_single_name)
}
