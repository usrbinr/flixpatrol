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
#' This function queries the `/franchises` endpoint. It supports fuzzy matching
#' via the API's `title[like]` parameter. It is vectorized via `purrr::map_chr`,
#' allowing you to search for multiple franchises at once.
#'
#' @param franchise_title Character vector. The title(s) of the franchises to lookup.
#' @param token Character. The name of the environment variable containing the API key.
#' @param silent Logical. If `FALSE` (default), prints success messages using `cli`.
#'
#' @return A named character vector of franchise IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' lookup_franchise_and_return_id("Indiana Jones")
#' # [1] "frn_STNa2GEw54ahWxJsXNgBMmt0"
#' }
lookup_franchise_and_return_id <- function(franchise_title, token = "FLIX_PATROL", silent = FALSE) {

    # Internal helper to find a single ID via API
    find_single_id <- function(name) {

        resp <- authenticate(site = "https://api.flixpatrol.com/v2/franchises", token = token) |>
            httr2::req_url_query(`title[like]` = name) |>
            httr2::req_perform()

        body_lst <- httr2::resp_body_json(resp)

        # Check if any data was returned
        if (length(body_lst$data) == 0) {
            cli::cli_abort("No franchise found matching: {.val {name}}")
        }

        # Extract ID from the first match
        # Structure based on docs: body_lst$data[[1]]$data$id
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
#' visualization.
#'
#' @param franchise_id Character. The FlixPatrol franchise ID (e.g., "frn_STNa2GEw54ahWxJsXNgBMmt0").
#' @param token Character. The name of the environment variable containing the API key.
#'
#' @return A character string containing the franchise title.
#' @export
#'
#' @examples
#' \dontrun{
#' get_franchise_name("frn_STNa2GEw54ahWxJsXNgBMmt0")
#' # [1] "Indiana Jones"
#' }
get_franchise_name <- function(franchise_id, token = "FLIX_PATROL") {

    if (missing(franchise_id) || !is.character(franchise_id)) {
        cli::cli_abort("{.arg franchise_id} must be a character string.")
    }

    # 1. Build the specific URL for this ID
    # Path format: https://api.flixpatrol.com/v2/franchises/:id
    endpoint_url <- paste0("https://api.flixpatrol.com/v2/franchises/", franchise_id)

    # 2. Authenticate and Perform Request
    # Reuses your package's authenticate() helper
    resp <- authenticate(site = endpoint_url, token = token) |>
        httr2::req_perform()

    # 3. Parse Response
    body <- httr2::resp_body_json(resp)

    # 4. Extract Title
    # According to docs, the title is at: body$data$title
    franchise_title <- purrr::pluck(body, "data", "title", .default = NULL)

    if (is.null(franchise_title)) {
        cli::cli_abort("Could not find a franchise title for ID: {.val {franchise_id}}")
    }

    return(franchise_title)
}



#' Lookup Title ID
#'
#' @description
#' Translates movie or TV show titles into FlixPatrol internal title IDs.
#'
#' @details
#' Queries the `/titles` endpoint using fuzzy matching (`title[like]`).
#' This is vectorized, allowing for multiple title lookups at once.
#'
#' @param title Character vector. The name of the movie or TV show.
#' @param token Character. The API token environment variable name.
#' @param silent Logical. If FALSE, prints success messages.
#'
#' @return A named character vector of title IDs.
#' @export
lookup_title_id <- function(title, token = "FLIX_PATROL", silent = FALSE) {

    find_single_id <- function(name) {
        resp <- authenticate(site = "https://api.flixpatrol.com/v2/titles", token = token) |>
            httr2::req_url_query(`title[like]` = name) |>
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


#' Get Title Name from ID
#'
#' @description
#' Retrieves the official movie or TV show title associated with a FlixPatrol ID.
#'
#' @details
#' Accesses the specific resource path `/titles/:id`. Useful for cleaning
#' up datasets where only IDs are present.
#'
#' @param id_prefix Character. The FlixPatrol title ID (e.g., "ttl_bHyGTvopBHPVtIKhR2CF68WD").
#' @param token Character. API environment variable name.
#'
#' @return A character string containing the title name.
#' @export
get_title_name <- function(fp_id, token = "FLIX_PATROL") {

    # 1. Clean the input and detect prefix
    fp_id <- trimws(fp_id)
    id_prefix <- substr(fp_id, 1, 3)

    # 2. Configuration mapping
    lookup_config <- list(
        "cmp" = list(path = "companies",  field = "name"),
        "ttl" = list(path = "titles",     field = "title"),
        "frn" = list(path = "franchises", field = "title")
    )

    config <- lookup_config[[id_prefix]]

    if (is.null(config)) {
        cli::cli_abort("Unknown ID prefix: {.val {id_prefix}}. Must be cmp_, ttl_, or frn_.")
    }

    # 3. Use httr2 to build the request safely
    # Start with the base API URL
    req <- httr2::request("https://api.flixpatrol.com/v2") |>
        # Append the path segments correctly: /v2/{path}/{id}
        httr2::req_url_path_append(config$path) |>
        httr2::req_url_path_append(fp_id) |>
        # Apply your existing authentication logic
        httr2::req_auth_basic(username = Sys.getenv(token), password = "") |>
        # Add a retry mechanism in case of temporary 404s/network blips
        httr2::req_retry(max_tries = 3)

    # 4. Perform Request with Error Handling
    resp <- req |>
        httr2::req_error(is_error = \(resp) FALSE) |> # Catch the error manually to debug
        httr2::req_perform()

    # Debugging: If it's still a 404, let's see exactly which URL was called
    if (httr2::resp_status(resp) == 404) {
        cli::cli_abort(c(
            "x" = "API returned 404 Not Found.",
            "i" = "Requested URL: {.url {resp$url}}",
            "!" = "Double-check that ID {.val {fp_id}} exists in the {.val {config$path}} database."
        ))
    }

    body <- httr2::resp_body_json(resp)

    # 5. Extract the result
    result_name <- purrr::pluck(body, "data", config$field, .default = NULL)

    if (is.null(result_name)) {
        cli::cli_abort("ID found, but field {.val {config$field}} is missing in the API response.")
    }

    return(result_name)
}
