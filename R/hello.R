# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


authenticate <- function(site="https://api.flixpatrol.com/v2/top10s",token="FLIX_PATROL"){


    if(!is.character(token)){

        cli::cli_abort("{token} must be character")

    }


    if(!is.character(site)){

        cli::cli_abort("{site} must be character")

    }

    access_token <- Sys.getenv(token)

    if(!nchar(access_token)>1){

        cli::cli_abort("{token} must be set in your enviorment. Use {.code usethis::edit_r_environ()} to set your environment varibable to {.val FLIX_PATROL}")

    }

    req <- httr2::request(site) |>
        httr2::req_auth_basic(username = access_token, password = "")

    return(req)


}



#' Title
#'
#' @param country_name
#' @param token
#' @param silent
#'
#' @returns
#' @export
#'
#' @examples
lookup_country_and_return_id <- function(country_name, token = "FLIX_PATROL", silent = FALSE) {

    # Internal helper to find a single ID
    find_single_id <- function(name) {
        # The endpoint changes to /countries
        req <-
            authenticate(site="https://api.flixpatrol.com/v2/countries",token=token) |>
            req_url_query(`name[like]` = name)

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

    # Use map_chr to handle vectors (e.g., c("USA", "France"))
    ids <- purrr::map_chr(country_name, find_single_id)

    return(ids)
}



#' Title
#'
#' @param platform_name
#' @param token
#' @param silent
#'
#' @returns
#' @export
#'
#' @examples
lookup_platform_and_return_id <- function(platform_name,token="FLIX_PATROL",silent=FALSE){

    # platform_name <- "Netflix"

    # Internal helper to find a single ID
    find_single_id <- function(name) {

        req <-
            authenticate(site="https://api.flixpatrol.com/v2/companies",token=token) |>
            httr2::req_url_query(`name[like]` = name)

        resp <- req |> httr2::req_perform()
        body_lst <- resp |> httr2::resp_body_json()

        if (length(body_lst$data) == 0) {

            cli::cli_abort("No platform found matching: {.val {platform_name[1]*}}")
        }

        id <- body_lst$data[[1]]$data$id

        if(!silent){
            cli::cli_alert_success("Found: {.val {body_lst$data[[1]]$data$name}} {.val {id}}")
        }

        return(id)
    }

    # Use map_chr to run the helper for every name in your vector
    ids <- purrr::map_chr(platform_name, find_single_id)

    return(ids)

}
