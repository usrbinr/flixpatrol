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



#' Title
#'
#' @param flix_type
#'
#' @returns
#' @export
#'
#' @examples
lookup_flix_type_and_return_id <- function(flix_type){

    flix_type_lower <- tolower(flix_type)

    validate_flix_types <- flixpatrol::flix_type_tbl$type_label

    if(!any(flix_type_lower %in% validate_flix_types)){

        cli::cli_abort("Please ensure {.val {flix_type}} is one of {.val {validate_flix_types}}")
    }


    flixpatrol::flix_type$type_id[flixpatrol::flix_type$type_label %in% flix_type_lower]

}



#' Title
#'
#' @param token
#' @param platform_name
#' @param country_name
#' @param date
#' @param flix_type
#'
#' @returns tibble
#' @keywords internal
create_single_daily_top_ten_tbl <- function(token="FLIX_PATROL",platform_name,country_name,date,flix_type){


    company_string        <- lookup_platform_and_return_id(platform_name = platform_name,token=token,silent=TRUE)
    country_string        <- lookup_country_and_return_id(country_name = country_name,token=token,silent=TRUE)
    flix_type_string      <- lookup_flix_type_and_return_id(flix_type = flix_type)


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



#' Title
#'
#' @param token
#' @param platform_name
#' @param country_name
#' @param start_date
#' @param end_date
#' @param flix_type
#'
#' @returns
#' @export
#'
#' @examples
create_daily_top_ten_tbl <- function(token="FLIX_PATROL",platform_name,country_name,start_date,end_date,flix_type){


    # test variables
    # start_date="2025-12-01"
    # end_date="2025-12-21"

    date_vec <- seq.Date(from = start_date,to=end_date)


   out <-  purrr::map(.x=date_vec,.f = \(x) create_single_daily_top_ten_tbl(token=token,platform_name=platform_name,country_name=country_name,date=x,flix_type=flix_type) ) |>
        purrr::list_rbind()

   return(out)




}

