

#' Title
#'
#' @param site
#' @param token
#'
#' @returns
#' @export
#'
#' @examples
authenticate <- function(site="https://api.flixpatrol.com/v2/top10s",token="FLIX_PATROL"){


    if(!is.character(token)){

        cli::cli_abort("{token} must be character")

    }


    if(!is.character(site)){

        cli::cli_abort("{site} must be character")

    }

    access_token <- Sys.getenv(token)

    if(!nchar(access_token)>1){

        cli::cli_abort("{token} must be set in your enviorment. Use {.fn usethis::edit_r_environ()} to set your environment varibable to {.val FLIX_PATROL}")

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

      body_lst <-
            authenticate(site="https://api.flixpatrol.com/v2/companies",token=token) |>
            httr2::req_url_query(
              `name[like]` = name
              ) |>
          httr2::req_perform() |>
          httr2::resp_body_json()

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



#' Title
#'
#' @param torrent_site
#'
#' @returns
#'
#' @examples
lookup_torrent_site_and_return_id <- function(x){

  # torrent_site <- "torlock"
  name_lower <- tolower(x)

  if(all(x=="*")){

    name_lower <- tolower(flixpatrol::torrent_sites$torrent_site_name)
  }

  out <- flixpatrol::torrent_sites$torrent_site_id[tolower(flixpatrol::torrent_sites$torrent_site_name) %in% c(name_lower)]


  return(out)
}


#' Title
#'
#' @param language_name
#'
#' @returns
#' @export
#'
#' @examples
lookup_language_name_and_return_id <- function(language_name){

  # torrent_site <- "torlock"
  language_name_lower <- tolower(language_name)

  valid_names <- unique(flixpatrol::language_name$language_name)

  if(!all(language_name_lower %in% valid_names)){

    cli::cli_abort("language name must be one of {.val {valid_names}} not {.val {language_name}}")


  }

  if(all(torrent_site=="*")){

    language_name_lower <- tolower(flixpatrol::language_name$language_name)
  }

  out <- flixpatrol::language_name$language_id[tolower(flixpatrol::language_name$language_name) %in% c(language_name_lower)]


  return(out)
}




#' Title
#'
#' @param media_type_name
#'
#' @returns
#' @export
#'
#' @examples
lookup_media_type_name_and_return_id <- function(media_type_name){

  # torrent_site <- "torlock"
  media_type_name_lower <- tolower(media_type_name)

  valid_names <- unique(flixpatrol::media_type_name$media_type_name)

  if(!all(media_type_name_lower %in% valid_names)){

    cli::cli_abort("language name must be one of {.val {valid_names}} not {.val {media_type_name}}")


  }

  if(all(media_type_name=="*")){

    media_type_name_lower <- tolower(flixpatrol::media_type_name$media_type_name)
  }

  out <- flixpatrol::media_type_name$media_type_id[tolower(flixpatrol::media_type_name$media_type_name) %in% c(media_type_name_lower)]


  return(out)
}

#' Title
#'
#' @param media_type_name
#'
#' @returns
#' @export
#'
#' @examples
lookup_date_type_name_and_return_id <- function(x){

  # torrent_site <- "torlock"
  name_lower <- tolower(x)

  valid_names <- unique(flixpatrol::date_type_name$date_type_name)

  if(!all(name_lower %in% valid_names)){

    cli::cli_abort("date_type_name must be one of {.val {valid_names}} not {.val {x}}")


  }

  if(all(x=="*")){

    name_lower <- tolower(flixpatrol::date_type_name$date_type_name)
  }

  out <- flixpatrol::date_type_name$date_type_id[tolower(flixpatrol::date_type_name$date_type_name) %in% c(name_lower)]


  return(out)

}


#' Title
#'
#' @param start_date
#' @param end_date
#' @param start_date_wday_abb
#' @param range_length
#'
#' @returns logical
#' @export
validate_date_range <- function(start_date,end_date,start_date_wday_abb,range_length=7){

  valid_wday_abb <- c("mon","tue","wed","thu","fri","sat","sun")

  if(!any(tolower(start_date_wday_abb) %in% valid_wday_abb)){

    cli::cli_abort("start_date_wday_abb must be one of {.val {valid_wday_abb}} not {.val {start_date_wday_abb}}")
  }

  if(!all(tolower(wday(start_date,label = TRUE))==start_date_wday_abb)){

    cli::cli_abort("{start_date} must start on {start_date_wday_abb} not {lubridate::wday(start_date,label=TRUE)}")

  }

  date_vec <- seq.Date(from=start_date,to=end_date)

  if(!all(length(date_vec)==range_length)){

    cli::cli_abort("{start_date} and {end_date} must be {.val {range_length}} days apart")


  }

  return(TRUE)


}









#' Title
#'
#' @param token
#' @param start_date
#' @param end_date
#' @param platform_name
#' @param media_type_name
#' @param language_name
#'
#' @returns tibble
#' @export
#'
create_hours_viewed_tbl <- function(token="FLIX_PATROL",platform_name="netflix",media_type_name,language_name,start_date,end_date){

  # test
#
#   platform_name="netflix"
#   media_type_name="Movie"
#   language_name="English"
#   start_date="2025-12-15"
#   date_type_name <- "Week"
#   end_date="2025-12-21"

  platform_name_vec <-  lookup_platform_and_return_id(platform_name = platform_name,token=token,silent=TRUE)
  language_name_vec <-  lookup_language_name_and_return_id(language_name = language_name)
  media_type_vec    <-  lookup_media_type_name_and_return_id(media_type_name = media_type_name)
  # date_type_vec    <-  lookup_date_type_name_and_return_id(x = date_type_name)
  validate_date_range(start_date = start_date,end_date = end_date,start_date_wday_abb = "mon",range_length = 7)



  body_lst <-
    authenticate(token=token,site="https://api.flixpatrol.com/v2/hoursviewed") |>
    httr2::req_url_query(
      `company[eq]` = platform_name_vec,
      `language[eq]`=language_name_vec,
      `number[eq]`=media_type_vec,
      `date[type][eq]` = 3,
      `date[from][eq]` = start_date,
      `date[to][eq]` = end_date
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("data")



  out <-
    body_lst |>
    tibble::tibble(raw=_) |>
    tidyr::unnest_wider(raw) |>
    tidyr::unnest_wider(data) |>
    tidyr::unnest_wider(movie, names_sep = "_") |>
    tidyr::unnest_wider(company, names_sep = "_") |>
    tidyr::unnest_wider(date, names_sep = "_") |>
    tidyr::unnest_wider(movie_data) |>
    tidyr::unnest_wider(movie_legacy,names_sep="_") |>
    tidyr::unnest_wider(company_data,names_sep="_") |>
    tidyr::unnest_wider(company_legacy,names_sep="_") |>
    dplyr::rename(
      hours_view=value
    )

  return(out)

}




#' Title
#'
#' @param token
#' @param platform_name
#' @param country_name
#' @param start_date
#' @param end_date
#' @param media_type
#' @param date_type
#' @param language_id
#'
#' @returns
#' @export
#'
#' @examples
create_official_ranking_tbl <- function(token,platform_name="netflix",country_name,start_date,end_date,media_type,date_type="week",language_id){

  platform_valid_name <- "netflix"

  if(!all(tolower(platform_name) %in% platform_valid_name)){

    cli::cli_abort("Currently flixpatrol only supports Netflix for the official top ten list, you spplied {platform_name}")

  }

  platform_id <- lookup_platform_and_return_id(platform_name = platform_name)
  media_type_id <- lookup_media_type_name_and_return_id(media_type=media_type)
  country_id <- lookup_country_and_return_id(country_name= c("United States"))
  date_type_id <- lookup_date_type_name_and_return_id("week")
  language_id <- lookup_language_name_and_return_id("english")



  validate_date_range(start_date = start_date,end_date = end_date,start_date_wday_abb = "mon",range_length = 7)
  ## offical ranking

  body_lst <- authenticate(site="https://api.flixpatrol.com/v2/rankingsofficial") |>
    httr2::req_url_query(
      `country[eq]`    =country_id
      ,`company[eq]`   =platform_id
      ,`language[eq]`  =language_id
      ,`number[eq]`    =media_type_id
      ,`date[type][eq]`=date_type_id
      ,`date[to][eq]`  =start_date
      ,`date[to][eq]`  =end_date
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("data")


  out <- body_lst |>
    tibble::tibble(raw=_) |>
    tidyr::unnest_wider("raw") |>
    tidyr::unnest_wider("data") |>
    tidyr::unnest_wider("movie",names_sep = "_") |>
    tidyr::unnest_wider("movie_data") |>
    tidyr::unnest_wider("company",names_sep = "_") |>
    tidyr::unnest_wider("company_data",names_sep="_") |>
    tidyr::unnest_wider("company_legacy",names_sep="_") |>
    tidyr::unnest_wider("country",names_sep="_") |>
    tidyr::unnest_wider("country_data",names_sep="_") |>
    tidyr::unnest_wider("country_legacy",names_sep="_") |>
    tidyr::unnest_wider("date",names_sep = "_")



  return(out)

}
