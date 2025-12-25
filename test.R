
library(httr2)
library(tidyverse)
library(devtools)

load_all()
# get weekly winner query
# get daily top ten
# get weekly winners
# build prediction model


create_daily_top_ten_tbl(token="FLIX_PATROL",platform_name = "netflix",country_name = "Mexico",start_date="2025-12-01",end_date = "2025-12-25",flix_type = "movies")


devtools::document()

create_daily_top_ten_tbl <- function(token="FLIX_PATROL",platform_name,country_name,start_date,end_date,flix_type){


    # test variables
    start_date="2025-12-01"
    end_date="2025-12-21"

    date_vec <- seq.Date(from = start_date,to=end_date)


   out <-  purrr::map(.x=date_vec,.f = \(x) create_single_daily_top_ten_tbl(token=token,platform_name=platform_name,country_name=country_name,date=x,flix_type=flix_type) ) |>
        purrr::list_rbind()

   return(out)




}



create_top_n_tbl <- function(token="FLIX_PATROL",platform_name,country_name,date,flix_type,n){


    if(!is.numeric(n)){

        cli::cli::cli_abort("{n} must be numeric")
    }


    purrr::map(
        .x=date_list
        ,.f=\(x) create_daily_top_ten_tbl(token=token,platform_name = platform_name,country_name = country_name,date = date,flix_type = flix_type) |>
            dplyr::filter(
                dplyr::row_number()==n
            )
    ) |>
        purrr::list_rbind()



}


date_list <- seq.Date(from="2025-12-22",to = "2025-12-24")
usethis::use_data_raw("type")

company_string <- lookup_platform_and_return_id(platform_name = c("netflix"))
flix_type_string <- lookup_flix_type_and_return_id("movies")
country_string <- lookup_country_and_return_id(country_name= c("United States"))
from_date <- "2025-12-15"
to_date <- "2025-12-21"

flix_type <- "Movies"


## hours vieews


body_lst <- authenticate(site="https://api.flixpatrol.com/v2/hoursviewed") |>
    req_url_query(
        `company[eq]` = company_string,
        `language[eq]`=1,
        # `country[eq]` = country_string,
        `number[eq]`  = 1,
        # `ranking[eq]`=1,
        # `type[eq]`=flix_type_string,
        `date[type][eq]` = 3,           # 1 = Daily Chart
        `date[from][eq]` = from_date,
        `date[to][eq]` = to_date
    ) |>
    req_perform()


body_lst |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("data") |>
    as_tibble() |>
    arrange(-data$views) |>
        head(100) |> view()


### get torrent sites

lookup_torrent_site_and_return_id <- function(torrent_site){


    torrent_site <- "torlock"
    torrent_site_lower <- tolower(torrent_site)

    if(all(torrent_site=="*")){

        torrent_site <- tolower(flixpatrol::torrent_sites_tbl$torrent_site_name)
    }

    out <- flixpatrol::torrent_sites_tbl$torrent_site_id[tolower(flixpatrol::torrent_sites_tbl$torrent_site_name) %in% c(torrent_site_lower)]


    return(out)
}




# Internal helper to find a single ID
find_single_id <- function(name) {

    name <- ""
    req <-
        authenticate(site="https://api.flixpatrol.com/v2/torrentsites",token=token) |>
        httr2::req_url_query(`name[like]` = name)

    resp <- req |> httr2::req_perform()
    body_lst <- resp |> httr2::resp_body_json(simplifyVector = TRUE)

    body_lst |>

    # body_lst |>

    if (length(body_lst$data) == 0) {

        cli::cli_abort("No platform found matching: {.val {platform_name}}")
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




###

Show items 	GET
https://api.flixpatrol.com/v2/torrents

authenticate(site="https://api.flixpatrol.com/v2/torrents") |>
    httr2::req_url_query(



    )
