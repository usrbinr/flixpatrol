
library(httr2)
library(tidyverse)
library(devtools)

load_all()
# get weekly winner query
# get daily top ten
# get weekly winners
# build prediction model



company_string <- lookup_platform_and_return_id(platform_name = c("netflix"))
flix_type_string <- lookup_flix_type_and_return_id("movies")
country_string <- lookup_country_and_return_id(country_name= c("United States"))
from_date <- "2025-12-15"
to_date <- "2025-12-21"
flix_type <- "Movies"


## hours vieews


create_hours_viewed_tbl <- function()


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





###

Show items 	GET
https://api.flixpatrol.com/v2/torrents

authenticate(site="https://api.flixpatrol.com/v2/torrents") |>
    httr2::req_url_query(




    )
