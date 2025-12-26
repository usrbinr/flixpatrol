
library(httr2)
library(tidyverse)
library(devtools)

load_all()
# get weekly winner query
# get daily top ten
# get weekly winners
# build prediction model



# official
# social fans
# franchise
# permieres

# daily views
# social fans?
## torrant isn't updated until end of the year or something
##


company_string <- lookup_platform_and_return_id(platform_name = c("netflix"))
flix_type_string <- lookup_flix_type_and_return_id("movies")
country_string <- lookup_country_and_return_id(country_name= c("United States"))
from_date <- "2025-12-15"
to_date <- "2025-12-21"
flix_type <- "Movies"





## offical ranking


authenticate(site="https://api.flixpatrol.com/v2/rankingsofficial")




## hours vieews

create_hours_viewed_tbl(language=1,start_date="2025-12-15",end_date="2025-12-21")


body_lst |> glimpse()

    pluck("data") |>
    as_tibble()
    rename(language = `$language`)
    rename(
        report_type=type
        ,languge="\$language"
    )

  hours_view_lst <-   body_lst |>
        resp_body_json() |>
        pluck("data") |>
        pluck(1)


  hours_view_lst
        glimpse()

    # arrange(-data$views) |>
        # head(100)


### get torrent sites



devtools::document()
torrent_id_vec <- lookup_torrent_site_and_return_id(x = "*")
###

authenticate(site="https://api.flixpatrol.com/v2/torrents") |>
        httr2::req_url_query(
            # `id[eq]`="trt_CfX89vcTOtjqMu0ng6w2QIfD",
            # `company[eq]` = company_string,
            # `country[eq]` = country_string,
            `site[eq]`    = torrent_id_vec[1],
            # `type[eq]`=media_type_vec,
            `date[type][eq]` = 1,           # 1 = Daily Chart
            `date[from][eq]`= "2023-12-01",
            `date[to][eq]` = "2023-12-01"
        ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    as_tibble()

