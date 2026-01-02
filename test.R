
library(httr2)
library(tidyverse)
library(devtools)
devtools::document()
load_all()

franchise_tbl <- body_lst$data.data$franchise$data |> as_tibble() |> count(id)
get_company_name("cmp_bBrpGTvopBHPVtIKhR2CF68W")
get_franchise_name("frn_KGBes9dtvkV710raDBpXoKRc")
get_franchise_name_safe <- purrr::possibly(get_franchise_name,otherwise = NA)
get_company_name("ttl_yPCJU2UzROTNVv5JZ7Hu4m8M")


out <- franchise_tbl |>
  rowwise() |>
  mutate(
    name=list(get_franchise_name_safe(id))
  )

# test <-
  out |>
  unnest(name) |> view()


test$name
lookup_franchise_and_return_id()
# top fan sites
# franchise look up
# company look up
# movie look up

# get weekly winners
# build prediction model
# get franchise name



# official
# social fans
# franchise
# permieres

# daily views
# social fans?
## torrant isn't updated until end of the year or something
##



##franchise

authenticate("https://api.flixpatrol.com/v2/franchises") |>
    httr2::req_url_query(
        `title[like]`=''
        # `country[eq]`=country_id
        # `company[eq]`="cmp_JeAe7ZCyHJKz3fYOrtq6En4s"
        # `movie[eq]`="frn_97nIMYOCvD6zprSPoHgTJauB"
        # ,`language[eq]`=language_id
        # ,`number[eq]`=media_type_id
        # `date[type][eq]`=1
        # ,`date[to][eq]`=start_date
        # ,`date[to][eq]`=start_date
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    as.data.frame() |>
    as_tibble() |>
    view()





usethis::edit_r_environ()
create_fans_site_ranking_tbl <- function(token="FLIX_PATROL",platform_name="netflix",country_name="USA",start_date,end_date,media_type,date_type="week",language){


  #test

  country_name="United States"
  start_date="2025-12-15"
  end_date="2025-12-21"
  media_type="Movie"
  date_type="week"
  language="English"



    platform_valid_name <- "netflix"

    if(!all(tolower(platform_name) %in% platform_valid_name)){

        cli::cli_abort("Currently flixpatrol only supports Netflix for the official top ten list, you spplied {platform_name}")

    }


    platform_id <- lookup_platform_and_return_id(platform_name = platform_name)
    media_type_id <- lookup_media_type_name_and_return_id(media_type=media_type)
    country_id <- lookup_country_and_return_id(country_name= c("United States"))
    date_type_id <- lookup_date_type_name_and_return_id("day")
    language_id <- lookup_language_name_and_return_id("english")



    validate_date_range(start_date = start_date,end_date = end_date,start_date_wday_abb = "mon",range_length = 7)
    ## offical ranking


    body_lst <-
        authenticate(site="https://api.flixpatrol.com/v2/fans") |>
        httr2::req_url_query(
            # `country[eq]`=country_id
            # `company[eq]`="cmp_JeAe7ZCyHJKz3fYOrtq6En4s"
            # `movie[eq]`="frn_97nIMYOCvD6zprSPoHgTJauB"
            # ,`language[eq]`=language_id
            # ,`number[eq]`=media_type_id
            `date[type][eq]`=1
            ,`date[to][eq]`=start_date
            ,`date[to][eq]`=start_date
        ) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE) |>
            as.data.frame() |>
        tibble::as_tibble()
          view()


        body_lst |> view()
        purrr::pluck("data")



  usethis::use_r("utils-api")

    out <- body_lst |>
        tibble(raw=_) |>
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

