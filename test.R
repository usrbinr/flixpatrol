
library(httr2)
library(tidyverse)
library(devtools)


document()

top10_usa_netflix <- authenticate() |>
    httr2::req_perform() |>
    resp_body_json()


top10_usa_netflix



create_daily_top_ten_tbl <- function(token,platform_name,country_name,date,type)


    company_string <- lookup_platform_and_return_id(platform_name = platform_name,token=token,silent=TRUE)
    country_string <- lookup_country_and_return_id(country_name = country_name,token=token,silent=TRUE)

x <- authenticate(token = token) |>
    req_url_query(
        `company[eq]` = company_string,
        `country[eq]` = country_string,
        `type[eq]`=2,
        `date[type][eq]` = 1,           # 1 = Daily Chart
        `date[from][eq]` = date,
        `date[to][eq]` = date
    ) |>
    req_perform() |>
    resp_body_json()


# top10_table <-
    x$data |>
    map_df(function(item) {
        # Extracting the inner 'data' block
        d <- item$data

        tibble(
            rank        = pluck(d, "ranking", .default = NA_integer_),
            title       = pluck(d, "movie", "data", "title", .default = pluck(d, "note")),
            type_id     = pluck(d, "type", .default = NA_integer_),
            days_total  = pluck(d, "daysTotal", .default = NA_integer_),
            rank_last   = pluck(d, "rankingLast", .default = NA_integer_),
            date        = pluck(d, "date", "from", .default = NA_character_),
            imdb_id     = pluck(d, "movie", "data", "imdbId", .default = NA_integer_)
        )
    })


usethis::use_data_raw("type")

netflix_string <- lookup_platform_and_return_id(platform_name = c("netflix"))

usa_string <- lookup_country_and_return_id(country_name= c("United States"))

flix_type <- "Movies"
lookup_flix_type_and_return_id <- function(flix_type){

    validate_types <- flixpatrol::flix_type$type_label


    flixpatrol::flix_type$type_id[flixpatrol::flix_type$type_label %in% flix_type]

}


lookup_flix_type_and_return_id(flix_type = flix_type)
