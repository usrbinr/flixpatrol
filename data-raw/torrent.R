## code to prepare `torrent` dataset goes here

name <- ""

body_lst <-
    authenticate(site="https://api.flixpatrol.com/v2/torrentsites",token="FLIX_PATROL") |>
    httr2::req_url_query(`name[like]` = name) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)


torrent_sites <- tibble::tibble(
    torrent_site_name=body_lst$data$data$name
    ,torrent_site_id=body_lst$data$data$id
)

usethis::use_data(torrent_sites, overwrite = TRUE)
