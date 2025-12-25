## code to prepare `torrent` dataset goes here
library(tibble)

torrent_sites_tbl <- tribble(
    ~type,          ~data.id,                         ~torrent_site_name,          ~torrent_site_id,
    "torrentsites", "trs_IMeETdMqwf6kuyQvxo9bJ4nK",     "TorrentGalaxy",      17024,
    "torrentsites", "trs_V5I7c7OphiUds0Hgjbz5MESn",    "TorrentDownloads",   17025,
    "torrentsites", "trs_gKwnRaeXpyz2U9Q5tEMYDwri",    "Torlock",            17026,
    "torrentsites", "trs_Pt7MruDVbWTRq7LrfwOyYgjX",    "LimeTorrents",       17027,
    "torrentsites", "trs_bpfhGTvopBHPVtIKhR2CF68W",    "RARbg",              17028,
    "torrentsites", "trs_nl9xV46scBZ5bMKzLHDR3P2m",    "1337x",              17029,
    "torrentsites", "trs_oU3OgdpOrjIu3XzTEnWPt87Y",    "PirateBay",          17030,
    "torrentsites", "trs_Uc0WK0k2lQEVcb5uWXZtGhTg",    "eztv",               17036
) |> janitor::clean_names()

usethis::use_data(torrent_sites_tbl, overwrite = TRUE)
