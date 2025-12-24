## code to prepare `type` dataset goes here


flix_type_tbl <- tibble::tribble(
    ~type_id, ~type_label,
    1,        "overall",
    2,        "movies",
    3,        "tv_shows",
    40,       "kids_overall",
    49,       "kidsmovies",
    50,       "kids_tv_shows",
    52,       "documentaries",
    53,       "reality_shows",
    54,       "entertainment_shows",
    55,       "tv_movies",
    57,       "rental_movies",
    58,       "anime",
    59,       "anime_movies"
)


usethis::use_data(flix_type_tbl, overwrite = TRUE)
