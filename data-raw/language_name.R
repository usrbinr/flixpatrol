## code to prepare `language_name` dataset goes here


language_name <- tibble::tribble(
    ~language_name,     ~language_id,
    "english",     1,
    "non_english", 2
)

usethis::use_data(language_name, overwrite = TRUE)





media_type_name <- tribble(
    ~media_type_name, ~media_type_id,
    "movie",1,
    "tv_show",2
)




usethis::use_data(media_type_name, overwrite = TRUE)



date_type_name <- tribble(
    ~date_type_name, ~date_type_id,
    "day",        1,
    "week",       3,
    "month",      4,
    "quarter",    6,
    "half_year",   7,
    "year",       5,
    "first_month", 14
)

usethis::use_data(date_type_name, overwrite = TRUE)




library(tibble)

# Creating the platform mapping table
social_media_name <- tibble::tribble(
    ~social_media_name,  ~social_media_id,
    "Instagram", "cmp_Di2u9cvFOpmwE3Zhn1SozXJN",
    "Facebook",  "cmp_JeAe7ZCyHJKz3fYOrtq6En4s",
    "Twitter",   "cmp_bBrpGTvopBHPVtIKhR2CF68W"
)

usethis::use_data(social_media_name, overwrite = TRUE)

