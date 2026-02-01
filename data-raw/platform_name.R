## code to prepare `platform_name` dataset goes here
## These are cached to avoid hitting the API for common platform lookups.
## To regenerate, run: source("data-raw/platform_name.R")

library(tibble)

platform_name <- tribble(
  ~platform_name,    ~platform_id,
  "netflix",         "cmp_IA6TdMqwf6kuyQvxo9bJ4nKX",
  "disney+",         "cmp_8mVDCjGKriSFQkGYMAFqPn50",
  "hbo max",         "cmp_Di2u9cvFOpmwE3Zhn1SozXJN",
  "amazon prime",    "cmp_JeAe7ZCyHJKz3fYOrtq6En4s",
  "apple tv+",       "cmp_bBrpGTvopBHPVtIKhR2CF68W",
  "hulu",            "cmp_5kGTPXwRSZCW8N3f7EhD2qYJ",
  "paramount+",      "cmp_9tRFVXwKSZCW8N3f7EhD2qYJ"
)

usethis::use_data(platform_name, overwrite = TRUE)
