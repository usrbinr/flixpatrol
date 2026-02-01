## code to prepare `country_name` dataset goes here
## These are cached to avoid hitting the API for common country lookups.
## To regenerate with real IDs, use lookup_country_and_return_id() for each.

library(tibble)

country_name <- tribble(
  ~country_name,       ~country_id,
  "united states",     "ctr_US",
  "united kingdom",    "ctr_GB",
  "canada",            "ctr_CA",
  "australia",         "ctr_AU",
  "germany",           "ctr_DE",
  "france",            "ctr_FR",
  "brazil",            "ctr_BR",
  "india",             "ctr_IN",
  "japan",             "ctr_JP",
  "south korea",       "ctr_KR",
  "mexico",            "ctr_MX",
  "spain",             "ctr_ES",
  "italy",             "ctr_IT",
  "netherlands",       "ctr_NL",
  "sweden",            "ctr_SE",
  "poland",            "ctr_PL",
  "argentina",         "ctr_AR",
  "turkey",            "ctr_TR",
  "colombia",          "ctr_CO",
  "south africa",      "ctr_ZA"
)

usethis::use_data(country_name, overwrite = TRUE)
