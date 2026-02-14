#' Date Type Lookup Table
#'
#' A tibble mapping date type names to FlixPatrol API IDs.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{date_type_name}{Character. The human-readable date type (e.g., "day", "week").}
#'   \item{date_type_id}{Integer. The internal API ID.}
#' }
"date_type_name"

#' Flix Type Lookup Table
#'
#' A tibble mapping content types to FlixPatrol API IDs.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{type_label}{Character. The content type label (e.g., "movies", "tv").}
#'   \item{type_id}{Integer. The internal API ID.}
#' }
"flix_type_tbl"

#' Language Lookup Table
#'
#' A tibble mapping language names to FlixPatrol API IDs.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{language_name}{Character. The language name (e.g., "english").}
#'   \item{language_id}{Integer. The internal API ID.}
#' }
"language_name"

#' Media Type Lookup Table
#'
#' A tibble mapping media type names to FlixPatrol API IDs.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{media_type_name}{Character. The media type name (e.g., "movie", "tv").}
#'   \item{media_type_id}{Integer. The internal API ID.}
#' }
"media_type_name"

#' Social Media Platform Lookup Table
#'
#' A tibble mapping social media platform names to FlixPatrol API IDs.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{social_media_name}{Character. The platform name (e.g., "Instagram").}
#'   \item{social_media_id}{Character. The internal API ID.}
#' }
"social_media_name"

#' Torrent Sites Lookup Table
#'
#' A tibble mapping torrent site names to FlixPatrol API IDs.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{torrent_site_name}{Character. The site name (e.g., "PirateBay", "1337x").}
#'   \item{torrent_site_id}{Character. The internal API ID.}
#' }
"torrent_sites"

#' Torrent Sites Lookup Table (Legacy)
#'
#' A tibble mapping torrent site names to FlixPatrol API IDs.
#' Legacy version kept for backwards compatibility.
#'
#' @format A tibble with 2 columns:
#' \describe{
#'   \item{torrent_site_name}{Character. The site name.}
#'   \item{torrent_site_id}{Character. The internal API ID.}
#' }
"torrent_sites_tbl"
