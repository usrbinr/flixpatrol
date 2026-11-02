# flixpatrol 1.1.0

## Breaking changes
* TV content is now labelled `"tvshow"` instead of `"tv_show"` in the `type`
  column of every function's output. Inputs still accept `"tv"`, `"tv show"`,
  `"tv_show"`, `"tvshow"` and their plurals.
* `validate_date_format()` is no longer exported.
* Removed `get_franchise_performance()`. The API's franchise response does not
  list a franchise's titles, so the function could only return an empty result.

## New features
* `country_name = "Global"` covers about 45 countries in `lookup_country()`,
  `get_top_ten()` and `get_official_ranking()`. Both add a `country` column.
  Each country is a separate set of requests, so a run can take many minutes:
  a message at the start gives the minimum time, and a warning at the end lists
  any country that failed or returned no rows, with the reason.

## Bug fixes
* `"TV Show"` and `"tv shows"` (with a space) were not recognised as TV content.
* `get_top_ten()` now warns when a request fails, instead of stopping with an
  unrelated error.
* `get_torrent_ranking()` filters `media_type` after the request, because the
  API does not support filtering torrents by type.

## Other changes
* The title is now "Client for the 'FlixPatrol' Streaming Rankings API", and
  the Description links to the API documentation.
* Tests that call the API are skipped on CRAN, so CRAN checks make no network
  requests.
* `:=` is imported from 'rlang'.

# flixpatrol 1.0.0
* updated documentation 
* updated syntax and harmonized input fields

# flixpatrol 0.2.0

## Breaking Changes
* Unified `flix_type` and `media_type` parameters into single `media_type` parameter across all functions. Use `media_type = "movie"` or `media_type = "tv_show"`.

## New Features
* Added `normalize_content_type()` helper to handle various content type inputs (e.g., "tv", "tvshow", "tv_show" all resolve correctly).
* Added `resolve_content_type()` internal function to map content types to API IDs.
* Added `lookup_social_platform()` for social media platform ID lookup.
* Added query default options: `flixpatrol.platform_name`, `flixpatrol.country_name`, `flixpatrol.start_date`, `flixpatrol.end_date`, `flixpatrol.media_type`, `flixpatrol.language`, `flixpatrol.social_platform`, `flixpatrol.torrent_site`.

## Bug Fixes
* Fixed `get_official_ranking()` to correctly extract title and title_id from API response after FlixPatrol API fix.

# flixpatrol 0.1.0

* Initial release.
* Core lookup functions: `lookup_country()`, `lookup_platform()`, `lookup_franchise()`, `lookup_title()`.
* Data retrieval: `get_top_ten()`, `get_hours_viewed()`, `get_official_ranking()`, `get_fans_ranking()`, `get_torrent_ranking()`, `get_premieres()`, `get_title_details()`.
* Analytics: `compare_platforms()`, `get_title_history()`, `get_weekly_movers()`, `get_top_titles_summary()`, `get_global_ranking()`, `get_franchise_performance()`.
* Package utilities: `flixpatrol_options()`, `flixpatrol_sitrep()`, `get_quota()`.
