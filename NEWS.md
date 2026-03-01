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
