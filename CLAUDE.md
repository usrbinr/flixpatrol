# FlixPatrol R Package

## API Reference

FlixPatrol API documentation: https://flixpatrol.com/api2/

### Query Parameter Operators

The API supports these filter operators:
- `[eq]` - exact match
- `[in]` - match any in list
- `[nin]` - not in list

The API does NOT support `[like]` for fuzzy matching.

### Authentication

All API requests use HTTP Basic Auth with the API key stored in the `FLIX_PATROL` environment variable.

### Endpoint Notes

- `/rankingsofficial` - Netflix only, requires Monday-Sunday date ranges
- `/hoursviewed` - Requires Monday-Sunday weekly date ranges
- `/top10s` - Daily data, limited to 300 records per request (chunk by 30 days)

## Code Conventions

### Parameter Naming

- Use `media_type` (not `flix_type`) - accepts "movie" or "tv_show"
- Use `platform_name` (not `company`) - maps to `/companies` endpoint internally
- Default to singular form: `"movie"` not `"movies"`

### File Organization

- `R/hello.R` - Core API functions, lookups, authentication
- `R/utils-api.R` - ID-to-name resolution functions
- `R/utils-new.R` - Analytics/comparison functions

## Build Commands

```r
# Documentation
devtools::document()
qrtdown::build_site()

# Check
devtools::check(remote = TRUE, manual = TRUE)
```
