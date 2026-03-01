# CRAN Submission Comments

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* Local: Pop!_OS 22.04 LTS, R 4.5.2
* GitHub Actions (or similar CI): ubuntu-latest, macOS-latest, windows-latest

## Package purpose

This package provides an R interface to the FlixPatrol API V2 for accessing streaming platform rankings, viewing hours, and related metrics. The API requires authentication via an API key.

## Notes

* Examples that require API authentication use `\dontrun{}` since they cannot be executed without a valid API key.
* All tests that require the API use `skip_if_no_api_key()` to gracefully skip when no key is available.
* The package does not write to the user's home directory or modify global state.
