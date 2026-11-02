## Resubmission

This is a resubmission of a new package. Version 1.0.0 was submitted on
2026-03-01 and was not published. In this version:

* Removed the redundant "R" from the title, which is now "Client for the
  'FlixPatrol' Streaming Rankings API".
* Added a link to the 'FlixPatrol' API documentation in the Description,
  in the form `<https://flixpatrol.com/api2/>`, and noted that an API key is
  required.
* Tests that make HTTP requests to the API now call `skip_on_cran()`, so the
  checks on CRAN make no network requests. They previously sent requests with
  a placeholder key, which made the tests slow and dependent on the network.
* Fixed an "undefined global function `:=`" NOTE by importing it from 'rlang'.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* Local: Pop!_OS 24.04 LTS, R 4.6.1

Checked with `devtools::check(remote = TRUE, manual = TRUE)`, with no API key
set, as on CRAN.

## Package purpose

This package provides an R interface to the 'FlixPatrol' API V2 for accessing
streaming platform rankings, viewing hours, and related metrics. The API
requires authentication with an API key, read from the `FLIX_PATROL`
environment variable.

## Notes

* Examples that call the API are wrapped in `\dontrun{}` because they cannot
  run without a valid API key.
* Tests that need the API are skipped on CRAN. Tests that need a real key also
  skip when no key is set.
* The package does not write to the user's home directory or modify global
  state.
