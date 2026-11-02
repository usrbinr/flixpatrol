# No network: fetch_one is a stub, and HTTP responses are mocked, so these run
# on CRAN.

stub_fetch <- function(cn) {
  switch(cn,
    fails = cli::cli_abort("HTTP 500 Internal Server Error."),
    empty = tibble::tibble(),
    partial = {
      cli::cli_warn("Failed Top 10 requests: 1 of 2.")
      tibble::tibble(rank = 1L)
    },
    tibble::tibble(rank = 1:2)
  )
}

describe("fetch_global()", {
  it("keeps good rows and names each gap with its reason", {
    local_mocked_bindings(global_countries = \() c("good", "fails", "empty", "partial"))
    warned <- NULL
    out <- suppressMessages(withCallingHandlers(
      fetch_global(stub_fetch, requests_per_country = 3),
      warning = \(w) {
        warned <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ))

    expect_equal(out$country, c("good", "good", "partial"))
    expect_match(warned, "3 of 4 countries are missing or incomplete", fixed = TRUE)
    expect_match(warned, "fails: HTTP 500", fixed = TRUE)
    expect_match(warned, "empty: No rows returned", fixed = TRUE)
    expect_match(warned, "partial: Failed Top 10 requests", fixed = TRUE)
  })

  it("does not warn when every country returns rows", {
    local_mocked_bindings(global_countries = \() c("a", "b"))
    expect_no_warning(out <- suppressMessages(fetch_global(stub_fetch, 3)))
    expect_equal(nrow(out), 4L)
  })

  it("survives braces in an error message", {
    local_mocked_bindings(global_countries = \() "a")
    expect_warning(
      suppressMessages(fetch_global(\(cn) stop("bad {json}"), 3)),
      "bad {json}",
      fixed = TRUE
    )
  })

  it("reports the original message from an error inside map()", {
    local_mocked_bindings(global_countries = \() "a")
    fetch_one <- \(cn) purrr::map(cn, \(x) cli::cli_abort("No country found"))
    warned <- tryCatch(suppressMessages(fetch_global(fetch_one, 3)), warning = conditionMessage)
    expect_match(warned, "a: No country found", fixed = TRUE)
    expect_no_match(warned, "In index", fixed = TRUE)
  })

  it("states the minimum run time from the throttle", {
    local_mocked_bindings(global_countries = \() c("a", "b", "c", "d"))
    withr::local_options(flixpatrol.throttle_rate = 10)
    # 4 countries x 3 requests / 10 per minute = 1.2, rounded up
    expect_message(fetch_global(stub_fetch, 3), "at least 2 minutes")
  })

  it("gives no run time when throttling is off", {
    local_mocked_bindings(global_countries = \() "a")
    withr::local_options(flixpatrol.throttle_rate = Inf)
    msgs <- testthat::capture_messages(fetch_global(stub_fetch, 3))
    expect_false(any(grepl("at least", msgs)))
  })
})

describe("get_top_ten() request failures", {
  it("warns when a request fails, and Global reports that country", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    withr::local_options(flixpatrol.throttle_rate = Inf)
    local_mocked_bindings(
      lookup_platform = \(...) "plt_1",
      lookup_country = \(...) "cnt_1",
      global_countries = \() "Testland"
    )
    httr2::local_mocked_responses(\(req) httr2::response(status_code = 403))

    args <- list(
      platform_name = "netflix", start_date = "2026-10-20",
      end_date = "2026-10-20", media_type = "movie"
    )

    expect_warning(
      out <- suppressMessages(do.call(get_top_ten, c(args, country_name = "Testland"))),
      "Failed Top 10 requests: 1 of 1"
    )
    expect_equal(nrow(out), 0L)

    expect_warning(
      suppressMessages(do.call(get_top_ten, c(args, country_name = "Global"))),
      "Testland: Failed Top 10 requests"
    )
  })
})
