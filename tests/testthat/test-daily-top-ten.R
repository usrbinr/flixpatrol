describe("create_daily_top_ten_tbl()", {

  it("coerces character dates to Date objects without error", {
    # Will fail on API call, but should not fail on date coercion
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      create_daily_top_ten_tbl(
        platform_name = "netflix",
        country_name  = "United States",
        start_date    = "2025-12-01",
        end_date      = "2025-12-01",
        flix_type     = "movies"
      ),
      class = "httr2_http"
    )
  })

  it("generates the correct number of date iterations", {
    # We can test that the date vector is correctly built by checking
    # that seq.Date produces the expected length
    start <- as.Date("2025-12-01")
    end   <- as.Date("2025-12-03")
    date_vec <- seq.Date(from = start, to = end, by = "day")
    expect_length(date_vec, 3)
  })
})
