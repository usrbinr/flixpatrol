describe("get_official_ranking()", {

  it("errors when platform is not netflix", {
    expect_error(
      get_official_ranking(
        platform_name = "disney+",
        country_name  = "United States",
        start_date    = "2025-12-15",
        end_date      = "2025-12-21",
        media_type    = "movie"
      ),
      "only supports Netflix"
    )
  })

  it("errors when platform is not netflix (mixed case)", {
    expect_error(
      get_official_ranking(
        platform_name = "HBO Max",
        country_name  = "United States",
        start_date    = "2025-12-15",
        end_date      = "2025-12-21",
        media_type    = "movie"
      ),
      "only supports Netflix"
    )
  })

  it("does not error for netflix (case-insensitive)", {
    # It will eventually error on the API call (no real key),
    # but should NOT error on the platform validation
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      get_official_ranking(
        platform_name = "Netflix",
        country_name  = "United States",
        start_date    = "2025-12-15",
        end_date      = "2025-12-21",
        media_type    = "movie"
      ),
      # Should fail on API call, not platform validation
      class = "httr2_http"
    )
  })
})
