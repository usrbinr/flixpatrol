describe("get_global_ranking()", {

  it("returns a tibble with expected columns even when API fails", {
    # calls the live API with a fake key; slow and depends on the network
    skip_on_cran()
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_global_ranking(
      title     = "Test Movie",
      date      = "2025-12-15",
      countries = c("United States", "United Kingdom")
    )

    expect_s3_class(result, "tbl_df")
    expect_true("country" %in% names(result))
    expect_true("rank" %in% names(result))
    expect_true("title" %in% names(result))
  })

  it("returns one row per country", {
    # calls the live API with a fake key; slow and depends on the network
    skip_on_cran()
    withr::local_envvar(FLIX_PATROL = "fake_key")
    countries <- c("United States", "France", "Germany")
    result <- get_global_ranking(
      title     = "Test",
      date      = "2025-12-15",
      countries = countries
    )
    expect_equal(nrow(result), length(countries))
  })

  it("defaults to 15 countries when none specified", {
    # calls the live API with a fake key; slow and depends on the network
    skip_on_cran()
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_global_ranking(
      title = "Test",
      date  = "2025-12-15"
    )
    expect_equal(nrow(result), 15)
  })
})
