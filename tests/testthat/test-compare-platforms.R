describe("compare_platforms_tbl()", {

  it("returns a tibble with expected columns even when API fails", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    # purrr::possibly wraps the API failures, so this should return NA ranks
    result <- compare_platforms_tbl(
      title     = "Test Movie",
      platforms = c("netflix", "disney+"),
      date      = "2025-12-15"
    )

    expect_s3_class(result, "tbl_df")
    expect_true("platform" %in% names(result))
    expect_true("rank" %in% names(result))
    expect_true("title" %in% names(result))
    expect_equal(nrow(result), 2)
  })

  it("returns one row per platform", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    platforms <- c("netflix", "disney+", "hbo max")
    result <- compare_platforms_tbl(
      title     = "Test",
      platforms = platforms,
      date      = "2025-12-15"
    )
    expect_equal(nrow(result), length(platforms))
  })

  it("fills NA ranks when API is unreachable", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- compare_platforms_tbl(
      title     = "Nonexistent Movie",
      platforms = c("netflix"),
      date      = "2025-12-15"
    )
    expect_true(is.na(result$rank[1]))
  })
})
