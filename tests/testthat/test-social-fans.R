describe("create_social_fans_tbl()", {

  it("errors for an invalid social platform name", {
    expect_error(
      create_social_fans_tbl(social_platform = "TikTok", start_date = "2025-12-15", end_date = "2025-12-21"),
      "must be one of"
    )
  })

  it("is case-insensitive for platform names", {
    # Should not error on validation — will error on API call
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      create_social_fans_tbl(social_platform = "instagram", start_date = "2025-12-15", end_date = "2025-12-21"),
      class = "httr2_http"
    )
  })

  it("accepts all three valid platforms without validation error", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    for (plat in c("Instagram", "Facebook", "Twitter")) {
      # Should error on API, not validation
      expect_error(
        create_social_fans_tbl(social_platform = plat, start_date = "2025-12-15", end_date = "2025-12-15"),
        class = "httr2_http"
      )
    }
  })
})
