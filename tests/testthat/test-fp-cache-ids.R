describe("fp_cache_ids()", {

  it("returns invisible TRUE when both arguments are NULL", {
    result <- fp_cache_ids(country_names = NULL, platform_names = NULL)
    expect_true(result)
  })

  it("attempts to resolve country names and handles failures gracefully", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "data"), showWarnings = FALSE)

    # Should not error even though API calls fail (tryCatch inside)
    result <- fp_cache_ids(
      country_names = c("Nonexistent Land"),
      pkg_path      = tmp_dir
    )

    expect_true(result)
    # The .rda should exist (even if empty after filtering NAs)
    expect_true(file.exists(file.path(tmp_dir, "data", "country_name.rda")))
  })

  it("attempts to resolve platform names and handles failures gracefully", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    tmp_dir <- withr::local_tempdir()
    dir.create(file.path(tmp_dir, "data"), showWarnings = FALSE)

    result <- fp_cache_ids(
      platform_names = c("FakeStreamCo"),
      pkg_path       = tmp_dir
    )

    expect_true(result)
    expect_true(file.exists(file.path(tmp_dir, "data", "platform_name.rda")))
  })
})
