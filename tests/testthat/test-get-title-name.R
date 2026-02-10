describe("get_title_name()", {

  it("returns NA for an unknown ID prefix", {
    result <- get_title_name("zzz_abc123")
    expect_true(is.na(result))
  })

  it("returns NA for non-existent cmp_ ID", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name("cmp_nonexistent")
    expect_true(is.na(result))
  })

  it("returns NA for non-existent ttl_ ID", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name("ttl_nonexistent")
    expect_true(is.na(result))
  })

  it("returns NA for non-existent frn_ ID", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name("frn_nonexistent")
    expect_true(is.na(result))
  })

  it("is vectorized", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name(c("cmp_fake", "ttl_fake"))
    expect_length(result, 2)
    expect_type(result, "character")
  })

  it("handles empty input", {
    result <- get_title_name(character(0))
    expect_length(result, 0)
  })
})
