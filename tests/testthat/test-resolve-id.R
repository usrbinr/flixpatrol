describe("get_title_name()", {

  it("returns NA for IDs that cannot be resolved (no API key)", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name(c("cmp_nonexistent", "ttl_nonexistent"))
    expect_length(result, 2)
    expect_true(all(is.na(result)))
  })

  it("returns a character vector", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name("cmp_abc")
    expect_type(result, "character")
  })

  it("handles an empty input vector", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name(character(0))
    expect_length(result, 0)
    expect_type(result, "character")
  })

  it("returns NA for unknown ID prefixes", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name("xyz_unknown")
    expect_true(is.na(result))
  })
})
