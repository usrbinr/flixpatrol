describe("get_title_name()", {

  it("returns NA for IDs that cannot be resolved (no API key)", {
    # calls the live API with a fake key; slow and depends on the network
    skip_on_cran()
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name(c("cmp_nonexistent", "ttl_nonexistent"))
    expect_length(result, 2)
    expect_true(all(is.na(result)))
  })

  it("returns a character vector", {
    # calls the live API with a fake key; slow and depends on the network
    skip_on_cran()
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name("cmp_abc")
    expect_type(result, "character")
  })

  it("handles an empty input vector", {
    # calls the live API with a fake key; slow and depends on the network
    skip_on_cran()
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name(character(0))
    expect_length(result, 0)
    expect_type(result, "character")
  })

  it("returns NA for unknown ID prefixes", {
    # calls the live API with a fake key; slow and depends on the network
    skip_on_cran()
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- get_title_name("xyz_unknown")
    expect_true(is.na(result))
  })
})
