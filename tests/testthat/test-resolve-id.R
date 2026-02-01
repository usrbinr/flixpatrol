describe("resolve_id()", {

  it("returns NA for IDs that cannot be resolved (no API key)", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- resolve_id(c("cmp_nonexistent", "ttl_nonexistent"))
    expect_length(result, 2)
    expect_true(all(is.na(result)))
  })

  it("returns a character vector", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- resolve_id("cmp_abc")
    expect_type(result, "character")
  })

  it("handles an empty input vector", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- resolve_id(character(0))
    expect_length(result, 0)
    expect_type(result, "character")
  })
})
