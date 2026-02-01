describe("get_title_name()", {

  it("errors for an unknown ID prefix", {
    expect_error(
      get_title_name("zzz_abc123"),
      "Unknown ID prefix"
    )
  })

  it("recognises the cmp_ prefix and routes to companies", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      get_title_name("cmp_nonexistent"),
      "404 Not Found|companies"
    )
  })

  it("recognises the ttl_ prefix and routes to titles", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      get_title_name("ttl_nonexistent"),
      "404 Not Found|titles"
    )
  })

  it("recognises the frn_ prefix and routes to franchises", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      get_title_name("frn_nonexistent"),
      "404 Not Found|franchises"
    )
  })

  it("trims whitespace from the input", {
    expect_error(
      get_title_name("zzz_abc"),
      "Unknown ID prefix"
    )
  })
})
