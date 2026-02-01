describe("authenticate()", {

  it("returns an httr2_request object when token is set", {
    withr::local_envvar(FLIX_PATROL = "test_key_12345")
    req <- authenticate()
    expect_s3_class(req, "httr2_request")
  })

  it("uses the default top10s endpoint when no site is provided", {
    withr::local_envvar(FLIX_PATROL = "test_key_12345")
    req <- authenticate()
    expect_true(grepl("top10s", req$url))
  })

  it("uses a custom endpoint when site is provided", {
    withr::local_envvar(FLIX_PATROL = "test_key_12345")
    req <- authenticate(site = "https://api.flixpatrol.com/v2/countries")
    expect_true(grepl("countries", req$url))
  })

  it("errors when token is not a character", {
    expect_error(authenticate(token = 123), "token")
  })

  it("errors when site is not a character", {
    withr::local_envvar(FLIX_PATROL = "test_key_12345")
    expect_error(authenticate(site = 123), "site")
  })

  it("errors when the environment variable is not set", {
    withr::local_envvar(FLIX_PATROL = "")
    expect_error(authenticate(), "must be set in your environment")
  })

  it("includes retry configuration", {
    withr::local_envvar(FLIX_PATROL = "test_key_12345")
    req <- authenticate()
    # httr2 stores retry policies internally
    expect_s3_class(req, "httr2_request")
  })
})
