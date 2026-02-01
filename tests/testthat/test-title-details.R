describe("get_title_details()", {

  it("errors when title_id is missing", {
    expect_error(get_title_details(), "title_id")
  })

  it("errors when title_id is not a character", {
    expect_error(get_title_details(123), "title_id")
  })

  it("builds the correct endpoint URL", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      get_title_details("ttl_abc123"),
      class = "httr2_http"
    )
  })
})


describe("search_titles()", {

  it("hits the API with a query parameter", {
    withr::local_envvar(FLIX_PATROL = "fake_key")
    expect_error(
      search_titles("Squid Game"),
      class = "httr2_http"
    )
  })
})
