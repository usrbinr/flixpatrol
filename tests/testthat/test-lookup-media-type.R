describe("lookup_media_type_name_and_return_id()", {

  it("returns 1 for 'movie'", {
    expect_equal(lookup_media_type_name_and_return_id("movie"), 1)
  })

  it("returns 2 for 'tv_show'", {
    expect_equal(lookup_media_type_name_and_return_id("tv_show"), 2)
  })

  it("is case-insensitive", {
    expect_equal(lookup_media_type_name_and_return_id("Movie"), 1)
    expect_equal(lookup_media_type_name_and_return_id("TV_SHOW"), 2)
  })

  it("returns all IDs for wildcard '*'", {
    result <- lookup_media_type_name_and_return_id("*")
    expect_length(result, 2)
  })

  it("errors for an invalid media type", {
    expect_error(
      lookup_media_type_name_and_return_id("podcast"),
      "must be one of"
    )
  })
})
