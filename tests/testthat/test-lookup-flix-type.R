describe("lookup_flix_type_and_return_id()", {

  it("returns 2 for 'movies'", {
    expect_equal(lookup_flix_type_and_return_id("movies"), 2)
  })

  it("returns 3 for 'tv_shows'", {
    expect_equal(lookup_flix_type_and_return_id("tv_shows"), 3)
  })

  it("returns 1 for 'overall'", {
    expect_equal(lookup_flix_type_and_return_id("overall"), 1)
  })

  it("returns 58 for 'anime'", {
    expect_equal(lookup_flix_type_and_return_id("anime"), 58)
  })

  it("is case-insensitive", {
    expect_equal(lookup_flix_type_and_return_id("Movies"), 2)
    expect_equal(lookup_flix_type_and_return_id("TV_SHOWS"), 3)
  })

  it("errors for an invalid flix type", {
    expect_error(
      lookup_flix_type_and_return_id("podcast"),
      "one of"
    )
  })

  it("handles kids types", {
    expect_equal(lookup_flix_type_and_return_id("kids_overall"), 40)
    expect_equal(lookup_flix_type_and_return_id("kidsmovies"), 49)
    expect_equal(lookup_flix_type_and_return_id("kids_tv_shows"), 50)
  })

  it("handles specialty types", {
    expect_equal(lookup_flix_type_and_return_id("documentaries"), 52)
    expect_equal(lookup_flix_type_and_return_id("reality_shows"), 53)
    expect_equal(lookup_flix_type_and_return_id("entertainment_shows"), 54)
    expect_equal(lookup_flix_type_and_return_id("tv_movies"), 55)
    expect_equal(lookup_flix_type_and_return_id("rental_movies"), 57)
    expect_equal(lookup_flix_type_and_return_id("anime_movies"), 59)
  })
})
