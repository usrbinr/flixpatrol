describe("lookup_flix_type()", {

  it("returns 2 for 'movies'", {
    expect_equal(lookup_flix_type("movies"), 2)
  })

  it("returns 3 for 'tv_shows'", {
    expect_equal(lookup_flix_type("tv_shows"), 3)
  })

  it("returns 1 for 'overall'", {
    expect_equal(lookup_flix_type("overall"), 1)
  })

  it("returns 58 for 'anime'", {
    expect_equal(lookup_flix_type("anime"), 58)
  })

  it("is case-insensitive", {
    expect_equal(lookup_flix_type("Movies"), 2)
    expect_equal(lookup_flix_type("TV_SHOWS"), 3)
  })

  it("errors for an invalid flix type", {
    expect_error(
      lookup_flix_type("podcast"),
      "one of"
    )
  })

  it("handles kids types", {
    expect_equal(lookup_flix_type("kids_overall"), 40)
    expect_equal(lookup_flix_type("kidsmovies"), 49)
    expect_equal(lookup_flix_type("kids_tv_shows"), 50)
  })

  it("handles specialty types", {
    expect_equal(lookup_flix_type("documentaries"), 52)
    expect_equal(lookup_flix_type("reality_shows"), 53)
    expect_equal(lookup_flix_type("entertainment_shows"), 54)
    expect_equal(lookup_flix_type("tv_movies"), 55)
    expect_equal(lookup_flix_type("rental_movies"), 57)
    expect_equal(lookup_flix_type("anime_movies"), 59)
  })
})
