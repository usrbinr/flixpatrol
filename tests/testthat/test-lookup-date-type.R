describe("lookup_date_type()", {

  it("returns 1 for 'day'", {
    expect_equal(lookup_date_type("day"), 1)
  })

  it("returns 3 for 'week'", {
    expect_equal(lookup_date_type("week"), 3)
  })

  it("returns 4 for 'month'", {
    expect_equal(lookup_date_type("month"), 4)
  })

  it("returns 5 for 'year'", {
    expect_equal(lookup_date_type("year"), 5)
  })

  it("returns 6 for 'quarter'", {
    expect_equal(lookup_date_type("quarter"), 6)
  })

  it("returns 7 for 'half_year'", {
    expect_equal(lookup_date_type("half_year"), 7)
  })

  it("returns 14 for 'first_month'", {
    expect_equal(lookup_date_type("first_month"), 14)
  })

  it("is case-insensitive", {
    expect_equal(lookup_date_type("Week"), 3)
    expect_equal(lookup_date_type("DAY"), 1)
  })

  it("returns all IDs for wildcard '*'", {
    result <- lookup_date_type("*")
    expect_length(result, 7)
  })

  it("errors for an invalid date type", {
    expect_error(
      lookup_date_type("biweekly"),
      "must be one of"
    )
  })
})
