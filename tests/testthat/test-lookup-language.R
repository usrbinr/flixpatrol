describe("lookup_language()", {

  it("returns 1 for 'english'", {
    expect_equal(lookup_language("english"), 1)
  })

  it("returns 2 for 'non_english'", {
    expect_equal(lookup_language("non_english"), 2)
  })

  it("is case-insensitive", {
    expect_equal(lookup_language("English"), 1)
    expect_equal(lookup_language("ENGLISH"), 1)
  })

  it("returns all IDs for wildcard '*'", {
    result <- lookup_language("*")
    expect_length(result, 2)
    expect_true(1 %in% result)
    expect_true(2 %in% result)
  })

  it("errors for an invalid language name", {
    expect_error(
      lookup_language("spanish"),
      "must be one of"
    )
  })
})
