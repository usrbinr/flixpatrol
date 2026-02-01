describe("lookup_language_name_and_return_id()", {

  it("returns 1 for 'english'", {
    expect_equal(lookup_language_name_and_return_id("english"), 1)
  })

  it("returns 2 for 'non_english'", {
    expect_equal(lookup_language_name_and_return_id("non_english"), 2)
  })

  it("is case-insensitive", {
    expect_equal(lookup_language_name_and_return_id("English"), 1)
    expect_equal(lookup_language_name_and_return_id("ENGLISH"), 1)
  })

  it("returns all IDs for wildcard '*'", {
    result <- lookup_language_name_and_return_id("*")
    expect_length(result, 2)
    expect_true(1 %in% result)
    expect_true(2 %in% result)
  })

  it("errors for an invalid language name", {
    expect_error(
      lookup_language_name_and_return_id("spanish"),
      "must be one of"
    )
  })
})
