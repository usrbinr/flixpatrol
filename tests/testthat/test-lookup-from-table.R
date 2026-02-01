describe("lookup_from_table()", {

  # Build a small reference table for testing
  test_tbl <- tibble::tibble(
    fruit_name = c("apple", "banana", "cherry"),
    fruit_id   = c(10L, 20L, 30L)
  )

  describe("with exact matches", {

    it("returns the correct ID for a single match", {
      result <- lookup_from_table("apple", test_tbl, "fruit_name", "fruit_id", "fruit")
      expect_equal(result, 10L)
    })

    it("returns multiple IDs for multiple inputs", {
      result <- lookup_from_table(c("apple", "cherry"), test_tbl, "fruit_name", "fruit_id", "fruit")
      expect_equal(result, c(10L, 30L))
    })

    it("is case-insensitive", {
      result <- lookup_from_table("BANANA", test_tbl, "fruit_name", "fruit_id", "fruit")
      expect_equal(result, 20L)
    })

    it("handles mixed-case vector inputs", {
      result <- lookup_from_table(c("Apple", "CHERRY"), test_tbl, "fruit_name", "fruit_id", "fruit")
      expect_equal(result, c(10L, 30L))
    })
  })

  describe("with wildcard", {

    it("returns all IDs when input is '*'", {
      result <- lookup_from_table("*", test_tbl, "fruit_name", "fruit_id", "fruit")
      expect_equal(result, c(10L, 20L, 30L))
    })
  })

  describe("with invalid inputs", {

    it("errors for an unrecognized name", {
      expect_error(
        lookup_from_table("dragonfruit", test_tbl, "fruit_name", "fruit_id", "fruit"),
        "must be one of"
      )
    })

    it("errors if any name in a vector is invalid", {
      expect_error(
        lookup_from_table(c("apple", "invalid"), test_tbl, "fruit_name", "fruit_id", "fruit"),
        "must be one of"
      )
    })
  })
})
