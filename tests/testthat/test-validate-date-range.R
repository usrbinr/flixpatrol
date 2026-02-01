describe("validate_date_range()", {

  describe("with valid inputs", {

    it("returns TRUE for a valid Monday-to-Sunday 7-day range", {
      # 2025-12-15 is a Monday, 2025-12-21 is a Sunday
      result <- validate_date_range(
        start_date = "2025-12-15",
        end_date   = "2025-12-21",
        start_date_wday_abb = "mon",
        range_length = 7
      )
      expect_true(result)
    })

    it("accepts Date objects as well as character strings", {
      result <- validate_date_range(
        start_date = as.Date("2025-12-15"),
        end_date   = as.Date("2025-12-21"),
        start_date_wday_abb = "mon",
        range_length = 7
      )
      expect_true(result)
    })

    it("works for non-Monday start days", {
      # 2025-12-17 is a Wednesday
      result <- validate_date_range(
        start_date = "2025-12-17",
        end_date   = "2025-12-19",
        start_date_wday_abb = "wed",
        range_length = 3
      )
      expect_true(result)
    })

    it("works for a single-day range", {
      # 2025-12-15 is a Monday
      result <- validate_date_range(
        start_date = "2025-12-15",
        end_date   = "2025-12-15",
        start_date_wday_abb = "mon",
        range_length = 1
      )
      expect_true(result)
    })
  })

  describe("with invalid inputs", {

    it("errors when start_date_wday_abb is not a valid abbreviation", {
      expect_error(
        validate_date_range("2025-12-15", "2025-12-21", "xyz"),
        "start_date_wday_abb"
      )
    })

    it("errors when start_date does not fall on the required weekday", {
      # 2025-12-16 is a Tuesday, not Monday
      expect_error(
        validate_date_range("2025-12-16", "2025-12-22", "mon"),
        "must start on"
      )
    })

    it("errors when the range length does not match", {
      # 15 to 21 is 7 days, not 5
      expect_error(
        validate_date_range("2025-12-15", "2025-12-21", "mon", range_length = 5),
        "days apart"
      )
    })

    it("errors when end_date is before start_date", {
      expect_error(
        validate_date_range("2025-12-21", "2025-12-15", "sun", range_length = 7)
      )
    })
  })
})
