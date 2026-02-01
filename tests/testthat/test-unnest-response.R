describe("unnest_flixpatrol_response()", {

  it("returns a tibble from a well-formed nested list", {
    mock_data <- list(
      list(
        data = list(
          ranking = 1,
          movie = list(
            data = list(title = "Test Movie", imdbId = "tt1234567"),
            legacy = list(slug = "test-movie")
          ),
          company = list(
            data = list(name = "Netflix", id = "cmp_123"),
            legacy = list(slug = "netflix")
          ),
          country = list(
            data = list(name = "United States", id = "ctr_US"),
            legacy = list(slug = "us")
          ),
          date = list(
            from = "2025-12-15",
            to   = "2025-12-15"
          )
        )
      )
    )

    result <- unnest_flixpatrol_response(mock_data)
    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) >= 1)
  })

  it("applies janitor::clean_names to the output", {
    mock_data <- list(
      list(
        data = list(
          ranking = 1,
          movie = list(
            data = list(title = "Test"),
            legacy = list(slug = "test")
          ),
          company = list(
            data = list(name = "Netflix"),
            legacy = list(slug = "netflix")
          ),
          country = list(
            data = list(name = "US"),
            legacy = list(slug = "us")
          ),
          date = list(from = "2025-12-15", to = "2025-12-15")
        )
      )
    )

    result <- unnest_flixpatrol_response(mock_data)
    # clean_names produces snake_case — no dots or spaces
    expect_false(any(grepl("[. ]", names(result))))
  })

  it("handles multiple rows", {
    mock_data <- list(
      list(data = list(
        ranking = 1,
        movie   = list(data = list(title = "A"), legacy = list(slug = "a")),
        company = list(data = list(name = "N"), legacy = list(slug = "n")),
        country = list(data = list(name = "US"), legacy = list(slug = "us")),
        date    = list(from = "2025-12-15", to = "2025-12-15")
      )),
      list(data = list(
        ranking = 2,
        movie   = list(data = list(title = "B"), legacy = list(slug = "b")),
        company = list(data = list(name = "N"), legacy = list(slug = "n")),
        country = list(data = list(name = "US"), legacy = list(slug = "us")),
        date    = list(from = "2025-12-15", to = "2025-12-15")
      ))
    )

    result <- unnest_flixpatrol_response(mock_data)
    expect_equal(nrow(result), 2)
  })
})
