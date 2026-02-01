describe("fp_enrich()", {

  it("auto-detects an ID column with cmp_ prefix and adds a name column", {
    test_tbl <- tibble::tibble(
      my_id = c("cmp_abc123", "ttl_def456"),
      value = c(1, 2)
    )

    # resolve_id uses possibly(), so it will return NAs rather than error
    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- fp_enrich(test_tbl)
    expect_true("name" %in% names(result))
    expect_equal(nrow(result), 2)
    # Names will be NA since the API key is fake
    expect_true(all(is.na(result$name)))
  })

  it("errors when no ID column is found and none is specified", {
    test_tbl <- tibble::tibble(
      name  = c("Alice", "Bob"),
      value = c(1, 2)
    )

    expect_error(
      fp_enrich(test_tbl),
      "No column with FlixPatrol IDs"
    )
  })

  it("uses explicit id_col when provided", {
    test_tbl <- tibble::tibble(
      fp_id = c("cmp_abc123"),
      other = c("no_prefix")
    )

    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- fp_enrich(test_tbl, id_col = fp_id)
    expect_true("name" %in% names(result))
    expect_true(is.na(result$name[1]))
  })

  it("uses a custom name_col", {
    test_tbl <- tibble::tibble(
      fp_id = c("cmp_abc123")
    )

    withr::local_envvar(FLIX_PATROL = "fake_key")
    result <- fp_enrich(test_tbl, id_col = fp_id, name_col = "resolved")
    expect_true("resolved" %in% names(result))
  })
})
