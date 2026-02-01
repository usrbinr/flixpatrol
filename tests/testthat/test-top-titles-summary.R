describe("create_top_titles_summary_tbl() — aggregation logic", {

  # Test the summary aggregation logic in isolation using a mock chart
  mock_chart <- tibble::tibble(
    rank  = c(1L, 2L, 3L, 1L, 3L, 2L, 5L),
    title = c("Alpha", "Beta", "Gamma", "Alpha", "Alpha", "Beta", "Delta"),
    date  = c("2025-12-01", "2025-12-01", "2025-12-01",
              "2025-12-02", "2025-12-03", "2025-12-02", "2025-12-03")
  )

  summary <- mock_chart |>
    dplyr::group_by(.data$title) |>
    dplyr::summarise(
      days_on_chart = dplyr::n(),
      best_rank     = min(.data$rank, na.rm = TRUE),
      avg_rank      = round(mean(.data$rank, na.rm = TRUE), 1),
      first_seen    = min(.data$date, na.rm = TRUE),
      last_seen     = max(.data$date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$days_on_chart), .data$best_rank)

  it("counts days_on_chart correctly", {
    expect_equal(summary$days_on_chart[summary$title == "Alpha"], 3)
    expect_equal(summary$days_on_chart[summary$title == "Beta"], 2)
    expect_equal(summary$days_on_chart[summary$title == "Gamma"], 1)
    expect_equal(summary$days_on_chart[summary$title == "Delta"], 1)
  })

  it("finds the best (minimum) rank", {
    expect_equal(summary$best_rank[summary$title == "Alpha"], 1)
    expect_equal(summary$best_rank[summary$title == "Beta"], 2)
  })

  it("calculates the average rank correctly", {
    # Alpha: (1 + 1 + 3) / 3 = 1.666... -> 1.7
    expect_equal(summary$avg_rank[summary$title == "Alpha"], 1.7)
    # Beta: (2 + 2) / 2 = 2.0
    expect_equal(summary$avg_rank[summary$title == "Beta"], 2.0)
  })

  it("identifies first and last seen dates", {
    expect_equal(summary$first_seen[summary$title == "Alpha"], "2025-12-01")
    expect_equal(summary$last_seen[summary$title == "Alpha"], "2025-12-03")
  })

  it("sorts by days_on_chart descending then best_rank ascending", {
    expect_equal(summary$title, c("Alpha", "Beta", "Gamma", "Delta"))
  })

  it("head(n) limits results", {
    limited <- utils::head(summary, 2)
    expect_equal(nrow(limited), 2)
    expect_equal(limited$title, c("Alpha", "Beta"))
  })
})
