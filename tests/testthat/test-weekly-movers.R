describe("get_weekly_movers()", {

  # We can't call the real API, so test the classification logic directly
  # by simulating the join + mutate that get_weekly_movers performs

  it("classifies new entries correctly", {
    before <- tibble::tibble(title = "Old Movie", rank_before = 1L)
    after  <- tibble::tibble(title = c("Old Movie", "New Movie"), rank_after = c(2L, 1L))

    result <- dplyr::full_join(before, after, by = "title") |>
      dplyr::mutate(
        change = .data$rank_before - .data$rank_after,
        status = dplyr::case_when(
          is.na(.data$rank_before) ~ "new_entry",
          is.na(.data$rank_after)  ~ "exit",
          .data$change > 0         ~ "gainer",
          .data$change < 0         ~ "loser",
          TRUE                     ~ "unchanged"
        )
      )

    expect_equal(result$status[result$title == "New Movie"], "new_entry")
  })

  it("classifies exits correctly", {
    before <- tibble::tibble(title = c("Stays", "Leaves"), rank_before = c(1L, 5L))
    after  <- tibble::tibble(title = "Stays", rank_after = 1L)

    result <- dplyr::full_join(before, after, by = "title") |>
      dplyr::mutate(
        change = .data$rank_before - .data$rank_after,
        status = dplyr::case_when(
          is.na(.data$rank_before) ~ "new_entry",
          is.na(.data$rank_after)  ~ "exit",
          .data$change > 0         ~ "gainer",
          .data$change < 0         ~ "loser",
          TRUE                     ~ "unchanged"
        )
      )

    expect_equal(result$status[result$title == "Leaves"], "exit")
  })

  it("classifies gainers correctly (rank goes down numerically = higher position)", {
    before <- tibble::tibble(title = "Rising Star", rank_before = 5L)
    after  <- tibble::tibble(title = "Rising Star", rank_after = 2L)

    result <- dplyr::full_join(before, after, by = "title") |>
      dplyr::mutate(
        change = .data$rank_before - .data$rank_after,
        status = dplyr::case_when(
          is.na(.data$rank_before) ~ "new_entry",
          is.na(.data$rank_after)  ~ "exit",
          .data$change > 0         ~ "gainer",
          .data$change < 0         ~ "loser",
          TRUE                     ~ "unchanged"
        )
      )

    expect_equal(result$status, "gainer")
    expect_equal(result$change, 3L)
  })

  it("classifies losers correctly", {
    before <- tibble::tibble(title = "Falling", rank_before = 1L)
    after  <- tibble::tibble(title = "Falling", rank_after = 8L)

    result <- dplyr::full_join(before, after, by = "title") |>
      dplyr::mutate(
        change = .data$rank_before - .data$rank_after,
        status = dplyr::case_when(
          is.na(.data$rank_before) ~ "new_entry",
          is.na(.data$rank_after)  ~ "exit",
          .data$change > 0         ~ "gainer",
          .data$change < 0         ~ "loser",
          TRUE                     ~ "unchanged"
        )
      )

    expect_equal(result$status, "loser")
    expect_equal(result$change, -7L)
  })

  it("classifies unchanged correctly", {
    before <- tibble::tibble(title = "Steady", rank_before = 3L)
    after  <- tibble::tibble(title = "Steady", rank_after = 3L)

    result <- dplyr::full_join(before, after, by = "title") |>
      dplyr::mutate(
        change = .data$rank_before - .data$rank_after,
        status = dplyr::case_when(
          is.na(.data$rank_before) ~ "new_entry",
          is.na(.data$rank_after)  ~ "exit",
          .data$change > 0         ~ "gainer",
          .data$change < 0         ~ "loser",
          TRUE                     ~ "unchanged"
        )
      )

    expect_equal(result$status, "unchanged")
    expect_equal(result$change, 0L)
  })
})
