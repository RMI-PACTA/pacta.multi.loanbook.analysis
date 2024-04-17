#' Return loan book level aggregation of company alignment metrics by exposure
#'
#' @param data data.frame. Holds output of company indicators. Contains columns
#'   `"name_abcd"`,	`"sector"`,	`"activity_unit"`,	`"region"`,
#'   `"scenario_source"`,	`"scenario"`,	`"year"`,	`"direction"`,
#'   `"total_deviation"` and	`"alignment_metric"`.
#' @param matched data.frame. Holds matched and prioritized loan book data.
#' @param level Character. Vector that indicates if the aggregate alignment
#'   metric should be returned based on the net technology deviations (`net`) or
#'   disaggregated into buildout and phaseout technologies (`bo_po`).
#' @param .by Character. Optionally, a selection of columns to group by. All
#'   columns indicated must be available variables in the `matched` data set.
#'   The intended use case is to allow analyzing the loan books by additional
#'   traits of interest, such as types of financial institutions. Default is
#'   `NULL`, which means the aggregation is done at the meta level. It is not
#'   possible to group by the critical columns of the `data` and `matched`
#'   inputs.
#'
#' @return NULL
#' @export
aggregate_alignment_loanbook_exposure <- function(data,
                                                  matched,
                                                  level = c("net", "bo_po"),
                                                  .by = NULL) {
  group_vars <- c(
    "scenario",
    "region",
    "sector",
    "year",
    "direction"
  )
  level <- rlang::arg_match(level)

  # validate input data sets
  validate_input_data_aggregate_alignment_loanbook_exposure(
    data = data,
    matched = matched,
    group_vars = group_vars,
    .by = .by
  )

  if (!is.null(.by)) {
    if (!inherits(.by, "character")) {
      stop(glue::glue("`.by` must be a character vector. Your input is {class(.by)}."))
    }
    group_vars <- c(.by, group_vars)
  }

  matched <- matched %>%
    dplyr::select(
      dplyr::all_of(
        c(
          .by,
          "id_loan",
          "loan_size_outstanding",
          "loan_size_outstanding_currency",
          "name_abcd",
          "sector"
        )
      )
    ) %>%
    dplyr::summarise(
      loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
      .by = dplyr::all_of(
        c(
          .env$.by,
          "loan_size_outstanding_currency",
          "name_abcd",
          "sector"
        )
      )
    ) %>%
    dplyr::mutate(
      exposure_weight = .data$loan_size_outstanding / sum(.data$loan_size_outstanding, na.rm = TRUE),
      .by = dplyr::all_of(c(.env$.by, "loan_size_outstanding_currency"))
    )

  aggregate_exposure_company <- data %>%
    dplyr::inner_join(
      matched,
      by = c("name_abcd", "sector"),
      relationship = "many-to-many"
    )

  # if a company only has technologies going in one direction in a sector with
  # high carbon and low carbon technologies, add an empty entry for the other
  # direction to ensure the aggregation is correct
  if (level == "bo_po") {
    aggregate_exposure_company <- aggregate_exposure_company %>%
      dplyr::mutate(
        n_directions = dplyr::n_distinct(.data$direction, na.rm = TRUE),
        .by = dplyr::all_of(
          c(
            group_vars[!group_vars == "direction"],
            "name_abcd",
            "sector",
            "activity_unit",
            "loan_size_outstanding_currency"
          )
        )
      )

    single_direction <- aggregate_exposure_company %>%
      dplyr::filter(
        .data$n_directions == 1,
        .data$direction %in% c("buildout", "phaseout"),
        .data$sector %in% c("automotive", "hdv", "power")
      )

    opposite_direction <- single_direction %>%
      dplyr::mutate(
        direction = dplyr::if_else(
          .data$direction == "buildout",
          "phaseout",
          "buildout"
        ),
        total_deviation = 0,
        alignment_metric = 0
      )

    aggregate_exposure_company <- aggregate_exposure_company %>%
      dplyr::bind_rows(opposite_direction) %>%
      dplyr::select(-"n_directions")
  }

  out_company <- aggregate_exposure_company %>%
    dplyr::relocate(
      dplyr::all_of(c(.by))
    )

  sector_aggregate_exposure_loanbook_summary <- aggregate_exposure_company %>%
    dplyr::mutate(
      n_companies = dplyr::n_distinct(.data$name_abcd),
      .by = dplyr::all_of(group_vars)
    ) %>%
    dplyr::mutate(
      companies_aligned = dplyr::if_else(.data$alignment_metric >= 0, TRUE, FALSE)
    ) %>%
    dplyr::summarise(
      n_companies_aligned = sum(.data$companies_aligned, na.rm = TRUE),
      .by = dplyr::all_of(c(group_vars, "n_companies"))
    ) %>%
    dplyr::mutate(
      share_companies_aligned = .data$n_companies_aligned / .data$n_companies
    )

  # aggregate exposure of aligned companies can only be calculated reasonably
  # for the net level, not the buildout/phaseout level since we cannot assume
  # the loan is split based on output units.
  if (level == "net") {
    sector_aggregate_exposure_loanbook_summary_value <- aggregate_exposure_company %>%
      dplyr::mutate(
        sum_loan_size_outstanding = sum(.data$loan_size_outstanding, na.rm = TRUE),
        .by = dplyr::all_of(group_vars)
      ) %>%
      dplyr::mutate(
        exposure_companies_aligned = dplyr::if_else(.data$alignment_metric >= 0, .data$loan_size_outstanding, 0)
      ) %>%
      dplyr::summarise(
        sum_exposure_companies_aligned = sum(.data$exposure_companies_aligned, na.rm = TRUE),
        .by = dplyr::all_of(c(group_vars, "sum_loan_size_outstanding"))
      ) %>%
      dplyr::mutate(
        share_exposure_aligned = .data$sum_exposure_companies_aligned / .data$sum_loan_size_outstanding
      )

    sector_aggregate_exposure_loanbook_summary <- sector_aggregate_exposure_loanbook_summary %>%
      dplyr::inner_join(
        sector_aggregate_exposure_loanbook_summary_value,
        by = group_vars
      )
  }

  sector_aggregate_exposure_loanbook_alignment <- aggregate_exposure_company %>%
    dplyr::summarise(
      exposure_weighted_net_alignment = stats::weighted.mean(
        x = .data$alignment_metric,
        w = .data$exposure_weight,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(group_vars)
    )

  out_aggregate <- sector_aggregate_exposure_loanbook_summary %>%
    dplyr::inner_join(
      sector_aggregate_exposure_loanbook_alignment,
      by = group_vars
    ) %>%
    dplyr::relocate(
      dplyr::all_of(
        c(
          group_vars,
          "n_companies",
          "n_companies_aligned",
          "share_companies_aligned",
          "exposure_weighted_net_alignment"
        )
      )
    ) %>%
    dplyr::arrange(
      !!!rlang::syms(.by),
      .data$scenario,
      .data$region,
      .data$sector,
      .data$year
    )

  out <- list(
    company = out_company,
    aggregate = out_aggregate
  )

  return(out)
}

validate_input_data_aggregate_alignment_loanbook_exposure <- function(data,
                                                                      matched,
                                                                      group_vars,
                                                                      .by = NULL) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      group_vars,
      "name_abcd",
      "activity_unit",
      "scenario_source",
      "alignment_metric"
    )
  )

  validate_data_has_expected_cols(
    data = matched,
    expected_columns = c(
      "id_loan",
      "loan_size_outstanding",
      "loan_size_outstanding_currency",
      "name_abcd",
      "sector",
      .by
    )
  )

  if (!is.null(.by)) {
    if (
      any(
        .by %in% c(
          group_vars,
          "name_abcd",
          "activity_unit",
          "scenario_source",
          "alignment_metric",
          "id_loan",
          "loan_size_outstanding",
          "loan_size_outstanding_currency",
          "name_abcd",
          "sector"
        )
      )
    ) {
      stop(
        "It is not possible to group by the critical columns of the `data` and
        `matched` inputs. Please check your .by argument!"
      )
    }
  }

  invisible()
}
