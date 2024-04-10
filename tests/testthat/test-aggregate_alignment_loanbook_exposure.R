# aggregate_alignment_loanbook_exposure----

# nolint start: indentation_linter.
# styler: off
test_data_aggregate_alignment_loanbook_exposure_net <- tibble::tribble(
     ~group_id, ~name_abcd,       ~sector, ~activity_unit,  ~region, ~scenario_source,       ~scenario, ~year, ~direction, ~total_deviation, ~alignment_metric,
  "test_group", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",             -110,              -0.3,
  "test_group", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -70,              -0.4,
  "test_group", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -40,              -0.2,
  "test_group", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",               50,               0.1
)
# styler: on

# styler: off
test_data_aggregate_alignment_loanbook_exposure_bopo <- tibble::tribble(
     ~group_id, ~name_abcd,       ~sector, ~activity_unit,  ~region, ~scenario_source,       ~scenario, ~year, ~direction, ~total_deviation, ~alignment_metric,
  "test_group", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -10,             -0.05,
  "test_group", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",             -100,             -0.25,
  "test_group", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -50,             -0.35,
  "test_group", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -20,             -0.05,
  "test_group", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -40,              -0.2,
  "test_group", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",               60,              0.15,
  "test_group", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -10,             -0.05

)
# styler: on

# styler: off
test_matched <- tibble::tribble(
     ~group_id, ~id_loan, ~loan_size_outstanding, ~loan_size_outstanding_currency,       ~name_abcd, ~sector,
  "test_group",     "L1",                 300000,                           "USD", "test_company_1", "power",
  "test_group",     "L2",                 700000,                           "USD", "test_company_2", "power",
  "test_group",     "L3",                1000000,                           "USD", "test_company_3", "power",
  "test_group",     "L4",                 500000,                           "USD", "test_company_4", "power"
)
# styler: on

test_level_net <- "net"
test_level_bopo <- "bo_po"

test_output_aggregate_alignment_loanbook_exposure_net <- test_data_aggregate_alignment_loanbook_exposure_net %>%
  aggregate_alignment_loanbook_exposure(
    matched = test_matched,
    level = test_level_net
  )

test_output_aggregate_alignment_loanbook_exposure_bopo <- test_data_aggregate_alignment_loanbook_exposure_bopo %>%
  aggregate_alignment_loanbook_exposure(
    matched = test_matched,
    level = test_level_bopo
  )

test_that("aggregated net alignment equals sum of aggregated buildout and phaseout alignments", {
  expect_equal(
    test_output_aggregate_alignment_loanbook_exposure_net$exposure_weighted_net_alignment,
    sum(test_output_aggregate_alignment_loanbook_exposure_bopo$exposure_weighted_net_alignment, na.rm = TRUE)
  )
})

test_that("net aggregated loan size equals sum of matched loan size", {
  expect_equal(
    sum(test_output_aggregate_alignment_loanbook_exposure_net$sum_loan_size_outstanding, na.rm = TRUE),
    sum(test_matched$loan_size_outstanding, na.rm = TRUE)
  )
})

test_that("number of identified companies equals unique list of companies in input data", {
  n_companies_input_net <- length(unique(test_data_aggregate_alignment_loanbook_exposure_net$name_abcd))

  expect_equal(
    test_output_aggregate_alignment_loanbook_exposure_net$n_companies,
    n_companies_input_net
  )
})

test_that("number of identified companies equals unique list of companies in input data", {
  n_companies_input_buildout <- test_data_aggregate_alignment_loanbook_exposure_bopo %>%
    dplyr::filter(.data$direction == "buildout") %>%
    dplyr::distinct(.data$name_abcd) %>%
    nrow()

  n_output_buildout <- test_output_aggregate_alignment_loanbook_exposure_bopo %>%
    dplyr::filter(.data$direction == "buildout") %>%
    dplyr::pull(.data$n_companies)

  expect_equal(
    n_output_buildout,
    n_companies_input_buildout
  )

  n_companies_input_phaseout <- test_data_aggregate_alignment_loanbook_exposure_bopo %>%
    dplyr::filter(.data$direction == "phaseout") %>%
    dplyr::distinct(.data$name_abcd) %>%
    nrow()

  n_output_phaseout <- test_output_aggregate_alignment_loanbook_exposure_bopo %>%
    dplyr::filter(.data$direction == "phaseout") %>%
    dplyr::pull(.data$n_companies)

  expect_equal(
    n_output_phaseout,
    n_companies_input_phaseout
  )
})

test_that("net aggregate results have the same columns as buildout/phaseout aggregate results, plus three columns on loan exposure", {
  exposure_columns <- c("sum_loan_size_outstanding", "sum_exposure_companies_aligned", "share_exposure_aligned")

  expect_equal(
    c(names(test_output_aggregate_alignment_loanbook_exposure_bopo), exposure_columns),
    names(test_output_aggregate_alignment_loanbook_exposure_net)
  )
})

# When an additional variable is passed via group_var, add group_var to
# variables considered in aggregation (GH: 33)

# styler: off
test_data_company_net <- tibble::tribble(
  ~group_id, ~name_abcd,       ~sector, ~activity_unit,  ~region, ~scenario_source,       ~scenario, ~year, ~direction, ~total_deviation, ~alignment_metric,
  "test_lbk_1", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",             -110,              -0.3,
  "test_lbk_1", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -70,              -0.4,
  "test_lbk_2", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -70,              -0.4,
  "test_lbk_2", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -40,              -0.2,
  "test_lbk_3", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",              -40,              -0.2,
  "test_lbk_3", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",               50,               0.1,
  "test_lbk_4", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",               50,               0.1,
  "test_lbk_4", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027,      "net",             -110,              -0.3
)

test_matched_group_var <- tibble::tribble(
     ~group_id, ~id_loan, ~loan_size_outstanding, ~loan_size_outstanding_currency,       ~name_abcd, ~sector,  ~foo, ~bar,
  "test_lbk_1",     "L1",                 300000,                           "USD", "test_company_1", "power", "Yes", "Yes",
  "test_lbk_1",     "L2",                 700000,                           "USD", "test_company_2", "power", "Yes", "Yes",
  "test_lbk_2",     "L2",                 700000,                           "USD", "test_company_2", "power",  "No", "Yes",
  "test_lbk_2",     "L3",                1000000,                           "USD", "test_company_3", "power",  "No", "Yes",
  "test_lbk_3",     "L3",                1000000,                           "USD", "test_company_3", "power",  "No", "Yes",
  "test_lbk_3",     "L4",                 500000,                           "USD", "test_company_4", "power",  "No", "Yes",
  "test_lbk_4",     "L1",                 300000,                           "USD", "test_company_1", "power", "Yes",  "No",
  "test_lbk_4",     "L4",                 500000,                           "USD", "test_company_4", "power", "Yes",  "No"
)
# styler: on

test_that("net aggregate results with a group_var returns results for each group", {
  n_groups <- length(unique(test_matched_group_var$foo))

  test_output_with_group_var <- test_data_company_net %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_net,
      group_var = "foo"
    )

  expect_equal(
    nrow(test_output_with_group_var),
    n_groups
  )
})

test_that("net aggregate results with a group_var returns results for each group for multiple variables", {
  n_groups_2 <- nrow(dplyr::distinct(test_matched_group_var, .data$foo, .data$bar))

  test_output_with_group_var_2 <- test_data_company_net %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_net,
      group_var = c("foo", "bar")
    )

  expect_equal(
    nrow(test_output_with_group_var_2),
    n_groups_2
  )
})

# styler: off
test_data_company_bopo <- tibble::tribble(
     ~group_id, ~name_abcd,       ~sector, ~activity_unit,  ~region, ~scenario_source,       ~scenario, ~year, ~direction, ~total_deviation, ~alignment_metric,
  "test_lbk_1", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -10,             -0.05,
  "test_lbk_1", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",             -100,             -0.25,
  "test_lbk_1", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -50,             -0.35,
  "test_lbk_1", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -20,             -0.05,
  "test_lbk_2", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -50,             -0.35,
  "test_lbk_2", "test_company_2", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -20,             -0.05,
  "test_lbk_2", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -40,              -0.2,
  "test_lbk_3", "test_company_3", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -40,              -0.2,
  "test_lbk_3", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",               60,              0.15,
  "test_lbk_3", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -10,             -0.05,
  "test_lbk_4", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",               60,              0.15,
  "test_lbk_4", "test_company_4", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",              -10,             -0.05,
  "test_lbk_4", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "buildout",              -10,             -0.05,
  "test_lbk_4", "test_company_1", "power",           "MW", "global",    "test_source", "test_scenario",  2027, "phaseout",             -100,             -0.25
)
# styler: on

test_that("bopo aggregate results with a group_var returns results for each available combination of buildout/phaseout and group", {
  group_var_by_group_id <- dplyr::distinct(test_matched_group_var, .data$group_id, .data$foo)
  direction_by_group_id <- dplyr::distinct(test_data_company_bopo, .data$group_id, .data$direction)

  n_groups <- group_var_by_group_id %>%
    dplyr::inner_join(
      direction_by_group_id,
      by = "group_id"
    ) %>%
    dplyr::distinct(.data$direction, .data$foo) %>%
    nrow()

  test_output_with_group_var <- test_data_company_bopo %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_bopo,
      group_var = "foo"
    )

  expect_equal(
    nrow(test_output_with_group_var),
    n_groups
  )
})

test_that("aggregated net alignment by group_var foo equals sum of aggregated buildout and phaseout alignments by group_var foo", {
  test_output_with_group_var_bopo <- test_data_company_bopo %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_bopo,
      group_var = "foo"
    )

  test_output_with_group_var_net <- test_data_company_net %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_net,
      group_var = "foo"
    )

  expect_equal(
    sum(test_output_with_group_var_bopo$exposure_weighted_net_alignment, na.rm = TRUE),
    sum(test_output_with_group_var_net$exposure_weighted_net_alignment, na.rm = TRUE)
  )
})

test_that("net aggregated loan size by group_var foo equals sum of matched loan size by group_var foo", {
  test_output_with_group_var_net <- test_data_company_net %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_net,
      group_var = "foo"
    )

  expect_equal(
    sum(test_output_with_group_var_net$sum_loan_size_outstanding, na.rm = TRUE),
    sum(test_matched_group_var$loan_size_outstanding, na.rm = TRUE)
  )
})

test_that("aggregated net alignment by group_var that mirrors group_id is equivalent to aggregated net alignment", {
  test_output_net <- test_data_company_net %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_net
    )

  test_matched_group_var_same_as_group_id <- test_matched_group_var %>%
    dplyr::mutate(baz = .data$group_id)

  test_output_with_group_var_net <- test_data_company_net %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var_same_as_group_id,
      level = test_level_net,
      group_var = "baz"
    ) %>%
    dplyr::rename(group_id = "baz") %>%
    dplyr::select(names(test_output_net))

  expect_equal(
    test_output_with_group_var_net,
    test_output_net
  )
})

test_that("aggregated bopo alignment by group_var that mirrors group_id is equivalent to aggregated bopo alignment", {
  test_output_bopo <- test_data_company_bopo %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var,
      level = test_level_bopo
    )

  test_matched_group_var_same_as_group_id <- test_matched_group_var %>%
    dplyr::mutate(baz = .data$group_id)

  test_output_with_group_var_bopo <- test_data_company_bopo %>%
    aggregate_alignment_loanbook_exposure(
      matched = test_matched_group_var_same_as_group_id,
      level = test_level_bopo,
      group_var = "baz"
    ) %>%
    dplyr::rename(group_id = "baz") %>%
    dplyr::select(names(test_output_bopo))

  expect_equal(
    test_output_with_group_var_bopo,
    test_output_bopo
  )
})

# nolint end
