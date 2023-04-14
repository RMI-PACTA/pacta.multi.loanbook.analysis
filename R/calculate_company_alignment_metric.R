#' Return company level technology deviations for TMS sectors. To be used as
#' input into calculation of company level aggregate alignment metrics for
#' production trajectory sectors.
#'
#' @param data data.frame. Holds the PACTA for Banks TMS results. Must have been
#'   calculated according to the green/brown logic of the CA100+ calculation
#'   and must return unweighted company level TMSR results.
#' @param technology_direction data frame that indicates which technologies are
#'   to be considered phase down technologies versus build out technologies
#' @param scenario_trajectory data frame containing the scenario file with
#'   information on yearly tmsr and smsp changes by scenario and region.
#' @param green_or_brown data frame. Indicates which technologies are to use
#'   tmsr versus smsp
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the alignment metrics. Currently,
#'   the only supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the alignment metric for. Must be a scenario available from `scenario_source`.
#' @param bridge_tech Character. Vector that indicates if a technology is
#'   considered a bridge technology. I.e. if the scenario requires a temporary
#'   build out despite the need for a long term phase down. If so, the alignment
#'   metric can be treated differently than for other technologies. Currently,
#'   the only allowed values are (`"none", "gascap"`). Default is `"none"` which
#'   means that no special calculations are applied to any technology.
#'
#' @return NULL
#' @export
calculate_company_tech_deviation <- function(data,
                                             technology_direction,
                                             scenario_trajectory,
                                             green_or_brown,
                                             scenario_source = "geco_2021",
                                             scenario = "1.5c",
                                             bridge_tech = c("none", "gascap")) {
  bridge_tech <- rlang::arg_match(bridge_tech)

  # validate input values
  validate_input_args_calculate_company_tech_deviation(
    scenario_source = scenario_source,
    scenario = scenario,
    bridge_tech = bridge_tech
  )

  # validate input data sets
  validate_input_data_calculate_company_tech_deviation(
    data = data,
    technology_direction = technology_direction,
    scenario_trajectory = scenario_trajectory,
    green_or_brown = green_or_brown
  )

  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)

  technology_direction <- technology_direction %>%
    dplyr::filter(
      .data$scenario_source == .env$scenario_source,
      grepl(pattern = .env$scenario, x = .data$scenario)
    ) %>%
    dplyr::select(c("sector", "technology", "region", "directional_dummy"))

  data <- data %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source)

  data <- data %>%
    dplyr::select(-c("technology_share", "scope", "percentage_of_initial_production_by_scope")) %>%
    dplyr::filter(.data$metric %in% c("projected", paste0("target_", .env$scenario))) %>%
    dplyr::filter(dplyr::between(.data$year, left = .env$start_year, right = .env$start_year + 5)) %>%
    tidyr::pivot_wider(
      names_from = "metric",
      values_from = "production"
    )

  # add directional dummy
  data <- data %>%
    dplyr::inner_join(technology_direction, by = c("sector", "technology", "region"))

  # remove rows if both projected and target values are 0
  data_to_remove_no_plans_no_target_tech <- data %>%
    dplyr::group_by(
      .data$group_id, .data$name_abcd, .data$region, .data$scenario_source,
      .data$sector, .data$technology, .data$year
    ) %>%
    dplyr::rename(target = !!rlang::sym(target_scenario)) %>%
    dplyr::summarise(
      projected = sum(.data$projected, na.rm = TRUE),
      target = sum(.data$target, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      .data$projected == 0,
      .data$target == 0
    )

  data_to_remove_no_target_in_sector <- data %>%
    dplyr::group_by(.data$group_id, .data$name_abcd, .data$region, .data$scenario_source, .data$sector, .data$year) %>%
    dplyr::rename(target = !!rlang::sym(target_scenario)) %>%
    dplyr::summarise(
      target = sum(.data$target, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$target == 0)

  data <- data %>%
    dplyr::anti_join(
      data_to_remove_no_plans_no_target_tech,
      by = c("group_id", "name_abcd", "region", "scenario_source", "sector", "technology", "year")
    )

  data <- data %>%
    dplyr::anti_join(
      data_to_remove_no_target_in_sector,
      by = c("group_id", "name_abcd", "region", "scenario_source", "sector", "year")
    )

  # calculate total deviation per technology
  data <- data %>%
    dplyr::mutate(
      total_tech_deviation = (.data$projected - !!rlang::sym(target_scenario)) * .data$directional_dummy
    )

  # add direction
  data <- data %>%
    dplyr::mutate(direction = dplyr::if_else(.data$directional_dummy == 1, "buildout", "phaseout")) %>%
    dplyr::select(-"directional_dummy")

  # add activity unit
  data <- data %>%
    dplyr::inner_join(
      activity_units,
      by = c("sector", "technology")
    )

  # TODO: functionise and possibly run outside of this function
  # add technology share by direction
  data <- data %>%
    dplyr::mutate(
      prod_sector = sum(.data$projected, na.rm = TRUE),
      .by = c("sector", "year", "region", "scenario_source", "name_abcd", "group_id", "activity_unit")
    ) %>%
    dplyr::mutate(
      technology_share_by_direction = sum(.data$projected, na.rm = TRUE) / .data$prod_sector,
      .by = c("sector", "year", "region", "scenario_source", "name_abcd", "group_id", "direction", "activity_unit")
    ) %>%
    dplyr::select(-"prod_sector")

  # if gas_cap is a bridge tech, both sides of the scenario are treated as misaligned
  if (bridge_tech == "gascap") {
    data <- data %>%
      apply_bridge_technology_cap(
        bridge_tech = bridge_tech,
        target_scenario = target_scenario
      )
  }

  return(data)
}

apply_bridge_technology_cap <- function(data,
                                        bridge_tech,
                                        target_scenario) {
  data_cap <- data %>%
    dplyr::filter(.data$technology == .env$bridge_tech)

  data_cap <- data_cap %>%
    dplyr::mutate(
      total_tech_deviation = dplyr::case_when(
        .data$projected < !!rlang::sym(target_scenario) ~ .data$projected - !!rlang::sym(target_scenario),
        .data$projected > !!rlang::sym(target_scenario) ~ (.data$projected - !!rlang::sym(target_scenario)) * -1,
        TRUE ~ 0
      )
    )

  data <- data %>%
    dplyr::filter(.data$technology != .env$bridge_tech) %>%
    dplyr::bind_rows(data_cap)

  return(data)
}


#' Return company level sector alignment metric for each company with
#' option to disaggregate by buildout / phaseout.
#'
#' @param data data.frame. Holds company-technology deviations based on PACTA
#'   for Banks TMS results. Must have been calculated according to the
#'   green/brown logic of the CA100+ calculation.
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the alignment metrics. Currently,
#'   the only supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the alignment metric for. Must be a scenario available from `scenario_source`.
#' @param level Character. Vector that indicates if the aggreagte alignment
#'   metric should be returned based on the net technology deviations (`net`) or
#'   disaggregated into buildout and phaseout technologies (`bo_po`).
#'
#' @return NULL
#' @export
calculate_company_aggregate_alignment_tms <- function(data,
                                                      scenario_source = "geco_2021",
                                                      scenario = "1.5c",
                                                      level = c("net", "bo_po")) {

  # validate input values
  validate_input_args_calculate_company_aggregate_alignment_tms(
    scenario_source = scenario_source,
    scenario = scenario
  )

  # validate input data set
  validate_input_data_calculate_company_aggregate_alignment_tms(
    data = data,
    scenario = scenario
  )

  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)
  level <- match.arg(level)


  if (level == "bo_po") {
    # calculate buildout and phaseout sector alignment_metric
    data <- data %>%
      dplyr::mutate(
        net_absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .by = c("group_id", "name_abcd", "scenario_source", "region", "sector", "activity_unit", "year")
      ) %>%
      dplyr::summarise(
        total_deviation = sum(.data$total_tech_deviation, na.rm = TRUE),
        .by = c("group_id", "name_abcd", "scenario_source", "region", "sector", "activity_unit", "year", "net_absolute_scenario_value", "direction", "technology_share_by_direction")
      ) %>%
      dplyr::mutate(
        alignment_metric = .data$total_deviation / .data$net_absolute_scenario_value,
        scenario = .env$scenario
      ) %>%
      dplyr::select(
        c(
          "group_id", "name_abcd", "sector", "activity_unit", "region",
          "scenario_source", "scenario", "year", "direction", "total_deviation",
          "technology_share_by_direction", "alignment_metric"
        )
      )
  } else if (level == "net") {
    # calculate net sector alignment_metric
    data <- data %>%
      dplyr::summarise(
        total_deviation = sum(.data$total_tech_deviation, na.rm = TRUE),
        net_absolute_scenario_value = sum(!!rlang::sym(target_scenario), na.rm = TRUE),
        .by = c("group_id", "name_abcd", "scenario_source", "region", "sector", "activity_unit", "year")
      ) %>%
      dplyr::mutate(
        alignment_metric = .data$total_deviation / .data$net_absolute_scenario_value,
        scenario = .env$scenario,
        direction = .env$level
      ) %>%
      dplyr::select(
        c(
          "group_id", "name_abcd", "sector", "activity_unit", "region",
          "scenario_source", "scenario", "year", "direction", "total_deviation",
          "alignment_metric"
        )
      )
  }

  data <- data %>%
    dplyr::arrange(.data$group_id, .data$sector, .data$name_abcd, .data$region, .data$year)

  return(data)
}


#' Return company level sector alignment metric for each company
#'
#' @param data data.frame. Holds the PACTA for Banks SDA results on company level.
#' @param scenario_emission_intensities data frame containing the scenario file with
#'   information on yearly emission intensity levels.
#' @param scenario_source Character. Vector that indicates which scenario_source
#'   to use for reference in the calculation of the alignment metrics. Currently,
#'   the only supported value is `"geco_2021"`.
#' @param scenario Character. Vector that indicates which scenario to calculate
#'   the alignment metric for. Must be a scenario available from `scenario_source`.
#'
#' @return NULL
#' @export
calculate_company_aggregate_alignment_sda <- function(data,
                                                      scenario_emission_intensities,
                                                      scenario_source = "geco_2021",
                                                      scenario = "1.5c") {
  # validate input values
  validate_input_args_calculate_company_aggregate_alignment_sda(
    scenario_source = scenario_source,
    scenario = scenario
  )

  # validate input data set
  validate_input_data_calculate_company_aggregate_alignment_sda(
    data = data,
    scenario_emission_intensities = scenario_emission_intensities
  )

  start_year <- min(data$year, na.rm = TRUE)
  target_scenario <- paste0("target_", scenario)

  data <- data %>%
    dplyr::filter(.data$scenario_source == .env$scenario_source)

  data <- data %>%
    group_by(
      .data$group_id, .data$name_abcd, .data$emission_factor_metric, .data$year, .data$region,
      .data$scenario_source
    ) %>%
    dplyr::filter(.data$name_abcd != "market") %>%
    dplyr::filter(.data$emission_factor_metric %in% c("projected", paste0("target_", .env$scenario))) %>%
    dplyr::filter(dplyr::between(.data$year, left = .env$start_year, right = .env$start_year + 5)) %>%
    tidyr::pivot_wider(
      names_from = "emission_factor_metric",
      values_from = "emission_factor_value"
    ) %>%
    dplyr::ungroup()

  # calculate sector alignment metric
  data <- data %>%
    dplyr::group_by(
      .data$group_id, .data$name_abcd, .data$scenario_source, .data$region, .data$sector, .data$year
    ) %>%
    dplyr::mutate(
      total_deviation = (.data$projected - !!rlang::sym(target_scenario)) * -1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      direction = "net",
      alignment_metric = .data$total_deviation / !!rlang::sym(target_scenario)
    )

  activity_units_sector <- activity_units %>%
    dplyr::distinct(.data$sector, .data$activity_unit)

  data <- data %>%
    dplyr::mutate(scenario = .env$scenario) %>%
    dplyr::inner_join(activity_units_sector, by = "sector") %>%
    dplyr::select(
      c(
        "group_id", "name_abcd", "sector", "activity_unit", "region",
        "scenario_source", "scenario", "year", "direction", "total_deviation",
        "alignment_metric"
      )
    ) %>%
    dplyr::arrange(.data$group_id, .data$sector, .data$name_abcd, .data$region, .data$year)

  return(data)
}

validate_input_args_calculate_company_tech_deviation <- function(scenario_source,
                                                                 scenario,
                                                                 bridge_tech) {
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of class character. Please check your input.")
  }
  if (!length(scenario) == 1) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }
  if (!inherits(scenario, "character")) {
    stop("Argument scenario must be of class character. Please check your input.")
  }
  if (!length(bridge_tech) == 1) {
    stop("Argument bridge_tech must be of length 1. Please check your input.")
  }
  if (!inherits(bridge_tech, "character")) {
    stop("Argument bridge_tech must be of class character. Please check your input.")
  }



  invisible()
}

validate_input_data_calculate_company_tech_deviation <- function(data,
                                                                 technology_direction,
                                                                 scenario_trajectory,
                                                                 green_or_brown) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "sector", "technology", "year", "region", "scenario_source", "name_abcd",
      "metric", "production", "technology_share", "scope",
      "percentage_of_initial_production_by_scope", "group_id"
    )
  )

  validate_data_has_expected_cols(
    data = technology_direction,
    expected_columns = c(
      "scenario_source", "scenario", "sector", "technology", "region",
      "directional_dummy"
    )
  )

  validate_data_has_expected_cols(
    data = scenario_trajectory,
    expected_columns = c(
      "scenario_source", "scenario", "sector", "technology", "region", "year",
      "tmsr", "smsp"
    )
  )

  validate_data_has_expected_cols(
    data = green_or_brown,
    expected_columns = c(
      "sector", "technology", "green_or_brown"
    )
  )

  invisible()
}

validate_input_args_calculate_company_aggregate_alignment_tms <- function(scenario_source,
                                                                          scenario) {
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of class character. Please check your input.")
  }
  if (!length(scenario) == 1) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }
  if (!inherits(scenario, "character")) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }

  invisible()
}

validate_input_data_calculate_company_aggregate_alignment_tms <- function(data,
                                                                          scenario) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns = c(
      "sector", "technology", "year", "region", "scenario_source", "name_abcd",
      "group_id", "projected", paste0("target_", scenario), "direction",
      "total_tech_deviation", "activity_unit", "technology_share_by_direction"
    )
  )

  invisible()
}

validate_input_args_calculate_company_aggregate_alignment_sda <- function(scenario_source,
                                                                          scenario) {
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of class character. Please check your input.")
  }
  if (!length(scenario) == 1) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }
  if (!inherits(scenario, "character")) {
    stop("Argument scenario must be of length 1. Please check your input.")
  }

  invisible()
}


validate_input_data_calculate_company_aggregate_alignment_sda <- function(data,
                                                                          scenario_emission_intensities) {
  validate_data_has_expected_cols(
    data = data,
    expected_columns <- c(
      "sector", "year", "region", "scenario_source", "name_abcd",
      "emission_factor_metric", "emission_factor_value", "group_id"
    )
  )

  validate_data_has_expected_cols(
    data = scenario_emission_intensities,
    expected_columns <- c(
      "scenario_source", "scenario", "sector", "region", "year",
      "emission_factor", "emission_factor_unit"
    )
  )

  invisible()
}