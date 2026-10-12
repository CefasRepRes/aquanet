library(aquanet)

site_ids <- c("S1", "S2", "S3")
movement_probabilities <- Matrix::sparseMatrix(
  i = c(1L, 1L, 3L),
  j = c(2L, 3L, 1L),
  x = c(0.2, 0.3, 0.4),
  dims = c(3L, 3L),
  dimnames = list(site_ids, site_ids)
)

make_catchment_movements <- function(type) {
  list(
    spmatrix_sites_catchment = Matrix::sparseMatrix(
      i = c(1L, 2L, 3L),
      j = c(1L, 1L, 2L),
      x = c(1, 1, 1),
      dims = c(3L, 2L),
      dimnames = list(site_ids, c("C1", "C2"))
    ),
    lgmatrix_catch_catch = Matrix::Matrix(
      matrix(c(TRUE, TRUE, FALSE,
               TRUE, TRUE, FALSE,
               FALSE, FALSE, TRUE), nrow = 3L),
      sparse = TRUE,
      dimnames = list(site_ids, site_ids)
    ),
    catchments_controlled_prev = methods::as(
      Matrix::Matrix(nrow = 2L, ncol = 1L, data = 0, sparse = TRUE),
      "unpackedMatrix"
    ),
    matrix_contacts_exclude = methods::new(
      Class = "dgTMatrix",
      Dim = c(3L, 3L)
    ),
    type_catchment_controls = type,
    sites_controlled = logical(3L),
    n_catchments_controlled = 0
  )
}

full_movement_probabilities <- Matrix::sparseMatrix(
  i = c(1L, 1L, 2L, 2L, 3L, 3L),
  j = c(2L, 3L, 1L, 3L, 1L, 2L),
  x = c(0.11, 0.12, 0.21, 0.22, 0.31, 0.32),
  dims = c(3L, 3L),
  dimnames = list(site_ids, site_ids)
)
expected_full_movements <- matrix(
  c(0, 0.11, 0.12,
    0.21, 0, 0.22,
    0.31, 0.32, 0),
  nrow = 3L,
  byrow = TRUE,
  dimnames = list(site_ids, site_ids)
)

filter_movements <- function(type = "None",
                             restricted = rep(FALSE, 3L),
                             catchment = make_catchment_movements(type),
                             source_allowed = NULL,
                             destination_allowed = NULL) {
  arguments <- list(
    move_restricted_sites = restricted,
    river_distances_df = data.frame(),
    spmatrix_risk_contacts = full_movement_probabilities,
    catchment_movements = catchment,
    matrix_movements_prob = full_movement_probabilities,
    river_downstream_transmission_matrix = full_movement_probabilities,
    site_details = data.frame(siteID = site_ids, tidal = c(FALSE, TRUE, FALSE))
  )
  if (!is.null(source_allowed)) {
    arguments$source_sites_allowed <- source_allowed
  }
  if (!is.null(destination_allowed)) {
    arguments$destination_sites_allowed <- destination_allowed
  }
  do.call(aquanet::excludeWithinCatchmentMovements, arguments)
}

expect_movement_matrix <- function(result, expected) {
  stopifnot(identical(as.matrix(result$spmatrix_risk_contacts), expected))
}

## source and destination eligibility filtering
omitted_eligibility <- filter_movements()
expect_movement_matrix(omitted_eligibility, expected_full_movements)
source_filtered <- filter_movements(source_allowed = c(FALSE, TRUE, TRUE))
expect_movement_matrix(
  source_filtered,
  matrix(c(0, 0, 0,
           0.21, 0, 0.22,
           0.31, 0.32, 0),
         nrow = 3L, byrow = TRUE, dimnames = list(site_ids, site_ids))
)
destination_filtered <- filter_movements(destination_allowed = c(TRUE, FALSE, TRUE))
expect_movement_matrix(
  destination_filtered,
  matrix(c(0, 0, 0.12,
           0.21, 0, 0.22,
           0.31, 0, 0),
         nrow = 3L, byrow = TRUE, dimnames = list(site_ids, site_ids))
)
source_and_destination_filtered <- filter_movements(
  source_allowed = c(TRUE, FALSE, TRUE),
  destination_allowed = c(TRUE, FALSE, TRUE)
)
expect_movement_matrix(
  source_and_destination_filtered,
  matrix(c(0, 0, 0.12,
           0, 0, 0,
           0.31, 0, 0),
         nrow = 3L, byrow = TRUE, dimnames = list(site_ids, site_ids))
)
zero_edge_result <- filter_movements(source_allowed = rep(FALSE, 3L))
expect_movement_matrix(
  zero_edge_result,
  matrix(0, nrow = 3L, ncol = 3L, dimnames = list(site_ids, site_ids))
)
stopifnot(identical(dimnames(source_and_destination_filtered$spmatrix_risk_contacts),
                    list(site_ids, site_ids)))

## catchment control modes with and without restricted sites
expected_mode_zero <- matrix(
  c(0, 0.11, 0,
    0.21, 0, 0,
    0, 0, 0),
  nrow = 3L,
  byrow = TRUE,
  dimnames = list(site_ids, site_ids)
)
expected_mode_two <- matrix(0, nrow = 3L, ncol = 3L,
                            dimnames = list(site_ids, site_ids))
expected_control_mask_mode_zero <- matrix(
  c(0, 0, 0.12,
    0, 0, 0.22,
    0.31, 0.32, 0),
  nrow = 3L,
  byrow = TRUE,
  dimnames = list(site_ids, site_ids)
)
expected_control_mask_mode_two <- expected_full_movements

for (control_type in list("None", 0, 1, 2)) {
  restricted_result <- filter_movements(
    type = control_type,
    restricted = c(TRUE, FALSE, TRUE)
  )
  if (identical(control_type, 0)) {
    expect_movement_matrix(restricted_result, expected_mode_zero)
    stopifnot(identical(as.matrix(
      restricted_result$catchment_movements$matrix_contacts_exclude
    ), expected_control_mask_mode_zero))
    stopifnot(identical(
      as.numeric(restricted_result$catchment_movements$sites_controlled),
      c(1, 1, 1)
    ))
  } else if (identical(control_type, 1)) {
    expect_movement_matrix(restricted_result, expected_full_movements)
    stopifnot(identical(as.matrix(
      restricted_result$catchment_movements$matrix_contacts_exclude
    ), matrix(0, 3L, 3L, dimnames = list(site_ids, site_ids))))
  } else if (identical(control_type, 2)) {
    expect_movement_matrix(restricted_result, expected_mode_two)
    stopifnot(identical(as.matrix(
      restricted_result$catchment_movements$matrix_contacts_exclude
    ), expected_control_mask_mode_two))
  } else {
    expect_movement_matrix(restricted_result, expected_full_movements)
    stopifnot(restricted_result$catchment_movements$n_catchments_controlled == 0)
  }
  stopifnot(restricted_result$catchment_movements$n_catchments_controlled ==
              if (identical(control_type, "None")) 0 else 2)

  unrestricted_result <- filter_movements(
    type = control_type,
    restricted = rep(FALSE, 3L)
  )
  expect_movement_matrix(unrestricted_result, expected_full_movements)
  stopifnot(unrestricted_result$catchment_movements$n_catchments_controlled == 0)
  stopifnot(identical(
    as.numeric(unrestricted_result$catchment_movements$catchments_controlled_prev),
    c(0, 0)
  ))
}

## reuse the catchment mask while eligibility changes, then update and clear controls
reuse_first <- filter_movements(type = 0, restricted = c(TRUE, FALSE, FALSE))
expect_movement_matrix(
  reuse_first,
  matrix(c(0, 0.11, 0,
           0.21, 0, 0,
           0.31, 0.32, 0),
         nrow = 3L, byrow = TRUE, dimnames = list(site_ids, site_ids))
)
reuse_second <- filter_movements(
  type = 0,
  restricted = c(FALSE, TRUE, FALSE),
  catchment = reuse_first$catchment_movements,
  source_allowed = c(TRUE, FALSE, TRUE),
  destination_allowed = c(TRUE, TRUE, FALSE)
)
expect_movement_matrix(
  reuse_second,
  matrix(c(0, 0.11, 0,
           0, 0, 0,
           0.31, 0.32, 0),
         nrow = 3L, byrow = TRUE, dimnames = list(site_ids, site_ids))
)
stopifnot(identical(
  as.matrix(reuse_second$catchment_movements$matrix_contacts_exclude),
  as.matrix(reuse_first$catchment_movements$matrix_contacts_exclude)
))
stopifnot(reuse_second$catchment_movements$n_catchments_controlled == 1)
reuse_changed <- filter_movements(
  type = 0,
  restricted = c(FALSE, FALSE, TRUE),
  catchment = reuse_second$catchment_movements
)
expect_movement_matrix(
  reuse_changed,
  matrix(c(0, 0.11, 0.12,
           0.21, 0, 0.22,
           0, 0, 0),
         nrow = 3L, byrow = TRUE, dimnames = list(site_ids, site_ids))
)
stopifnot(identical(
  as.matrix(reuse_changed$catchment_movements$matrix_contacts_exclude),
  matrix(c(0, 0, 0,
           0, 0, 0,
           0.31, 0.32, 0),
         nrow = 3L, byrow = TRUE, dimnames = list(site_ids, site_ids))
))
stopifnot(identical(
  as.numeric(reuse_changed$catchment_movements$catchments_controlled_prev),
  c(0, 1)
))
reuse_cleared <- filter_movements(
  type = 0,
  restricted = rep(FALSE, 3L),
  catchment = reuse_changed$catchment_movements
)
expect_movement_matrix(reuse_cleared, expected_full_movements)
stopifnot(reuse_cleared$catchment_movements$n_catchments_controlled == 0)
stopifnot(identical(
  as.matrix(reuse_cleared$catchment_movements$matrix_contacts_exclude),
  matrix(0, 3L, 3L, dimnames = list(site_ids, site_ids))
))

## excludeWithinCatchmentMovements
exclude_result <- aquanet::excludeWithinCatchmentMovements(
  move_restricted_sites = c(TRUE, FALSE, FALSE),
  river_distances_df = data.frame(),
  spmatrix_risk_contacts = movement_probabilities,
  catchment_movements = make_catchment_movements(0),
  matrix_movements_prob = movement_probabilities,
  river_downstream_transmission_matrix = movement_probabilities,
  site_details = data.frame(
    siteID = site_ids,
    tidal = c(FALSE, TRUE, FALSE)
  )
)

expected_risk_contacts <- matrix(
  c(0, 0, 0.4,
    0.2, 0, 0,
    0, 0, 0),
  nrow = 3L,
  dimnames = list(site_ids, site_ids)
)
stopifnot(identical(as.matrix(exclude_result$spmatrix_risk_contacts),
                    expected_risk_contacts))
stopifnot(identical(
  as.numeric(exclude_result$catchment_movements$sites_controlled),
  c(1, 1, 0)
))
stopifnot(exclude_result$catchment_movements$n_catchments_controlled == 1)

## updateRates
zero_contacts <- Matrix::sparseMatrix(
  i = integer(),
  j = integer(),
  x = numeric(),
  dims = c(3L, 3L),
  dimnames = list(site_ids, site_ids)
)
rates_result <- aquanet::updateRates(
  control_matrix = matrix(0, nrow = 3L, ncol = 7L),
  state_vector = c(1, 0, 0),
  farm_vector = c(1, 0, 1),
  culling_vector = c(0, 0, 0),
  site_indices = 0:2,
  catchment_movements = make_catchment_movements("None"),
  movements_prob = list(matrix_movements_prob = movement_probabilities),
  movements_prob_top_sites_removed = list(
    matrix_movements_prob = movement_probabilities
  ),
  river_prob = list(matrix_river_distances_prob = zero_contacts),
  site_distances_prob = list(matrix_distances_probability = zero_contacts),
  run_time_params = data.frame(
    Site_Recovers = 4,
    Infection_Becomes_Subclinical = 10,
    Clearing_Of_Latency_From_Infected_Sites = 20
  ),
  non_peak_season = TRUE,
  contact_tracing = FALSE,
  remove_top_sites = FALSE,
  sites_states_cumulative = c(1, 0, 0),
  n_infections_remove_top_sites = 1,
  disease_controls = FALSE,
  river_distances_df = data.frame(),
  site_details = data.frame(
    siteID = site_ids,
    modelID = 1:3,
    tidal = c(FALSE, TRUE, FALSE)
  )
)

rates <- rates_result$trans_rates
stopifnot(identical(as.numeric(rates$rate_type), c(0, 0, 3)))
stopifnot(identical(as.numeric(rates$position), c(1, 2, 0)))
stopifnot(isTRUE(all.equal(as.numeric(rates$rate), c(0.2, 0.3, 0.25))))
stopifnot(identical(is.na(rates$source), c(FALSE, FALSE, TRUE)))
stopifnot(identical(
  as.numeric(rates$source[!is.na(rates$source)]),
  c(0, 0)
))
stopifnot(identical(
  rates_result$sites_all_movement_restricted,
  c(FALSE, FALSE, FALSE)
))

## updateRates forwards source and destination eligibility to movement filtering
eligibility_rates_result <- aquanet::updateRates(
  control_matrix = matrix(c(0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 1, 0,
                           0, 0, 0, 0, 0, 0, 0),
                         nrow = 3L, byrow = TRUE),
  state_vector = c(1, 0, 0),
  farm_vector = c(1, 0, 1),
  culling_vector = c(0, 0, 0),
  site_indices = 0:2,
  catchment_movements = make_catchment_movements("None"),
  movements_prob = list(matrix_movements_prob = full_movement_probabilities),
  movements_prob_top_sites_removed = list(
    matrix_movements_prob = full_movement_probabilities
  ),
  river_prob = list(matrix_river_distances_prob = zero_contacts),
  site_distances_prob = list(matrix_distances_probability = zero_contacts),
  run_time_params = data.frame(
    Site_Recovers = 4,
    Infection_Becomes_Subclinical = 10,
    Clearing_Of_Latency_From_Infected_Sites = 20
  ),
  non_peak_season = TRUE,
  contact_tracing = FALSE,
  remove_top_sites = FALSE,
  sites_states_cumulative = c(1, 0, 0),
  n_infections_remove_top_sites = 1,
  disease_controls = FALSE,
  river_distances_df = data.frame(),
  site_details = data.frame(
    siteID = site_ids,
    modelID = 1:3,
    tidal = c(FALSE, TRUE, FALSE)
  )
)
eligibility_infection_rates <- eligibility_rates_result$trans_rates
infection_rate_rows <- eligibility_infection_rates$rate_type == 0
stopifnot(identical(
  as.numeric(eligibility_infection_rates$position[infection_rate_rows]),
  2
))
stopifnot(isTRUE(all.equal(
  as.numeric(eligibility_infection_rates$rate[infection_rate_rows]),
  0.12
)))
stopifnot(identical(
  as.numeric(eligibility_infection_rates$source[infection_rate_rows]),
  0
))
stopifnot(identical(
  as.numeric(eligibility_rates_result$catchment_movements$catchments_controlled_prev),
  c(0, 0)
))

## runSimulations
simulation_site_ids <- "S1"
simulation_matrix <- Matrix::sparseMatrix(
  i = integer(),
  j = integer(),
  x = numeric(),
  dims = c(1L, 1L),
  dimnames = list(simulation_site_ids, simulation_site_ids)
)
simulation_root <- tempfile("aquanet-runSimulations-")
simulation_paths <- list(
  results = file.path(simulation_root, "results"),
  results_batch = file.path(simulation_root, "results_batch"),
  results_full = file.path(simulation_root, "results_full")
)
invisible(lapply(simulation_paths, dir.create, recursive = TRUE))

simulation_result <- local({
  previous_directory <- setwd(simulation_root)
  on.exit(setwd(previous_directory), add = TRUE)
  aquanet::runSimulations(
    n_cores = 1,
    n_sims = 1,
    seed_num = 2468,
    clear_results = FALSE,
    tmax = 1e6,
    run_time_params = data.frame(
      Site_Recovers = 1,
      Infection_Becomes_Subclinical = 1,
      Clearing_Of_Latency_From_Infected_Sites = 1,
      Second_Outbreak_Due_To_Subclinical_Infection = 1,
      Fomite_Transmission_Independant_Prob = 1,
      Early_Controls_Fisheries = 0,
      Late_Controls_Fisheries = 0
    ),
    non_peak_season_length = "0",
    out_createContactProbabilityMatrix = list(
      n_sites = 1L,
      matrix_movements_prob = simulation_matrix
    ),
    out_createContactProbabilityMatrixTopSitesRemoved = list(
      n_sites = 1,
      matrix_movements_prob = simulation_matrix
    ),
    out_createWithinCatchmentEdges = list(
      lgmatrix_catch_catch = Matrix::Matrix(matrix(TRUE), sparse = TRUE)
    ),
    out_createCatchmentToSiteMatrix = list(
      spmatrix_catchment_sites = Matrix::sparseMatrix(
        i = 1L,
        j = 1L,
        x = 1,
        dims = c(1L, 1L),
        dimnames = list(simulation_site_ids, "C1")
      )
    ),
    out_createRiverDistanceProbabilityMatrix = list(
      matrix_river_distances_prob = simulation_matrix
    ),
    out_createDistanceMatrix = list(
      matrix_distances_probability = simulation_matrix
    ),
    farm_vector = 1,
    n_states = 20,
    n_initial_infections = 1,
    type_catchment_controls = "None",
    filepath_results = simulation_paths,
    contact_tracing = FALSE,
    remove_top_sites = FALSE,
    n_infections_remove_top_sites = 1,
    disease_controls = FALSE,
    proportion_cullable = 0,
    days_before_catchment_restock = 0,
    river_distances_df = data.frame(),
    site_details = data.frame(
      siteID = simulation_site_ids,
      modelID = 1,
      tidal = FALSE
    ),
    stochastic_run = FALSE,
    seed_farm_choice = "S1"
  )
})

stopifnot(simulation_result[[1]] == 1)
stopifnot(identical(as.numeric(simulation_result[[2]]), 1))

summary_env <- new.env()
load(file.path(simulation_paths$results_batch, "batchNo-1.RData"),
     envir = summary_env)
summary_values <- as.matrix(summary_env$output_summary_states)[, 1:3, drop = FALSE]
expected_state_counts <- matrix(0, nrow = 20L, ncol = 3L)
expected_state_counts[10L, 1L] <- 1
expected_state_counts[16L, 2L] <- 1
expected_state_counts[10L, 3L] <- 1

stopifnot(identical(
  unname(summary_values[9:28, , drop = FALSE]),
  unname(expected_state_counts)
))

stopifnot(identical(
  unname(summary_values[c(1L, 2L, 5L, 6L, 7L, 8L), , drop = FALSE]),
  unname(rbind(
    c(1, 1, 1),
    c(1, 1, 1),
    c(1, 1, 1),
    c(0, 3, 4),
    c(0, 0, 0),
    c(1, 1, 1)
  ))
))
