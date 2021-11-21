get_lookaway_scope_end <- function(df, scope, lookaway_stop) {

  # destructure provided scope
  scope_end <- scope$end

  # get non-fixation data
  nf_data <- get_non_fixation_data(df, scope)

  # loop over durations within nf_data to check which durations are greater lookaway_stop
  for (i in seq_along(nf_data)) {

    # check if lookaway_stop criterion is fulfilled
    if (TRUE %in% (nf_data[[i]]$durations > lookaway_stop)) {

      # get the index of the very first element being greater equal the lookaway_stop
      # ... as all subsequent elements great equal the lookaway_stop must be ignored

      # get the duration index that matches the criterion
      lookaway_index <- min(which(nf_data[[i]]$durations >= lookaway_stop))

      # overwrite scope_end pos (subtract 1 to get the end of the last
      # ... fixation satisfying time criterion)
      scope_end[i] <- nf_data[[i]]$start[lookaway_index] - 1
    }
  }

  return(scope_end)
}


