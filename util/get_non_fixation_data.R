get_non_fixation_data <- function(df, scope = NA) {

  # If scope is not explicitly set, use scope boundary to include all rows
  if (missing(scope)) {
    scope <- list(start = 1, end = nrow(df))
  }

  # get fixation pairs
  fi_pairs <- get_fixationindex_pairs(df$FixationIndex)

  # init storage list to return
  non_fixation_data <- vector("list", length(scope$start))

  # define structure
  for (i in seq_along(non_fixation_data)) {
    non_fixation_data[[i]] <- list(
      start = c(),
      end = c(),
      durations = c()
    )
  }

  # loop over scope
  for (i in seq_along(scope$start)) {

    # subset df in current scope
    df_sub <- df[scope$start[i]:scope$end[i], , drop = FALSE]

    # filter all fixation indexes that are of type Fixation
    df_sub <- df_sub[which(df_sub$GazeEventType == "Fixation"), , drop = FALSE]

    # get all fixation indexes within current scope
    fixations_in_current_scope <- unique(df_sub$FixationIndex)

    # check if fixations_in_current_scope is empty, because it contained only NAs
    # ... if so assign NA to non_fixation_data start, end and durations
    if (length(fixations_in_current_scope) == 0) {
      non_fixation_data[[i]]$start <- NA
      non_fixation_data[[i]]$end <- NA
      non_fixation_data[[i]]$durations <- NA
      # go to next i
      next
    }


    # GET ALL NON FIXATION INDEX ROW NUMBERS (nfi_rn) IN CURRENT SCOPE
    # the start of a non(!) fixation rn is the end of a fixation rn + 1
    nfi_rn_start <- fi_pairs$fiend[fixations_in_current_scope] + 1
    # if the last row number is a fixation, then there is no nfi anymore, thus use the
    # ... last row number and pretend it is a nfi
    # ... (this more theoretical, and will not be relevant in practice)
    if (max(fi_pairs$fiend[fixations_in_current_scope]) == nrow(df)) {
      nfi_rn_start <- fi_pairs$fiend[fixations_in_current_scope]
    }

    # likewise for the end
    nfi_rn_end <- fi_pairs$fistart[fixations_in_current_scope] - 1
    # the first end value is not needed as we always need the first valid fixation
    nfi_rn_end <- nfi_rn_end[-1]
    # attach the end of current scope to the end to fill it up; matching with scope$start
    nfi_rn_end <- c(nfi_rn_end, scope$end[i] - 1)


    # get time diffs between fixation indexes
    time_diff <- df$RecordingTimestamp[nfi_rn_end + 1] - df$RecordingTimestamp[nfi_rn_start]

    # check if current duration is negative, which can happen if fixation indexes surpasses
    # ... the current scope end
    # ... if negative remove this last entry for start, end, durations
    if (time_diff[length(time_diff)] < 0) {
      nfi_rn_start <- nfi_rn_start[-length(nfi_rn_start)]
      nfi_rn_end <- nfi_rn_end[-length(nfi_rn_end)]
      time_diff <- time_diff[-length(time_diff)]
    }

    # save to list
    non_fixation_data[[i]]$start <- nfi_rn_start
    non_fixation_data[[i]]$end <- nfi_rn_end
    non_fixation_data[[i]]$durations <- time_diff
  }

  return(non_fixation_data)
}
