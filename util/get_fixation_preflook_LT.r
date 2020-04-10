get_fixation_preflook_LT <- function(df, trial_startend) {

  GazeEventDurations_left <- c() # left object
  GazeEventDurations_right <- c() # right object
  FirstLook_list <- c()


  while (length(trial_startend) > 0) {

    # get the current trial pair
    current_start_pos <- trial_startend[1]
    current_end_pos <- trial_startend[2]

    # get all FixationIndexes in a trial
    inter_trial_FixationIndexes <- df$FixationIndex[current_start_pos:current_end_pos]

    # filter all NAs and check if length of inter_trial_FixationIndexes == 0. If so skip current trial
    if (length(na.omit(inter_trial_FixationIndexes)) == 0) {
      # Append 0 to current trials and NA to FirstLook in this trial
      GazeEventDurations_left <- c(GazeEventDurations_left, 0)
      GazeEventDurations_right <- c(GazeEventDurations_right, 0)
      FirstLook_list <- c(FirstLook_list, NA)
      # remove current not working index
      trial_startend <- trial_startend[!trial_startend %in% c(current_start_pos, current_end_pos)]
      # go to next trial
      next
    }

    # get first and last FixationIndex (remove NAs)
    min_FixationIndex <- min(inter_trial_FixationIndexes, na.rm = TRUE)
    max_FixationIndex <- max(inter_trial_FixationIndexes, na.rm = TRUE)

    # set/reset to current trial duration to 0
    current_trial_total_GazeEventDurations_left <- 0
    current_trial_total_GazeEventDurations_right <- 0

    found_first_look <- FALSE
    first_look <- ""

    # operate WITHIN the current fixation pair (i.e., within a trial)
    for (i in min_FixationIndex:max_FixationIndex) {
      AOIs_in_current_FixationIndex <- df$AOIPrefLook[which(df$FixationIndex == i)]

      # stop processing if"left" and "right" is in the current chunk
      if ("left" %in% AOIs_in_current_FixationIndex && "right" %in% AOIs_in_current_FixationIndex) {
        warning(paste("In current fixation index:", i, "are left AND right AOIs! Skipping this index!", sep = " "))
        next
      }

      # check if "left" is in current pair, if so, add it
      if ("left" %in% AOIs_in_current_FixationIndex) {

        # Grab the current GazeEventDuration chunk and select the first value
        current_GazeEventDuration <- df$GazeEventDuration[which(df$FixationIndex == i)][1]

        # Add it to the total
        current_trial_total_GazeEventDurations_left <- current_trial_total_GazeEventDurations_left + current_GazeEventDuration

        # set first_look left if flag is not set
        if (!found_first_look) {
          first_look <- "left"
          found_first_look <- TRUE
        }
      }

      # same for right
      if ("right" %in% AOIs_in_current_FixationIndex) {
        current_GazeEventDuration <- df$GazeEventDuration[which(df$FixationIndex == i)][1]
        current_trial_total_GazeEventDurations_right <- current_trial_total_GazeEventDurations_right + current_GazeEventDuration

        # set first_look left if flag is not set
        if (!found_first_look) {
          first_look <- "right"
          found_first_look <- TRUE
        }
      }
    }

    # Append it to the Trial Lists
    GazeEventDurations_left <- c(GazeEventDurations_left, current_trial_total_GazeEventDurations_left)
    GazeEventDurations_right <- c(GazeEventDurations_right, current_trial_total_GazeEventDurations_right)

    # Append first look to list
    # check if first_look was there
    if (first_look == "") {
      first_look = NA
    }
    FirstLook_list <- c(FirstLook_list, first_look)

    # remove current pair, so it continues with the next pair/trial in the while loop
    trial_startend <- trial_startend[!trial_startend %in% c(current_start_pos, current_end_pos)]

  }

  return(
    list(
      left = GazeEventDurations_left,
      right = GazeEventDurations_right,
      firstlook = FirstLook_list
    )
  )
}
