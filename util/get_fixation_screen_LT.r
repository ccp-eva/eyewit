get_fixation_screen_LT <- function(df, trial_startend, starttime = 0, endtime = 0, markercut = TRUE) {

  # Time Window Setup
  if (starttime != 0 && endtime != 0) {

    # overwrite trial_startend to match the time windows
    # Extract only the start position of each trial
    legacy_start_indexes <- trial_startend[seq(1, length(trial_startend), 2)]

    # Add the starttime argument to every start position of the recording timestamp to get the actual start window
    starting_times <- df$RecordingTimestamp[legacy_start_indexes] + starttime
    # Add the endtime argument to every start position to get the new
    ending_times <- starting_times + endtime

    # convert starting and ending times into the closest matching index
    # find closest match (http://adomingues.github.io/2015/09/24/finding-closest-element-to-a-number-in-a-list/)
    start_indexes <- unlist(
      lapply(
        starting_times,
        function(x) which.min(abs(df$RecordingTimestamp - x))
      )
    )
    # end index
    end_indexes <- unlist(
      lapply(
        ending_times,
        function(x) which.min(abs(df$RecordingTimestamp - x))
      )
    )

    # merge/sort new star and end indexes and overwrite the trial_startend argument
    trial_startend <- sort(c(start_indexes, end_indexes))

  }



  GazeEventDurations <- c() # sums up looking times for entire screen (GazeEventDuration)


  while (length(trial_startend) > 0) {

    # get the current trial pair
    current_start_pos <- trial_startend[1]
    current_end_pos <- trial_startend[2]

    # get all FixationIndexes in a trial
    inter_trial_FixationIndexes <- df$FixationIndex[current_start_pos:current_end_pos]

    # filter all NAs and check if length of inter_trial_FixationIndexes == 0. If so skip current trial
    if (length(na.omit(inter_trial_FixationIndexes)) == 0) {
      # Append 0 to current trials and NA to FirstLook in this trial
      GazeEventDurations <- c(GazeEventDurations, 0)
      # remove current not working index
      trial_startend <- trial_startend[!trial_startend %in% c(current_start_pos, current_end_pos)]
      # go to next trial
      next
    }

    # get first and last FixationIndex (remove NAs)
    min_FixationIndex <- min(inter_trial_FixationIndexes, na.rm = TRUE)
    max_FixationIndex <- max(inter_trial_FixationIndexes, na.rm = TRUE)

    # set/reset to current trial duration to 0
    current_trial_total_GazeEventDurations <- 0

    # operate WITHIN the current fixation pair (i.e., within a trial)
    for (i in min_FixationIndex:max_FixationIndex) {

      AOIs_in_current_FixationIndex <- df$AOIScreen[which(df$FixationIndex == i)]

      # skip processing if TRUE and FALSE is in the same fixation index
      if (TRUE %in% AOIs_in_current_FixationIndex && FALSE %in% AOIs_in_current_FixationIndex) {
        warning(paste("There is TRUE and FALSE in the current fixation index:", i, sep = " "))
        # next (we don't use this anymore as it is conflicting with other durations counts
        # ... see  pilot subj "emilia trial 23 familiar phase
        # (117ms get a count for screen and not for aoi left, and 208ms gets counted for AOIFamBodyObj and not for Screen :/
      }

      # check if TRUE is in current pair, if so, add it
      if (TRUE %in% AOIs_in_current_FixationIndex) {

        # check if the first fixation index started before the current_start_pos and if markercut is TRUE
        if (i == min_FixationIndex && which(df$FixationIndex == i)[1] < current_start_pos && markercut) {
          # get start and end milliseconds
          start_ms <- df$RecordingTimestamp[current_start_pos]
          end_ms <-  df$RecordingTimestamp[which(df$FixationIndex == i)][length(which(df$FixationIndex == i))]

          # set the difference of start_ms and end_ms to the current GazeEventDuration
          current_GazeEventDuration <- end_ms - start_ms
          # Add it to the total
          current_trial_total_GazeEventDurations <- current_trial_total_GazeEventDurations + current_GazeEventDuration
          # skip to the next fixation index
          next
        }

        # check if the last fixation index continues after the current_end_pos and if markercut is TRUE
        if (i == max_FixationIndex && which(df$FixationIndex == i)[length(which(df$FixationIndex == i))] > current_end_pos && markercut) {
          # get start and end milliseconds
          start_ms <- df$RecordingTimestamp[which(df$FixationIndex == i)][1]
          end_ms <-  df$RecordingTimestamp[current_end_pos]

          # set the difference of start_ms and end_ms to the current GazeEventDuration
          current_GazeEventDuration <- end_ms - start_ms
          # Add it to the total
          current_trial_total_GazeEventDurations <- current_trial_total_GazeEventDurations + current_GazeEventDuration
          # skip to the next fixation index
          next
        }

        # ========== Regular "middle" section of fixation indexes ==========
        # Grab the current GazeEventDuration chunk and select the first value
        current_GazeEventDuration <- df$GazeEventDuration[which(df$FixationIndex == i)][1]

        # Add it to the total
        current_trial_total_GazeEventDurations <- current_trial_total_GazeEventDurations + current_GazeEventDuration
      }
    }

    # Append it to the Trial Lists
    GazeEventDurations <- c(GazeEventDurations, current_trial_total_GazeEventDurations)

    # remove current pair, so it continues with the next pair/trial in the while loop
    trial_startend <- trial_startend[!trial_startend %in% c(current_start_pos, current_end_pos)]

  }

  return(GazeEventDurations)
}
