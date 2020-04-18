get_fixation_famBodyObj_LT <- function(df, trial_startend, starttime = 0, endtime = 0, markercut = TRUE) {


  # Time Window Setup
  if (starttime != 0 && endtime != 0) {

    # overwrite trial_startend to match the time windows
    # Extract only the start position of each trial
    legacy_start_indexes <- trial_startend$start

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
    trial_startend <- list(start = start_indexes, end = end_indexes)

  }


  GazeEventDurations_left <- c() # left actor
  GazeEventDurations_center <- c() # center object
  GazeEventDurations_right <- c() # right actor
  FirstLook_list <- c()


  while (length(trial_startend$start) > 0) {

    # get the current trial pair
    current_start_pos <- trial_startend$start[1]
    current_end_pos <- trial_startend$end[1]

    # get all FixationIndexes in a trial
    inter_trial_FixationIndexes <- df$FixationIndex[current_start_pos:current_end_pos]

    # filter all NAs and check if length of inter_trial_FixationIndexes == 0. If so skip current trial
    if (length(na.omit(inter_trial_FixationIndexes)) == 0) {
      # Append 0 to current trials and NA to FirstLook in this trial
      GazeEventDurations_left <- c(GazeEventDurations_left, 0)
      GazeEventDurations_center <- c(GazeEventDurations_center, 0)
      GazeEventDurations_right <- c(GazeEventDurations_right, 0)
      FirstLook_list <- c(FirstLook_list, NA)
      # remove current not working index
      trial_startend$start <- trial_startend$start[!trial_startend$start %in% c(current_start_pos)]
      trial_startend$end <- trial_startend$end[!trial_startend$end %in% c(current_end_pos)]
      # go to next trial
      next
    }

    # get first and last FixationIndex (remove NAs)
    min_FixationIndex <- min(inter_trial_FixationIndexes, na.rm = TRUE)
    max_FixationIndex <- max(inter_trial_FixationIndexes, na.rm = TRUE)

    # set/reset to current trial duration to 0
    current_trial_total_GazeEventDurations_left <- 0
    current_trial_total_GazeEventDurations_center <- 0
    current_trial_total_GazeEventDurations_right <- 0

    found_first_look <- FALSE
    first_look <- ""

    # operate WITHIN the current fixation pair (i.e., within a trial)
    for (i in min_FixationIndex:max_FixationIndex) {

      AOIs_in_current_FixationIndex <- df$AOIFamBodyObj[which(df$FixationIndex == i)]

      # stop processing if"left" and "right" is in the current chunk and other combinations
      if ("left" %in% AOIs_in_current_FixationIndex && "right" %in% AOIs_in_current_FixationIndex) {
        stop(paste("In current pair", i, "are left AND right AOIs! Skipping this index!", sep = " "))
        next
      }
      if ("left" %in% AOIs_in_current_FixationIndex && "center" %in% AOIs_in_current_FixationIndex) {
        stop(paste("In current pair", i, "are left AND center AOIs! Skipping this index!", sep = " "))
        next
      }
      if ("right" %in% AOIs_in_current_FixationIndex && "center" %in% AOIs_in_current_FixationIndex) {
        stop(paste("In current pair", i, "are right AND center AOIs! Skipping this index!", sep = " "))
        next
      }

      # check if "left" is in current pair, if so, add it
      if ("left" %in% AOIs_in_current_FixationIndex) {

        # check if the first fixation index started before the current_start_pos and if markercut is TRUE
        if (i == min_FixationIndex && which(df$FixationIndex == i)[1] < current_start_pos && markercut) {
          # get start and end milliseconds
          start_ms <- df$RecordingTimestamp[current_start_pos]
          end_ms <-  df$RecordingTimestamp[which(df$FixationIndex == i)][length(which(df$FixationIndex == i))]

          # set the difference of start_ms and end_ms to the current GazeEventDuration
          current_GazeEventDuration <- end_ms - start_ms
          # Add it to the total
          current_trial_total_GazeEventDurations_left <- current_trial_total_GazeEventDurations_left + current_GazeEventDuration

          # set first_look left if flag is not set
          if (!found_first_look) {
            first_look <- "left"
            found_first_look <- TRUE
          }

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
          current_trial_total_GazeEventDurations_left <- current_trial_total_GazeEventDurations_left + current_GazeEventDuration

          # set first_look left if flag is not set
          if (!found_first_look) {
            first_look <- "left"
            found_first_look <- TRUE
          }

          # skip to the next fixation index
          next
        }

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

      # same for center
      if ("center" %in% AOIs_in_current_FixationIndex) {

        # check if the first fixation index started before the current_start_pos and if markercut is TRUE
        if (i == min_FixationIndex && which(df$FixationIndex == i)[1] < current_start_pos && markercut) {
          # get start and end milliseconds
          start_ms <- df$RecordingTimestamp[current_start_pos]
          end_ms <-  df$RecordingTimestamp[which(df$FixationIndex == i)][length(which(df$FixationIndex == i))]

          # set the difference of start_ms and end_ms to the current GazeEventDuration
          current_GazeEventDuration <- end_ms - start_ms
          # Add it to the total
          current_trial_total_GazeEventDurations_center <- current_trial_total_GazeEventDurations_center + current_GazeEventDuration

          # set first_look left if flag is not set
          if (!found_first_look) {
            first_look <- "center"
            found_first_look <- TRUE
          }

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
          current_trial_total_GazeEventDurations_center <- current_trial_total_GazeEventDurations_center + current_GazeEventDuration

          # set first_look left if flag is not set
          if (!found_first_look) {
            first_look <- "center"
            found_first_look <- TRUE
          }

          # skip to the next fixation index
          next
        }

        current_GazeEventDuration <- df$GazeEventDuration[which(df$FixationIndex == i)][1]
        current_trial_total_GazeEventDurations_center <- current_trial_total_GazeEventDurations_center + current_GazeEventDuration

        # set first_look left if flag is not set
        if (!found_first_look) {
          first_look <- "center"
          found_first_look <- TRUE
        }
      }

      # same for right
      if ("right" %in% AOIs_in_current_FixationIndex) {

        # check if the first fixation index started before the current_start_pos and if markercut is TRUE
        if (i == min_FixationIndex && which(df$FixationIndex == i)[1] < current_start_pos && markercut) {
          # get start and end milliseconds
          start_ms <- df$RecordingTimestamp[current_start_pos]
          end_ms <-  df$RecordingTimestamp[which(df$FixationIndex == i)][length(which(df$FixationIndex == i))]

          # set the difference of start_ms and end_ms to the current GazeEventDuration
          current_GazeEventDuration <- end_ms - start_ms
          # Add it to the total
          current_trial_total_GazeEventDurations_right <- current_trial_total_GazeEventDurations_right + current_GazeEventDuration

          # set first_look left if flag is not set
          if (!found_first_look) {
            first_look <- "right"
            found_first_look <- TRUE
          }

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
          current_trial_total_GazeEventDurations_right <- current_trial_total_GazeEventDurations_right + current_GazeEventDuration

          # set first_look left if flag is not set
          if (!found_first_look) {
            first_look <- "right"
            found_first_look <- TRUE
          }

          # skip to the next fixation index
          next
        }

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
    GazeEventDurations_center <- c(GazeEventDurations_center, current_trial_total_GazeEventDurations_center)
    GazeEventDurations_right <- c(GazeEventDurations_right, current_trial_total_GazeEventDurations_right)

    # Append first look to list
    # check if first_look was there
    if (first_look == "") {
      first_look = NA
    }
    FirstLook_list <- c(FirstLook_list, first_look)

    # remove current pair, so it continues with the next pair/trial in the while loop
    trial_startend$start <- trial_startend$start[!trial_startend$start %in% c(current_start_pos)]
    trial_startend$end <- trial_startend$end[!trial_startend$end %in% c(current_end_pos)]

  }

  return(
    list(
      left = GazeEventDurations_left,
      center = GazeEventDurations_center,
      right = GazeEventDurations_right,
      firstlook = FirstLook_list
    )
  )
}
