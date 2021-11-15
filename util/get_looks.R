get_looks <- function(df, aoi_collection, scope = NA, intra_scope_window = c("start", "end"), lookaway_stop = NA, intra_scope_cut = TRUE) {

  # If scope is not explicitly set, use scope boundary to include all rows
  if (missing(scope)) {
    scope <- list(start = 1, end = nrow(df))
  }

  # check if intra_scope_window was passed as an argument, if so ...
  # ... use time ranges defined by intra_scope_window to overwrite scope
  if (!missing(intra_scope_window)) {

    # get all starting times at scope start
    starting_times <- df$RecordingTimestamp[scope$start]

    # only modify scope if input was numeric
    # check if there was an actual numeric value
    if (intra_scope_window[1] != "start") {
      # Add the starttime (intra_scope_window[1]) to every start position (scope$start) ...
      # ... of the recording timestamp to get the actual start window in milliseconds
      starting_times <- df$RecordingTimestamp[scope$start] + as.numeric(intra_scope_window[1])
    }

    if (intra_scope_window[2] != "end") {
      # Add the endtime argument to the starting times
      ending_times <- starting_times + as.numeric(intra_scope_window[2])
    }

    if (intra_scope_window[1] == "start") {
      start_indexes <- scope$start
    } else {
      # find the closest matching index for starting and ending times
      # find closest match (http://adomingues.github.io/2015/09/24/finding-closest-element-to-a-number-in-a-list/)
      start_indexes <- unlist(
        lapply(
          starting_times,
          function(x) which.min(abs(df$RecordingTimestamp - x))
        )
      )
    }

    if (intra_scope_window[2] == "end") {
      end_indexes <- scope$end
    } else {
      end_indexes <- unlist(
        lapply(
          ending_times,
          function(x) which.min(abs(df$RecordingTimestamp - x))
        )
      )
    }

    # overwrite scope
    scope <- list(start = start_indexes, end = end_indexes)
  }

  # check if lookaway_stop was provided
  if (!missing(lookaway_stop)) {
    # overwrite scope$end if lookaway criterion is fulfilled
    scope$end <- get_lookaway_scope_end(df, scope, lookaway_stop)
  }

  # destructure aoi_collection
  column_name <- aoi_collection$column_name
  # store all hit_names
  hit_names <- c()
  for (aoi in aoi_collection$aoilist) {
    hit_names <- c(hit_names, aoi$hit_name)
  }

  # create a storage container (i.e. a empty lists) for all hit_names ...
  # ... that track looking times over all trials (e.g., looking_times$left)
  looking_times <- setNames(vector("list", length(hit_names)), hit_names)

  # create a storage container for all looking frequencies (i.e., counting the number of looks within an AOI)
  looking_frequencies <- setNames(vector("list", length(hit_names)), hit_names)

  # create a storage container for gaze shifts (use it with get_looks$OriginHitname$TargetHitname)
  # https://stackoverflow.com/a/63451891/2258480
  hit_names_unknown <- c(hit_names, "unknown")
  gaze_shifts <- setNames(lapply(hit_names_unknown, function(x) setNames(rep(list(numeric()), length(hit_names_unknown) - 1L), setdiff(hit_names_unknown, x))), hit_names_unknown)


  # flag if first looks should be used (if there is only one hitname FLs aren’t  necessary)
  use_first_looks <- ifelse(length(hit_names) == 1, FALSE, TRUE)

  # storage container for first_looks
  if (use_first_looks) {
    first_looks <- c()
  }

  # log fixation indexes that contain multiple hit_names
  bad_fixation_indexes <- c()


  ######################################
  #### loop over scope / all trials ####
  ######################################
  for (seq in seq_along(scope$start)) {
    current_start <- scope$start[seq]
    current_end <- scope$end[seq]

    # get all FixationIndexes in current trial
    inter_trial_FixationIndexes <- df$FixationIndex[current_start:current_end]


    # Filter out all NAs within the current trial and check if there are still...
    # ... valid fixations left. If not, skip current trial/scope
    if (length(na.omit(inter_trial_FixationIndexes)) == 0) {
      # Append 0 to looking_times, looking_frequencies, gaze_shifts, and NA to FirstLook in the current trial and skip to next
      for (hn in hit_names) {
        looking_times[[hn]] <- c(looking_times[[hn]], 0)
      }
      for (hn in hit_names) {
        looking_frequencies[[hn]] <- c(looking_frequencies[[hn]], 0)
      }
      for (hn_origin in hit_names) {
        hn_origin_reduced <- hit_names[hit_names != hn_origin]
        for (hn_target in hn_origin_reduced) {
          gaze_shifts[[hn_origin]][[hn_target]] <- c(gaze_shifts[[hn_origin]][[hn_target]], 0)
        }
      }
      if (use_first_looks) {
        first_looks <- c(first_looks, NA)
      }
      # go to next trial
      next
    }


    # RESET/INIT STORAGE CONTAINERS PER TRIAL
    # init storage containers for all fixation indexes for hit_names in the current trial
    current_trial_total_duration <- setNames(vector("list", length(hit_names)), hit_names)
    for (hn in hit_names) {
      # set to current trial duration to 0
      current_trial_total_duration[[hn]] <- 0
    }

    # init storage containers for looking frequencies
    current_trial_total_looks <- setNames(vector("list", length(hit_names)), hit_names)
    for (hn in hit_names) {
      # set to current trial duration to 0
      current_trial_total_looks[[hn]] <- 0
    }


    # init storage containers for gaze shifts
    current_trial_gaze_shifts <- setNames(lapply(hit_names_unknown, function(x) setNames(rep(list(numeric()), length(hit_names_unknown) - 1L), setdiff(hit_names_unknown, x))), hit_names_unknown)
    for (hn_origin in hit_names_unknown) {
      hn_origin_reduced <- hit_names_unknown[hit_names_unknown != hn_origin]
      for (hn_target in hn_origin_reduced) {
        current_trial_gaze_shifts[[hn_origin]][[hn_target]] <- 0
      }
    }

    # init storage for the last_hit_name of the fixation index
    last_hit_name <- ""


    # set first look flag and state
    found_first_look <- FALSE
    first_look <- ""


    # get first and last FixationIndex (remove NAs), which define the boundaries of a single trial
    min_FixationIndex <- min(inter_trial_FixationIndexes, na.rm = TRUE)
    max_FixationIndex <- max(inter_trial_FixationIndexes, na.rm = TRUE)


    # operate WITHIN the current fixation pair (i.e., within a trial)
    for (i in min_FixationIndex:max_FixationIndex) {

      # get all hit names within current fixation index
      hit_names_in_FixationIndex <- df[[column_name]][which(df$FixationIndex == i)]

      # check if multiple hit names are in current fixation index (implicitly applies to hit_names w/ length > 1 only)
      # see reference for the check below: (https://stackoverflow.com/a/2191824/2258480)
      # This should not happen because we have a preflight check for overlapping AOIs, yet this might should stay in anyway
      if (sum(hit_names %in% hit_names_in_FixationIndex, na.rm = TRUE) > 1) {
        # log this index
        bad_fixation_indexes <- c(bad_fixation_indexes, i)
        # go to next index
        next
      }

      # ----- Looking frequency (Looks) & Gaze Shifts -----
      # Check if there is a defined hit_name within all the hit_names of the current fixation index
      if (TRUE %in% (hit_names %in% hit_names_in_FixationIndex)) {
        # get the current hit_name within the current fixation index
        hn_in_current_FI <- hit_names[hit_names %in% hit_names_in_FixationIndex]
        # Looking frequency will disregard repeated looks within the same fixation index or if last hit_name has never been set (i.e., ""), which happens when i = 1
        if ((last_hit_name != "") && (last_hit_name != hn_in_current_FI)) {
          current_trial_total_looks[[hn_in_current_FI]] <- current_trial_total_looks[[hn_in_current_FI]] + 1

          # Gaze Shifts
          # False case: Origin was not in any defined AOI, but somehwere else (i.e., last hit name = FALSE), then
          #             we set the origin to "unknown"
          if (last_hit_name == FALSE) {
            current_trial_gaze_shifts$unknown[[hn_in_current_FI]] <- current_trial_gaze_shifts$unknown[[hn_in_current_FI]] + 1
          }
          # Normal case: Origin was in a different AOI
          else {
            current_trial_gaze_shifts[[last_hit_name]][[hn_in_current_FI]] <- current_trial_gaze_shifts[[last_hit_name]][[hn_in_current_FI]] + 1
          }
        }

        # update the last_hit_name with the current one
        last_hit_name <- hn_in_current_FI

        # check for FALSE within the fixation (i.e., subject was not looking at active AOIs), and track it
      } else if (FALSE %in% hit_names_in_FixationIndex) {
        # update the last_hit_name with FALSE as there was subject were not looking at active AOIs
        last_hit_name <- FALSE
      }


      # iterate over hit names
      for (hn in hit_names) {

        # check if current hit_name (hn) is within hit_names_in_FixationIndex, if not check the next hn
        if (hn %in% hit_names_in_FixationIndex) {

          # check if the first fixation index started before the current_start and if intra_scope_cut is TRUE
          if (i == min_FixationIndex && which(df$FixationIndex == i)[1] < current_start && intra_scope_cut) {
            # get start and end milliseconds
            start_ms <- df$RecordingTimestamp[current_start]
            end_ms <- df$RecordingTimestamp[which(df$FixationIndex == i)][length(which(df$FixationIndex == i))]

            # set the difference of start_ms and end_ms to the current GazeEventDuration
            current_GazeEventDuration <- end_ms - start_ms
            # Add it to the total
            current_trial_total_duration[[hn]] <- current_trial_total_duration[[hn]] + current_GazeEventDuration

            # set first_look if flag is not set
            if (!found_first_look && use_first_looks) {
              first_look <- hn
              found_first_look <- TRUE
            }

            # go to the next fixation index
            break
          }

          # check if the last fixation index continues after the current_end_pos and if markercut is TRUE
          if (i == max_FixationIndex && which(df$FixationIndex == i)[length(which(df$FixationIndex == i))] > current_end && intra_scope_cut) {
            # get start and end milliseconds
            start_ms <- df$RecordingTimestamp[which(df$FixationIndex == i)][1]
            end_ms <- df$RecordingTimestamp[current_end]

            # set the difference of start_ms and end_ms to the current GazeEventDuration
            current_GazeEventDuration <- end_ms - start_ms
            # Add it to the total
            current_trial_total_duration[[hn]] <- current_trial_total_duration[[hn]] + current_GazeEventDuration

            # set first_look if flag is not set
            if (!found_first_look && use_first_looks) {
              first_look <- hn
              found_first_look <- TRUE
            }

            # go to the next fixation index
            break
          }

          # continue, if intra_scope_cut is FALSE or the current index is not min/max of the current fixation index
          # Grab the current GazeEventDuration chunk and select the first value
          current_GazeEventDuration <- df$GazeEventDuration[which(df$FixationIndex == i)][1]

          # Add it to the total
          current_trial_total_duration[[hn]] <- current_trial_total_duration[[hn]] + current_GazeEventDuration

          # set first_look if flag is not set
          if (!found_first_look) {
            first_look <- hn
            found_first_look <- TRUE
          }
        }
      }
    }


    # Append it to the Trial Lists
    for (hn in hit_names) {
      looking_times[[hn]] <- c(looking_times[[hn]], current_trial_total_duration[[hn]])
      looking_frequencies[[hn]] <- c(looking_frequencies[[hn]], current_trial_total_looks[[hn]])
    }
    # Append gaze shifts
    for (hn_origin in hit_names_unknown) {
      hn_origin_reduced <- hit_names_unknown[hit_names_unknown != hn_origin]
      for (hn_target in hn_origin_reduced) {
        gaze_shifts[[hn_origin]][[hn_target]] <- c(gaze_shifts[[hn_origin]][[hn_target]], current_trial_gaze_shifts[[hn_origin]][[hn_target]])
      }
    }

    # Append first look to list
    # check if first_look was there
    if (first_look == "" && use_first_looks) {
      first_look <- NA
    }
    if (use_first_looks) {
      first_looks <- c(first_looks, first_look)
    }
  }

  # End of loop over entire scope / all trials

  # Returns

  # if there is only one hit_name it does not need to be in a nested list
  if (length(hit_names) == 1) {
    looking_times <- unlist(looking_times[[1]])
  }

  if (use_first_looks) {
    return(
      list(
        looking_times = looking_times,
        first_looks = first_looks,
        bad_fixation_indexes = bad_fixation_indexes,
        looking_frequencies = looking_frequencies,
        gaze_shifts = gaze_shifts
      )
    )
  }

  return(list(looking_times = looking_times, bad_fixation_indexes = bad_fixation_indexes))
}
