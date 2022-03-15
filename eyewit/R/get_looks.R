#' Title
#'
#' @param df df
#' @param aoi_collection aoi_collection
#' @param scope scope
#' @param intra_scope_window intra_scope_window
#' @param lookaway_stop lookaway_stop
#' @param omit_first_overflow_fi omit_first_overflow_fi
#' @param first_look_emergency_cutoff first_look_emergency_cutoff
#' @param intra_scope_cut intra_scope_cut
#'
#' @return None
#' @export
#'
get_looks <- function(df,
                      aoi_collection,
                      scope = NA,
                      intra_scope_window = c("start", "end"),
                      lookaway_stop = NA,
                      omit_first_overflow_fi = FALSE,
                      first_look_emergency_cutoff = NA,
                      intra_scope_cut = TRUE) {

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
      ending_times <- df$RecordingTimestamp[scope$start] + as.numeric(intra_scope_window[2])
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

  omit_first_overflow_fi_applied <- FALSE
  if (omit_first_overflow_fi) {
    # store former scope$start
    former_scope_start <- scope$start
    # overwrite scope$start
    scope$start <- get_first_free_fi(df, scope)
    # create logical vector to track which scope$start was modified
    omit_first_overflow_fi_applied <- !(former_scope_start == scope$start)
  }

  # in some cases the combination of a small intra_scope_window, a given omit_first_overflow_fi and
  # ... lookaway_stop can set the scope$start behind the scpe$end
  # You can try to avoid this by not setting the omit_first_overflow_fi or increase the
  # ...intra_scope_windows.
  # By default the script assign the smaller scope$end to scope$start, so the function will treat it
  # ... in a way as if there is no data (which is the case)
  if (any(scope$end - scope$start < 0)) {
    warning("Skipped Evaluation because scope$start was greater than scope$end")
    for (bs in which(scope$end < scope$start)) {
      scope$start[bs] <- scope$end[bs]
    }
  }



  # destructure aoi_collection
  column_name <- aoi_collection$column_name
  # store all hit_names
  hit_names <- c()
  for (aoi in aoi_collection$aoilist) {
    hit_names <- c(hit_names, aoi$hit_name)
  }

  outside_aoi_label <- aoi_collection$outside_aoi_label


  # create a storage container (i.e. a empty lists) for all hit_names ...
  # ... that track looking times over all trials (e.g., looking_times$left)
  looking_times <- stats::setNames(vector("list", length(hit_names)), hit_names)

  # create a storage container for all looking frequencies (i.e., counting the number of looks within an AOI)
  looking_frequencies <- stats::setNames(vector("list", length(hit_names)), hit_names)

  # create a storage container for gaze shifts (use it with get_looks$OriginHitname$TargetHitname)
  # https://stackoverflow.com/a/63451891/2258480
  hit_names_unknown <- c(hit_names, "unknown")
  gaze_shifts <- stats::setNames(lapply(hit_names_unknown, function(x) stats::setNames(rep(list(numeric()), length(hit_names_unknown) - 1L), setdiff(hit_names_unknown, x))), hit_names_unknown)

  # storage container for first_looks
  first_looks <- c()

  first_looks_collection <- list()
  # define structure
  for (hn in hit_names) {
    first_looks_collection[[hn]]$durations <- c()
    # this can either be "lookaway" (because an "outside" aoi was detected or the consecutive fixation was in another aoi), or "time criterion")
    first_looks_collection[[hn]]$ending_reason <- c()
    first_looks_collection[[hn]]$found_first <- FALSE
    first_looks_collection[[hn]]$forced_stop <- FALSE
    first_looks_collection[[hn]]$init_fi <- NA
    first_looks_collection[[hn]]$last_fi <- NA
    first_looks_collection[[hn]]$first_look_initial_timestamp <- NA
  }

  lookaway_collection <- list()
  # define structure
  for (hn in hit_names) {
    lookaway_collection[[hn]]$found_first_hn <- FALSE
    lookaway_collection[[hn]]$lookaway_stop_applied <- c()
    lookaway_collection[[hn]]$previous_end_rts <- NA
    lookaway_collection[[hn]]$current_start_rts <- NA
    lookaway_collection[[hn]]$durations <- c()
    lookaway_collection[[hn]]$init_fi <- NA
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
    if (length(stats::na.omit(inter_trial_FixationIndexes)) == 0) {
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

      first_looks <- c(first_looks, NA)

      for (hn in hit_names) {
        first_looks_collection[[hn]]$durations <- c(first_looks_collection[[hn]]$durations, NA)
        first_looks_collection[[hn]]$ending_reason <- c(first_looks_collection[[hn]]$ending_reason, NA)
      }

      for (hn in hit_names) {
        lookaway_collection[[hn]]$durations <- c(lookaway_collection[[hn]]$durations, NA)
        lookaway_collection[[hn]]$lookaway_stop_applied <- c(lookaway_collection[[hn]]$lookaway_stop_applied, NA)
      }



      # go to next trial
      next
    }


    # RESET/INIT STORAGE CONTAINERS PER TRIAL
    # init storage containers for all fixation indexes for hit_names in the current trial
    current_trial_total_duration <- stats::setNames(vector("list", length(hit_names)), hit_names)
    for (hn in hit_names) {
      # set to current trial duration to 0
      current_trial_total_duration[[hn]] <- 0
    }

    # init storage containers for looking frequencies
    current_trial_total_looks <- stats::setNames(vector("list", length(hit_names)), hit_names)
    for (hn in hit_names) {
      # set to current trial duration to 0
      current_trial_total_looks[[hn]] <- 0
    }


    # init storage containers for gaze shifts
    current_trial_gaze_shifts <- stats::setNames(lapply(hit_names_unknown, function(x) stats::setNames(rep(list(numeric()), length(hit_names_unknown) - 1L), setdiff(hit_names_unknown, x))), hit_names_unknown)
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

    # set first_look durations for current trial
    current_first_look_duration <- stats::setNames(vector("list", length(hit_names)), hit_names)
    current_first_look_ending_reason <- stats::setNames(vector("list", length(hit_names)), hit_names)
    for (hn in hit_names) {
      # set to current trial duration to 0
      current_first_look_duration[[hn]] <- 0
      current_first_look_ending_reason[[hn]] <- ""
      first_looks_collection[[hn]]$found_first <- FALSE
      first_looks_collection[[hn]]$forced_stop <- FALSE
    }


    # set first_look durations for current trial
    current_lookaway_duration <- stats::setNames(vector("list", length(hit_names)), hit_names)
    current_lookaway_stop_applied <- stats::setNames(vector("list", length(hit_names)), hit_names)
    for (hn in hit_names) {
      current_lookaway_duration[[hn]] <- 0
      lookaway_collection[[hn]]$found_first_hn <- FALSE
      current_lookaway_stop_applied[[hn]] <- FALSE
    }


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
        # stop execution, as this should be impossible (but it is, it is a tobii thing)
        # stop()
        # go to next index
        warning(stringr::str_interp("At Fixation index: ${i}, are multiple hit names (aois) within the same Fixation (see bad_fixation_indexes"))
        next
      }

      # FYI: outside label can share the the same fi with a hitname
      # do not try to prevent that!
      # see: https://raw.githubusercontent.com/kalaschnik/media/main/possible-overlapping-aoi-with-outside-label.png

      # ----- Looking frequency (Looks), Gaze Shifts, Last Hit Name -----
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
          if (last_hit_name == outside_aoi_label) {
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
      } else if (outside_aoi_label %in% hit_names_in_FixationIndex) {
        # update the last_hit_name with FALSE as there was subject were not looking at active AOIs
        last_hit_name <- outside_aoi_label
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
            if (!found_first_look) {
              first_look <- hn
              found_first_look <- TRUE
              first_look_initial_timestamp <- df$RecordingTimestamp[fi_pairs$fistart[i]]
            }

            # first_look_collection part
            # enter found first only once it detects the hn
            if (!first_looks_collection[[hn]]$found_first) {
              first_looks_collection[[hn]]$found_first <- TRUE
              first_looks_collection[[hn]]$init_fi <- i
              first_looks_collection[[hn]]$last_fi <- i
              first_looks_collection[[hn]]$first_look_initial_timestamp <- df$RecordingTimestamp[fi_pairs$fistart[i]]
            }

            if (
              first_looks_collection[[hn]]$found_first && !first_looks_collection[[hn]]$forced_stop &&
                (
                  # check if initial i
                  first_looks_collection[[hn]]$init_fi == i ||
                    # or check if consecutive fixation
                    first_looks_collection[[hn]]$last_fi == i - 1
                )
            ) {
              current_first_look_duration[[hn]] <- current_first_look_duration[[hn]] + current_GazeEventDuration
              first_looks_collection[[hn]]$last_fi <- i

              # check if outside label is between this fi and the next fi
              if (i < max(df$FixationIndex, na.rm = TRUE) && is_hitname_in_range(df[[column_name]], outside_aoi_label, i, i + 1)) {
                # outside fixation or saccade after last fi
                current_first_look_ending_reason[[hn]] <- "outside"
                first_looks_collection[[hn]]$forced_stop <- TRUE
                # overwrite current_first_look_duration[[hn]] to include saccades and error data and not only fixations
                current_first_look_duration[[hn]] <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1] - first_looks_collection[[hn]]$first_look_initial_timestamp
              }

              # check if the time criterion is met within this fi and the next fi#
              if (i < max(df$FixationIndex, na.rm = TRUE) && !first_looks_collection[[hn]]$forced_stop && !missing(first_look_emergency_cutoff) &&
                (
                  df$RecordingTimestamp[fi_pairs$fistart[i + 1]] -
                    df$RecordingTimestamp[fi_pairs$fiend[i] + 1]) >= first_look_emergency_cutoff
              ) {
                current_first_look_ending_reason[[hn]] <- "timecriterion"
                first_looks_collection[[hn]]$forced_stop <- TRUE
                # overwrite current_first_look_duration[[hn]] to include saccades and error data and not only fixations
                current_first_look_duration[[hn]] <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1] - first_looks_collection[[hn]]$first_look_initial_timestamp
              }
            }






            ##################### LOOKAWAY
            if (!missing(lookaway_stop) && !current_lookaway_stop_applied[[hn]]) {
              if (!lookaway_collection[[hn]]$found_first_hn) {
                lookaway_collection[[hn]]$current_start_rts <- df$RecordingTimestamp[fi_pairs$fistart[i]]
                lookaway_collection[[hn]]$previous_end_rts <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1]


                current_lookaway_duration[[hn]] <- current_lookaway_duration[[hn]] + current_GazeEventDuration

                lookaway_collection[[hn]]$found_first_hn <- TRUE
                lookaway_collection[[hn]]$init_fi <- i
              }

              if (i != lookaway_collection[[hn]]$init_fi) {
                lookaway_collection[[hn]]$current_start_rts <- df$RecordingTimestamp[fi_pairs$fistart[i]]

                if (lookaway_collection[[hn]]$current_start_rts - lookaway_collection[[hn]]$previous_end_rts < lookaway_stop) {
                  current_lookaway_duration[[hn]] <- current_lookaway_duration[[hn]] + current_GazeEventDuration
                  lookaway_collection[[hn]]$previous_end_rts <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1]
                } else {
                  current_lookaway_stop_applied[[hn]] <- TRUE
                }
              }
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
            if (!found_first_look) {
              first_look <- hn
              found_first_look <- TRUE
              first_look_initial_timestamp <- df$RecordingTimestamp[fi_pairs$fistart[i]]
            }

            # first_look_collection part
            # enter found first only once it detects the hn
            if (!first_looks_collection[[hn]]$found_first) {
              first_looks_collection[[hn]]$found_first <- TRUE
              first_looks_collection[[hn]]$init_fi <- i
              first_looks_collection[[hn]]$last_fi <- i
              first_looks_collection[[hn]]$first_look_initial_timestamp <- df$RecordingTimestamp[fi_pairs$fistart[i]]
            }

            if (
              first_looks_collection[[hn]]$found_first && !first_looks_collection[[hn]]$forced_stop &&
                (
                  # check if initial i
                  first_looks_collection[[hn]]$init_fi == i ||
                    # or check if consecutive fixation
                    first_looks_collection[[hn]]$last_fi == i - 1
                )
            ) {
              current_first_look_duration[[hn]] <- current_first_look_duration[[hn]] + current_GazeEventDuration
              first_looks_collection[[hn]]$last_fi <- i

              # check if outside label is between this fi and the next fi
              if (i < max(df$FixationIndex, na.rm = TRUE) && is_hitname_in_range(df[[column_name]], outside_aoi_label, i, i + 1)) {
                # outside fixation or saccade after last fi
                current_first_look_ending_reason[[hn]] <- "outside"
                first_looks_collection[[hn]]$forced_stop <- TRUE
                # overwrite current_first_look_duration[[hn]] to include saccades and error data and not only fixations
                current_first_look_duration[[hn]] <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1] - first_looks_collection[[hn]]$first_look_initial_timestamp
              }

              # check if the time criterion is met within this fi and the next fi#
              if (i < max(df$FixationIndex, na.rm = TRUE) && !first_looks_collection[[hn]]$forced_stop && !missing(first_look_emergency_cutoff) &&
                (
                  df$RecordingTimestamp[fi_pairs$fistart[i + 1]] -
                    df$RecordingTimestamp[fi_pairs$fiend[i] + 1]) >= first_look_emergency_cutoff
              ) {
                current_first_look_ending_reason[[hn]] <- "timecriterion"
                first_looks_collection[[hn]]$forced_stop <- TRUE
                # overwrite current_first_look_duration[[hn]] to include saccades and error data and not only fixations
                current_first_look_duration[[hn]] <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1] - first_looks_collection[[hn]]$first_look_initial_timestamp
              }
            }





            ##################### LOOKAWAY
            if (!missing(lookaway_stop) && !current_lookaway_stop_applied[[hn]]) {
              if (!lookaway_collection[[hn]]$found_first_hn) {
                lookaway_collection[[hn]]$current_start_rts <- df$RecordingTimestamp[fi_pairs$fistart[i]]
                lookaway_collection[[hn]]$previous_end_rts <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1]


                current_lookaway_duration[[hn]] <- current_lookaway_duration[[hn]] + current_GazeEventDuration

                lookaway_collection[[hn]]$found_first_hn <- TRUE
                lookaway_collection[[hn]]$init_fi <- i
              }

              if (i != lookaway_collection[[hn]]$init_fi) {
                lookaway_collection[[hn]]$current_start_rts <- df$RecordingTimestamp[fi_pairs$fistart[i]]

                if (lookaway_collection[[hn]]$current_start_rts - lookaway_collection[[hn]]$previous_end_rts < lookaway_stop) {
                  current_lookaway_duration[[hn]] <- current_lookaway_duration[[hn]] + current_GazeEventDuration
                  lookaway_collection[[hn]]$previous_end_rts <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1]
                } else {
                  current_lookaway_stop_applied[[hn]] <- TRUE
                }
              }
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
            first_look_initial_timestamp <- df$RecordingTimestamp[fi_pairs$fistart[i]]
          }


          # first_look_collection part
          # enter found first only once it detects the hn
          if (!first_looks_collection[[hn]]$found_first) {
            first_looks_collection[[hn]]$found_first <- TRUE
            first_looks_collection[[hn]]$init_fi <- i
            first_looks_collection[[hn]]$last_fi <- i
            first_looks_collection[[hn]]$first_look_initial_timestamp <- df$RecordingTimestamp[fi_pairs$fistart[i]]
          }

          if (
            first_looks_collection[[hn]]$found_first && !first_looks_collection[[hn]]$forced_stop &&
              (
                # check if initial i
                first_looks_collection[[hn]]$init_fi == i ||
                  # or check if consecutive fixation
                  first_looks_collection[[hn]]$last_fi == i - 1
              )
          ) {
            current_first_look_duration[[hn]] <- current_first_look_duration[[hn]] + current_GazeEventDuration
            first_looks_collection[[hn]]$last_fi <- i

            # check if outside label is between this fi and the next fi
            if (i < max(df$FixationIndex, na.rm = TRUE) && is_hitname_in_range(df[[column_name]], outside_aoi_label, i, i + 1)) {
              # outside fixation or saccade after last fi
              current_first_look_ending_reason[[hn]] <- "outside"
              first_looks_collection[[hn]]$forced_stop <- TRUE
              # overwrite current_first_look_duration[[hn]] to include saccades and error data and not only fixations
              current_first_look_duration[[hn]] <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1] - first_looks_collection[[hn]]$first_look_initial_timestamp
            }

            # check if the time criterion is met within this fi and the next fi#
            if (i < max(df$FixationIndex, na.rm = TRUE) && !first_looks_collection[[hn]]$forced_stop && !missing(first_look_emergency_cutoff) &&
              (
                df$RecordingTimestamp[fi_pairs$fistart[i + 1]] -
                  df$RecordingTimestamp[fi_pairs$fiend[i] + 1]) >= first_look_emergency_cutoff
            ) {
              current_first_look_ending_reason[[hn]] <- "timecriterion"
              first_looks_collection[[hn]]$forced_stop <- TRUE
              # overwrite current_first_look_duration[[hn]] to include saccades and error data and not only fixations
              current_first_look_duration[[hn]] <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1] - first_looks_collection[[hn]]$first_look_initial_timestamp
            }
          }








          ##################### LOOKAWAY
          if (!missing(lookaway_stop) && !current_lookaway_stop_applied[[hn]]) {
            if (!lookaway_collection[[hn]]$found_first_hn) {
              lookaway_collection[[hn]]$current_start_rts <- df$RecordingTimestamp[fi_pairs$fistart[i]]
              lookaway_collection[[hn]]$previous_end_rts <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1]


              current_lookaway_duration[[hn]] <- current_lookaway_duration[[hn]] + current_GazeEventDuration

              lookaway_collection[[hn]]$found_first_hn <- TRUE
              lookaway_collection[[hn]]$init_fi <- i
            }

            if (i != lookaway_collection[[hn]]$init_fi) {
              lookaway_collection[[hn]]$current_start_rts <- df$RecordingTimestamp[fi_pairs$fistart[i]]

              if (lookaway_collection[[hn]]$current_start_rts - lookaway_collection[[hn]]$previous_end_rts < lookaway_stop) {
                current_lookaway_duration[[hn]] <- current_lookaway_duration[[hn]] + current_GazeEventDuration
                lookaway_collection[[hn]]$previous_end_rts <- df$RecordingTimestamp[fi_pairs$fiend[i] + 1]
              } else {
                current_lookaway_stop_applied[[hn]] <- TRUE
              }
            }
          }
        } # end of if (hn %in% hit_names_in_FixationIndex)
      } # end of for (hn in hit_names)
    } # end of for (i in min_FixationIndex:max_FixationIndex)


    # Append it to the Trial Lists
    for (hn in hit_names) {
      looking_times[[hn]] <- c(looking_times[[hn]], current_trial_total_duration[[hn]])
      looking_frequencies[[hn]] <- c(looking_frequencies[[hn]], current_trial_total_looks[[hn]])
      first_looks_collection[[hn]]$durations <- c(first_looks_collection[[hn]]$durations, current_first_look_duration[[hn]])
      first_looks_collection[[hn]]$ending_reason <- c(first_looks_collection[[hn]]$ending_reason, current_first_look_ending_reason[[hn]])

      lookaway_collection[[hn]]$durations <- c(lookaway_collection[[hn]]$durations, current_lookaway_duration[[hn]])
      lookaway_collection[[hn]]$lookaway_stop_applied <- c(lookaway_collection[[hn]]$lookaway_stop_applied, current_lookaway_stop_applied[[hn]])
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
    if (first_look == "") {
      first_look <- NA
    }

    first_looks <- c(first_looks, first_look)
  }

  # End of loop over entire scope / all trials

  # Returns

  # if there is only one hit_name it does not need to be in a nested list
  if (length(hit_names) == 1) {
    looking_times <- unlist(looking_times[[1]])
  }

  return(
    list(
      looking_times = looking_times,
      first_looks = first_looks,
      first_looks_collection = first_looks_collection,
      bad_fixation_indexes = bad_fixation_indexes,
      looking_frequencies = looking_frequencies,
      lookaway_collection = lookaway_collection,
      gaze_shifts = gaze_shifts,
      omit_first_overflow_fi_applied = omit_first_overflow_fi_applied,
      processed_scope = scope
    )
  )
}
