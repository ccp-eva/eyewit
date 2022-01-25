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
  }

  # first_look_collection part
  # enter found first only once it detects the hn
  if (!first_looks_collection[[hn]]$found_first) {
    first_looks_collection[[hn]]$found_first <- TRUE
    first_looks_collection[[hn]]$init_fi <- i
    first_looks_collection[[hn]]$last_fi <- i
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
    }

    # check if the time criterion is met within this fi and the next fi#
    if (i < max(df$FixationIndex, na.rm = TRUE) && !first_looks_collection[[hn]]$forced_stop && !missing(first_look_emergency_cutoff) &&
        (
          df$RecordingTimestamp[fi_pairs$fistart[i + 1]] -
          df$RecordingTimestamp[fi_pairs$fiend[i] + 1]) >= first_look_emergency_cutoff
    ) {
      current_first_look_ending_reason[[hn]] <- "timecriterion"
      first_looks_collection[[hn]]$forced_stop <- TRUE
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
  }

  # first_look_collection part
  # enter found first only once it detects the hn
  if (!first_looks_collection[[hn]]$found_first) {
    first_looks_collection[[hn]]$found_first <- TRUE
    first_looks_collection[[hn]]$init_fi <- i
    first_looks_collection[[hn]]$last_fi <- i
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
    }

    # check if the time criterion is met within this fi and the next fi#
    if (i < max(df$FixationIndex, na.rm = TRUE) && !first_looks_collection[[hn]]$forced_stop && !missing(first_look_emergency_cutoff) &&
        (
          df$RecordingTimestamp[fi_pairs$fistart[i + 1]] -
          df$RecordingTimestamp[fi_pairs$fiend[i] + 1]) >= first_look_emergency_cutoff
    ) {
      current_first_look_ending_reason[[hn]] <- "timecriterion"
      first_looks_collection[[hn]]$forced_stop <- TRUE
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
}


# first_look_collection part
# enter found first only once it detects the hn
if (!first_looks_collection[[hn]]$found_first) {
  first_looks_collection[[hn]]$found_first <- TRUE
  first_looks_collection[[hn]]$init_fi <- i
  first_looks_collection[[hn]]$last_fi <- i
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
  }

  # check if the time criterion is met within this fi and the next fi#
  if (i < max(df$FixationIndex, na.rm = TRUE) && !first_looks_collection[[hn]]$forced_stop && !missing(first_look_emergency_cutoff) &&
      (
        df$RecordingTimestamp[fi_pairs$fistart[i + 1]] -
        df$RecordingTimestamp[fi_pairs$fiend[i] + 1]) >= first_look_emergency_cutoff
  ) {
    current_first_look_ending_reason[[hn]] <- "timecriterion"
    first_looks_collection[[hn]]$forced_stop <- TRUE
  }
}
