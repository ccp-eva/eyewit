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
