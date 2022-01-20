# Append it to the Trial Lists
for (hn in hit_names) {
  looking_times[[hn]] <- c(looking_times[[hn]], current_trial_total_duration[[hn]])
  looking_frequencies[[hn]] <- c(looking_frequencies[[hn]], current_trial_total_looks[[hn]])
  first_looks_collection[[hn]]$durations <- c(first_looks_collection[[hn]]$durations, current_first_look_duration[[hn]])
  first_looks_collection[[hn]]$ending_reason <- c(first_looks_collection[[hn]]$ending_reason, current_first_look_ending_reason[[hn]])
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
