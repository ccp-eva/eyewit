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

# set first_look durations for current trial
current_first_look_duration <- setNames(vector("list", length(hit_names)), hit_names)
current_first_look_ending_reason <- setNames(vector("list", length(hit_names)), hit_names)
for (hn in hit_names) {
  # set to current trial duration to 0
  current_first_look_duration[[hn]] <- 0
  current_first_look_ending_reason[[hn]] <- ""
  first_looks_collection[[hn]]$found_first <- FALSE
  first_looks_collection[[hn]]$forced_stop <- FALSE
}
