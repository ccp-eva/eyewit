# Filter out all NAs within the current trial and check if there are still...
# ... valid fixations left. If not, skip current trial/scope and append NA, 0 to containers
if (length(na.omit(current_tr_fis)) == 0) {
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

  # go to next trial
  next
}
