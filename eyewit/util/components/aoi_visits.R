# 📚 Tobii Pro Lab User Manual v 1.181
# AOI Visit metrics
# An AOI visit corresponds to all the data between the start of the first fixation inside and AOI to
# the end of the last fixation in the same AOI. From the first fixation inside the AOI until the
# last fixation inside the AOI, all data is considered as part of the AOI visit (even saccades,
# blinks or invalid gaze data).

aoi_visits <- function(df,
  aoi_collection,
  trial_ranges = list(start = 1, end = nrow(df)), # defaults to include all rows of df
  trial_offset = c("start", "end"), # defaults to the regular VideoStimulusStart/End range
  omit_first_overflow_fi = FALSE) {
  browser()

  # check if trial_offset was passed as an argument,
  if (!missing(trial_offset) && !identical(trial_offset, c("start", "end"))) {
    trial_ranges <- get_trial_offset(df, trial_ranges, trial_offset)
  }

  omit_first_overflow_fi_applied <- FALSE
  if (omit_first_overflow_fi) {
    # store former trial_ranges$start
    former_trial_ranges_start <- trial_ranges$start
    # overwrite trial_ranges$start
    trial_ranges$start <- get_first_free_fi(df, trial_ranges)
    # create logical vector to track which trial_ranges$start was modified
    omit_first_overflow_fi_applied <- !(former_trial_ranges_start == trial_ranges$start)
  }

  # in some cases the combination of a small trial_offset, a given omit_first_overflow_fi and
  # ... lookaway_stop can set the trial_ranges$start behind the scpe$end
  # You can try to avoid this by not setting the omit_first_overflow_fi or increase the
  # ...trial_offset.
  # By default the script assign the smaller trial_ranges$end to trial_ranges$start, so the function will treat it
  # ... in a way as if there is no data (which is the case)
  if (any(trial_ranges$end - trial_ranges$start < 0)) {
    warning("Skipped Evaluation because scope$start was greater than scope$end")
    for (bs in which(trial_ranges$end < trial_ranges$start)) {
      trial_ranges$start[bs] <- trial_ranges$end[bs]
    }
  }

  # STORAGE CONTAINERS
  source("util/containers.R", local = TRUE)

  ##################################################################
  ################# LOOP OVER ALL TRIAL RANGES (TR) ################
  ##################################################################
  for (itr in seq_along(trial_ranges)) {
    current_tr_start <- trial_ranges$start[itr]
    current_tr_end <- trial_ranges$end[itr]

    # get all FixationIndexes (fis) in current trial range (ctrfi)
    current_tr_fis <- df$FixationIndex[current_tr_start:current_tr_end]

    # check if current tr has no fi, if so append an empty filler (NA, 0, etc) and go to next
    source("util/append_empty_fillers_for_trial_range.R", local = TRUE)

    # Clear storage containers for every trial
    source("util/clear_storage_containers.R", local = TRUE)


    ##################################################################
    ##### LOOP OVER FIXATION INDEXES WITHIN A SINGLE TRIAL RANGE #####
    ##################################################################
    # operate WITHIN the current fixation pair (i.e., within a trial)
    for (fi in min(current_tr_fis, na.rm = TRUE):max(current_tr_fis, na.rm = TRUE)) {

      # get all hit names within current fixation index
      hit_names_in_FixationIndex <- df[[column_name]][which(df$FixationIndex == i)]

      source("util/check_multiple_hit_names_in_same_fi.R")

      source("util/looking_freq_gaze_shifts.R")


      ############################################################################
      ##### ITERATE OVER ALL HITNAMES WITHIN CURRENT FI WITHIN CURRENT RANGE #####
      ############################################################################
      for (hn in hit_names) {

        # check if current hit_name (hn) is within hit_names_in_FixationIndex, if not check the next hn
        if (hn %in% hit_names_in_FixationIndex) {
          source("util/main_loop.R")
        }
      }
    }

    source("util/data_handover.R")
  }

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
      gaze_shifts = gaze_shifts,
      omit_first_overflow_fi_applied = omit_first_overflow_fi_applied,
      lookaway_stop_applied = lookaway_stop_applied,
      processed_scope = scope
    )
  )

  # return(
  #   list(
  #     # TODO PART
  #     # total_duration_of_visit = ...,
  #     # average_duration_of_visit = ...,
  #     # minimum_duration_of_visit = ...,
  #     # maximum_duration_of_visit = ...,
  #     # number_of_visits = number_of_visits, <- Looking frequency?
  #     # time_to_first_visit = ...,
  #     duration_of_first_visit = duration_of_first_visit
  #   )
  # )
}
