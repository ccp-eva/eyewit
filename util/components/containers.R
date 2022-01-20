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
looking_times <- setNames(vector("list", length(hit_names)), hit_names)

# create a storage container for all looking frequencies (i.e., counting the number of looks within an AOI)
looking_frequencies <- setNames(vector("list", length(hit_names)), hit_names)

# create a storage container for gaze shifts (use it with get_looks$OriginHitname$TargetHitname)
# https://stackoverflow.com/a/63451891/2258480
hit_names_unknown <- c(hit_names, "unknown")
gaze_shifts <- setNames(lapply(hit_names_unknown, function(x) setNames(rep(list(numeric()), length(hit_names_unknown) - 1L), setdiff(hit_names_unknown, x))), hit_names_unknown)

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
}

# log fixation indexes that contain multiple hit_names
bad_fixation_indexes <- c()
