# check if multiple hit names are in current fixation index (implicitly applies to hit_names w/ length > 1 only)
# see reference for the check below: (https://stackoverflow.com/a/2191824/2258480)
# This should not happen because we have a preflight check for overlapping AOIs, yet this might should stay in anyway

# FYI: outside label can share the the same fi with a hitname
# do not try to prevent that!
# see: https://github.com/Kalaschnik/media/possible-overlapping-aoi-with-outside-label.png

if (sum(hit_names %in% hit_names_in_FixationIndex, na.rm = TRUE) > 1) {

  # log this index
  bad_fixation_indexes <- c(bad_fixation_indexes, i)
  # stop execution, as this should be impossible (but it is, it is a tobii thing)
  # stop()
  # go to next index
  warning(str_interp("At Fixation index: ${i}, are multiple hit names (aois) within the same Fixation (see bad_fixation_indexes"))
  next
}

