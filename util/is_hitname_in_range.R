# this function accepts a column containing hitnames, a target hitname to look for, and a
# ... starting and ending fixation index
is_hitname_in_range <- function(vec, hitname, fi_start, fi_end) {

  if (!"fi_pairs" %in% ls(envir = .GlobalEnv)) {
    fi_pairs <- get_fixationindex_pairs(df$FixationIndex)
    # save to global env for performance
    list2env(fi_pairs, envir = .GlobalEnv)
  }

  # shift fixation, so they do not sit the fixation but rather +1 after the fixation and -1 likewise
  start_index <- fi_pairs$fiend[fi_start] + 1
  end_index <- fi_pairs$fistart[fi_end] - 1

  return(hitname %in% vec[start_index:end_index])
}
