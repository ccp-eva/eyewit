get_first_free_fi <- function(df, scope) {

  # init return vector
  scope_start <- scope$start

  # get fi pairs for help
  fi_pairs <- get_fixationindex_pairs(df$FixationIndex)

  # check if current scope$start is a "Fixation"
  # we can use the fact, that every valid "Fixation" gets an numeric index
  # non-fixation will have NA
  scope_start_fis <- df$FixationIndex[scope$start]

  # iterative over valid Fixation indexes (thus skipping NAs)
  for (i in which(!!scope_start_fis)) {

    # check if it possible to go to the next fixation index (if it was already the last one you would get NA)
    if (scope_start_fis[i] == max(df$FixationIndex, na.rm = TRUE)) {
      warning(paste0(
        "Fixation Index: ", scope_start_fis[i], " (", fi_pairs$fistart[scope_start_fis[i]], ":",
        fi_pairs$fiend[scope_start_fis[i]], ") is the last fixation index in this data. ",
        "No skipping will be applied. Instead this fixation index will be used."
      ))
      next
    }

    # get subsequent fixation index (+1) and retrieve row number using fi_pars and assign
    scope_start[i] <- fi_pairs$fistart[scope_start_fis[i] + 1]
  }

  return(scope_start)
}



