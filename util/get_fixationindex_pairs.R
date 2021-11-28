# returns a named list which contains rownumbers for a given Fixation index
# ... $fistart is the first and $fiend the last value for a given single FixationIndex
# ... Example: get_fixationindex_pairs(df$FixationIndex)$fistart[110] returns a rn of 6835
get_fixationindex_pairs <- function(fi_col) {

  # check if fixation indexes are an incremental list from 1:n
  stopifnot(
    identical(
      as.numeric(min(fi_col, na.rm = TRUE):max(fi_col, na.rm = TRUE)),
      unique(na.omit(fi_col))
    ) & as.numeric(min(fi_col,na.rm = TRUE)) == 1
  )

  fixationindex_start <- c()
  fixationindex_end <- c()

  # iterate over all fixationindexes
  for (fi in unique(na.omit(fi_col))) {
    fixationindex_start <- c(fixationindex_start, min(which(fi_col == fi)))
    fixationindex_end <- c(fixationindex_end, max(which(fi_col == fi)))
  }

  # guarantee same length
  stopifnot(length(fixationindex_start) == length(fixationindex_end))

  return(
    list(
      fistart = fixationindex_start,
      fiend = fixationindex_end
    )
  )

}



