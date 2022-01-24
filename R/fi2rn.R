#' Map FixationIndexes to RowNumbers (fi2rn)
#'
#' Given a vector containing fixation indexes, this function returns a named list containing
#' corresponding row names to the given fixation indexes. Since fixation indexes span a range
#' for each index, the function uses the two groups: `fistart` and `fiend` to separate them.
#'
#'
#'
#'
#' Since FixationIndexes span a range fixation_indexes_to_row_numbers fi2rn
#' `fistart` and `fiend`.
#' $fistart is the first and $fiend the last value for a given single FixationIndex
#'
#'
#'
#' @param fi_col A column vector containing FixationIndexes
#'
#' @return A named list which contains rownumbers for a given FixationIndex
#' @export
#'
#' @examples
#' # Todo fi2rn(df$FixationIndex)$fistart[110] returns a rn of 6835

# todo check bounces of FI, if FI it out of bounce show warning and return NULL
# todo make a rn2fi function
# add fi2rn as hidden variable in preflight
# or rather in a helper list -> util$fi2rn, util$gazeshifts, etc.
fi2rn <- function(fi_col) {

  # check if fixation indexes are an incremental list from 1:n
  stopifnot(
    identical(
      as.numeric(min(fi_col, na.rm = TRUE):max(fi_col, na.rm = TRUE)),
      unique(stats::na.omit(fi_col))
    ) & as.numeric(min(fi_col,na.rm = TRUE)) == 1
  )

  fixationindex_start <- c()
  fixationindex_end <- c()

  # iterate over all fixationindexes
  for (fi in unique(stats::na.omit(fi_col))) {
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



