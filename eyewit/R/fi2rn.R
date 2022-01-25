#' Map FixationIndexes to RowNumbers (fi2rn)
#'
#' Given a vector containing fixation indexes, this function returns a named list containing
#' corresponding row names to the given fixation indexes. Since fixation indexes span a range
#' for each index, the function uses the two groups: `fistart` and `fiend` to separate them.
#' That means, `$fistart` denotes all row names where a fixation index is starting, wheras `$fiend`
#' denotes all row names where the fixation index ends.
#' Since the function is used in several places within the package, it gets
#' **executed during the preflight automatically**. That means you don’t need to call this
#' function on your own. To access the content, you can call the **variable**:
#' `.eyewit_util$fi2rn` (see examples).
#'
#' @family preflight functions
#'
#' @param fi_col A column vector containing fixation indexes
#'
#' @return A named list
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume you have fixation indexes in a df with a column named FixationIndex
#'
#' # returns all $fistart and $fiend row names and store it (this is what happens in preflight)
#' fi2rn <- fi2rn(df$FixationIndex)
#'
#' # now call the variable because the function call is expensive
#'
#' # get the first row number of the first fixation index
#' fi2rn(df$FixationIndex)$fistart[1]
#'
#' # get the last row number of the first fixation index
#' fi2rn(df$FixationIndex)$fiend[1]
#'
#' # get the last row number of the 42. fixation index
#' fi2rn(df$FixationIndex)$fiend[42]
#' }
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


