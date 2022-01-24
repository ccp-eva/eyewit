#' Is hitname in range
#'
#' this function accepts a column containing hitnames, a target hitname to look for, and a
#' starting and ending fixation index
#'
#' @param vec vec
#' @param hitname hitname
#' @param fi_start fi_start
#' @param fi_end fi_end
#'
#' @return None
#' @export
#'
is_hitname_in_range <- function(vec, hitname, fi_start, fi_end) {

  if (!"fi_pairs" %in% ls(envir = .GlobalEnv)) {
    fi_pairs <- fi2rn(df$FixationIndex)
    # save to global env for performance
    list2env(fi_pairs, envir = .GlobalEnv)
  }

  # shift fixation, so they do not sit the fixation but rather +1 after the fixation and -1 likewise
  start_index <- fi_pairs$fiend[fi_start] + 1
  end_index <- fi_pairs$fistart[fi_end] - 1

  return(hitname %in% vec[start_index:end_index])
}
