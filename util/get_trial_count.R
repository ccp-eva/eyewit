get_trial_count <- function(se_lists) {

  if (
      length(merge_startend_chunks(se_lists, "start")) !=
      length(merge_startend_chunks(se_lists, "end"))
  ) {
    stop("Subject has a mismatch in start/end length")
  }

  if (length(se_lists) > 1) {
    return(
      length(merge_startend_chunks(se_lists, "start")) / (length(se_lists) / 2)
    )
  }

  return(length(merge_startend_chunks(se_lists, "start")))
}
