#' Title
#'
#' @param se_lists se_lists
#' @param shared_key shared_key
#'
#' @return None
#'
#'
merge_startend_chunks <- function(se_lists, shared_key = "start") {
  sort(
    as.integer(
      unlist(
        se_lists[names(se_lists) == shared_key]
      )
    )
  )
}
