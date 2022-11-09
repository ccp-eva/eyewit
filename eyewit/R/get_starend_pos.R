#' Get Start & End Positions
#'
#' Return a named list of start and stop positions for a given regular expression based on
#' event column, which contains start and end markers
#'
#' Note that the return pairs will be NOT BE shifted with +1 for Start Events or -1 for End Events
#' this is due to Tobii’s bad behavior to sometimes stack events https://pasteboard.co/J24dbgx.png
#'
#' @param df A df which includes Tobii/E-Prime start and stop Markers.
#' @param regex_pattern A regular expression.
#' @param event_start The name of the start marker (e.g. MovieStart).
#' @param event_end The name of the end marker (MovieEnd)
#'
#' @export
#'
#' @return Returns a named list of start and stop positions for a given regexp.
#' @examples
#' # get_StartEnd_list(coi_df, ".*Familiar.*", "MovieStart", "MovieEnd")
get_start_end_pos <- function(df, regex_pattern, event_start = "MovieStart", event_end = "MovieEnd") {

  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!isTRUE((all.equal(as.numeric(rownames(df)), 1:nrow(df))))) stop("The df is not in sequence. Do not remove any rows.")


  MovieStart_Indexes <-
    which(
      df$event == event_start &
        df$eventValue %in% grep(regex_pattern, df$eventValue, value = TRUE)
    )

  # Add +1 to skip the marker row
  # MovieStart_Indexes <- MovieStart_Indexes + 1


  MovieEnd_Indexes <-
    which(
      df$event == event_end &
        df$eventValue %in% grep(regex_pattern, df$eventValue, value = TRUE)
    )

  # MovieEnd_Indexes <- MovieEnd_Indexes - 1


  # check if both indexes are of equal length
  if (length(MovieStart_Indexes) != length(MovieEnd_Indexes)) {
    stop("Start end End list are uneven")
  }

  # return named list
  return(list(start = MovieStart_Indexes, end = MovieEnd_Indexes))

}
