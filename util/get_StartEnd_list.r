#' Return a list of start/stop pairs for a given regular expression based on Tobii/E-Prime’s
#' StudioEventData column, which contains start and end markers
#' Note that the return pairs will be NOT BE shifted with +1 for Start Events or -1 for End Events
#' ... this is due to Tobiis behavior to stack events https://pasteboard.co/J24dbgx.png
#'
#' @param df A df which includes Tobii/E-Prime start and stop Markers.
#' @param chunk_pattern A regular expression.
#' @param StudioEvent_start The name of the start marker (e.g. MovieStart).
#' @param StudioEvent_end The name of the end marker (MovieEnd)
#' @return Returns a list of start/stop pairs for a given regexp.
#' @examples
#' get_StartEnd_list(coi_df, ".*Familiar.*", "MovieStart", "MovieEnd")
get_StartEnd_list <- function(df, chunk_pattern, StudioEvent_start, StudioEvent_end) {

  # check if rownames are sequential, if not stop execution
  if (!is_sequence(rownames(df))) {
    stop("The df is not in sequence. Do not remove any rows.")
  }


  MovieStart_Indexes <-
    which(
      df$StudioEvent == StudioEvent_start &
        df$StudioEventData %in% grep(chunk_pattern, df$StudioEventData, value = TRUE)
    )

  # Add +1 to skip the marker row
  # MovieStart_Indexes <- MovieStart_Indexes + 1


  MovieEnd_Indexes <-
    which(
      df$StudioEvent == StudioEvent_end &
        df$StudioEventData %in% grep(chunk_pattern, df$StudioEventData, value = TRUE)
    )

  # MovieEnd_Indexes <- MovieEnd_Indexes - 1


  # combine and sort the two list
  combined <- sort(c(MovieStart_Indexes, MovieEnd_Indexes))

  if (length(combined) %% 2 != 0) {
    stop("Lists are odd...")
  }

  return(combined)
}
