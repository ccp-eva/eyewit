getLTs <- function(df, aoi_collection, scope, intra_scope_window = c(0, 0), intra_scope_cut = TRUE) {

  # check if intra_scope_window was passed as an argument, if so ...
  # ... use time ranges defined by intra_scope_window to overwrite scope
  if (!missing(intra_scope_window)) {

    # Add the starttime (intra_scope_window[1]) to every start position (scope$start) ...
    # ... of the recording timestamp to get the actual start window in milliseconds
    starting_times <- df$RecordingTimestamp[scope$start] + intra_scope_window[1]
    # Add the endtime argument to the starting times
    ending_times <- starting_times + intra_scope_window[2]

    # find the closest matching index for starting and ending times
    # find closest match (http://adomingues.github.io/2015/09/24/finding-closest-element-to-a-number-in-a-list/)
    start_indexes <- unlist(
      lapply(
        starting_times,
        function(x) which.min(abs(df$RecordingTimestamp - x))
      )
    )
    end_indexes <- unlist(
      lapply(
        ending_times,
        function(x) which.min(abs(df$RecordingTimestamp - x))
      )
    )

    # overwrite scope
    scope <- list(start = start_indexes, end = end_indexes)
  }

  # TODO carefully compare the remainder of all get fixation functions... how much shit they share


}



df <- readRDS("rds_df")
aoi_collection <- readRDS("rds_aoi_collection")
scope <- readRDS("rds_scope")
intra_scope_window <- c(2000, 3000)


getLTs(df, aoi_collection, scope, intra_scope_window)
