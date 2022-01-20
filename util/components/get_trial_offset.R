get_trial_offset <- function(df, trial_ranges, trial_offset) {
  # ... use time ranges defined by trial_offset to overwrite trial_ranges
  # get all starting times at trial_ranges start
  starting_times <- df$RecordingTimestamp[trial_ranges$start]

  # only modify trial_ranges if input was numeric
  # check if there was an actual numeric value
  if (trial_offset[1] != "start") {
    # Add the starttime (trial_offset[1]) to every start position (trial_ranges$start) ...
    # ... of the recording timestamp to get the actual start window in milliseconds
    starting_times <- df$RecordingTimestamp[trial_ranges$start] + as.numeric(trial_offset[1])
  }

  if (trial_offset[2] != "end") {
    # Add the endtime argument to the starting times
    ending_times <- df$RecordingTimestamp[trial_ranges$start] + as.numeric(trial_offset[2])
  }

  if (trial_offset[1] == "start") {
    start_indexes <- trial_ranges$start
  } else {
    # find the closest matching index for starting and ending times
    # find closest match (http://adomingues.github.io/2015/09/24/finding-closest-element-to-a-number-in-a-list/)
    start_indexes <- unlist(
      lapply(
        starting_times,
        function(x) which.min(abs(df$RecordingTimestamp - x))
      )
    )
  }

  if (trial_offset[2] == "end") {
    end_indexes <- trial_ranges$end
  } else {
    end_indexes <- unlist(
      lapply(
        ending_times,
        function(x) which.min(abs(df$RecordingTimestamp - x))
      )
    )
  }

  return(list(start = start_indexes, end = end_indexes))
}
