getExperimentDuration <- function(df, first_StudioEventData_name, timeformat = "m") {

  if (!"RecordingTimestamp" %in% colnames(df) &&
      !"StudioEventData" %in% colnames(df) &&
      !"StudioEventIndex" %in% colnames(df)
  ) {
    stop("Column(s) missing: I need RecordingTimestamp, StudioEventData and StudioEventIndex.")
  }

  # Find the experiment start in milliseconds (i.e., the first familiarization video)
  # the last + 1 is shifting the index + 1 to not use the marker row
  start_timestamp <- df$RecordingTimestamp[which(df$StudioEventData == first_StudioEventData_name)[1] + 1]

  # Find the experiment end in milliseconds (i.e., the last Studio Event Index)
  # the last - 1 is shifting the index - 1 to not use the marker row
  end_timestamp <- df$RecordingTimestamp[which(df$StudioEventIndex == max(df$StudioEventIndex, na.rm = TRUE)) - 1]

  duration_in_ms <- end_timestamp - start_timestamp

  # return timeformat is milliseconds
  if (timeformat == "ms") {
    return(duration_in_ms)
  }

  # return timeformat is seconds
  if (timeformat == "s") {
    return(duration_in_ms / 1000)
  }

  # return timeformat is hours:minutes:seconds (h:m:s)
  if (timeformat == "h" || timeformat == "m") {
    hrs <- duration_in_ms/(60 * 60 * 1000)
    mins <- (hrs %% 1) * 60
    secs <- (mins %% 1) * 60
    hour_time <- paste(trunc(hrs), trunc(mins), round(secs, 2), sep = ":")
    return(hour_time)
  }
}
