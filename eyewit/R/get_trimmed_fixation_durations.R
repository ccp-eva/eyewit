#' Get Trimmed Fixations Durations
#'
#' Given a df and trialranges, the functions trims
#' leading and trailing overflowing fixations
#' to get an accuarte duration
#' gets the adjusted fixiation durations for trials where a fixation started before the trial and
#' trials where the fixation lasted after the ending of a trial. The function will trim the
#' leading and trailing overflow fixations and will return the duration based on the provided trialrange.
#'
#'
#' @param df A dataframe.
#' @param trialrange A trial range list.
#'
#' @return Returns a named list containing durations in ms for the fixation which overflow the scope
#' @export
#'
get_trimmed_fixation_durations <- function(df, trialrange) {

  leading_durations <- NA
  trailing_durations <- NA

  if (df$FixationIndex[trialrange$start] |> is.na() |> any()) {
    # get indexes, fixation indexes, and relative position of leading fixations before trial started
    leading_fi_positions <- which(!is.na(df$FixationIndex[trialrange$start]))
    leading_fi <- df$FixationIndex[trialrange$start] |> stats::na.omit() |> as.vector()
    leading_i <- trialrange$start[leading_fi_positions]

    # get true starting timestamp
    adj_start_timestamp <- df$RecordingTimestamp[leading_i + 1] # + 1 to avoid sitting on the event marker

    # get end position of all first overflown indexes
    leading_i_end <- .eyewit_utils$fi2rn$fiend[leading_fi] + 1 # + 1 to get correct time diff

    adj_leading_durations <- df$RecordingTimestamp[leading_i_end] - adj_start_timestamp


    # create output vetor
    leading_durations <- df$FixationIndex[trialrange$start]
    # overwrite positions which are not NA with the new leading durations (NA means no change)
    leading_durations[which(!is.na(df$FixationIndex[trialrange$start]))] <- adj_leading_durations
  }


  # same for trailing indexes
  if (df$FixationIndex[trialrange$end] |> is.na() |> any()) {
    trailing_fi_positions <- which(!is.na(df$FixationIndex[trialrange$end]))
    trailing_fi <- df$FixationIndex[trialrange$end] |> stats::na.omit() |> as.vector()
    trailing_i <- trialrange$end[trailing_fi_positions]

    adj_end_timestamp <- df$RecordingTimestamp[trailing_i] # sitting on the marker is good

    # get start position of all first overflown indexes
    trailing_i_end <- .eyewit_utils$fi2rn$fistart[trailing_fi] # sitting on the actual start

    adj_trailing_durations <- adj_end_timestamp - df$RecordingTimestamp[trailing_i_end]


    # create output vetor
    trailing_durations <- df$FixationIndex[trialrange$end]
    # overwrite positions which are not NA with the new trailing durations (NA means no change)
    trailing_durations[which(!is.na(df$FixationIndex[trialrange$end]))] <- adj_trailing_durations
  }

  return(
    list(
      leading_durations = leading_durations,
      leading_trial_positions = leading_fi_positions,
      trailing_durations = trailing_durations,
      trailing_trial_positions = trailing_fi_positions
      )
    )

}
