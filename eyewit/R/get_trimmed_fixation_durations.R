#' Get Trimmed Fixation Durations
#'
#' Given a df and trialranges, the functions trims
#' leading and trailing overflowing fixations
#' to get an accurate duration. Gets the adjusted fixation durations for trials where a fixation started before the trial and
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

  # get indexes, fixation indexes, and relative position of leading fixations before trial started
  leading_fi_positions <- which(!is.na(df$fi[trialrange$start]))
  leading_fi <- df$fi[trialrange$start] |> stats::na.omit() |> as.vector()
  leading_i <- trialrange$start[leading_fi_positions]

  # get true starting timestamp
  adj_start_timestamp <- df$timestamp[leading_i + 1] # + 1 to avoid sitting on the event marker

  # get end position of all first overflown indexes
  leading_i_end <- .eyewit_utils$fi2rn$fiend[leading_fi] + 1 # + 1 to get correct time diff

  adj_leading_durations <- df$timestamp[leading_i_end] - adj_start_timestamp


  # create output vector
  leading_durations <- df$fi[trialrange$start]
  # overwrite positions which are not NA with the new leading durations (NA means no change)
  leading_durations[which(!is.na(df$fi[trialrange$start]))] <- adj_leading_durations



  # same for trailing indexes
  trailing_fi_positions <- which(!is.na(df$fi[trialrange$end]))
  trailing_fi <- df$fi[trialrange$end] |> stats::na.omit() |> as.vector()
  trailing_i <- trialrange$end[trailing_fi_positions]

  adj_end_timestamp <- df$timestamp[trailing_i] # sitting on the marker is good

  # get start position of all first overflown indexes
  trailing_i_end <- .eyewit_utils$fi2rn$fistart[trailing_fi] # sitting on the actual start

  adj_trailing_durations <- adj_end_timestamp - df$timestamp[trailing_i_end]


  # create output vetor
  trailing_durations <- df$fi[trialrange$end]
  # overwrite positions which are not NA with the new trailing durations (NA means no change)
  trailing_durations[which(!is.na(df$fi[trialrange$end]))] <- adj_trailing_durations

  return(
    list(
      leading_durations = leading_durations,
      leading_trial_positions = leading_fi_positions,
      trailing_durations = trailing_durations,
      trailing_trial_positions = trailing_fi_positions
      )
    )

}
