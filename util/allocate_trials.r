allocate_trials <- function(df, index_pairs) {

  # index_pairs need to be even (i.e., pairs of start and stop events)
  if (length(index_pairs) %% 2 != 0) {
    stop("List is odd...")
  }

  # check if rownames are sequential, if not stop execution
  if (!is_sequence(rownames(df))) {
    stop("The df is not in sequence. Do not remove any rows.")
  }


  # Check if Trial column exists, if not create it
  if (!"Trial" %in% colnames(df)) {
    df <- cbind(Trial = NA, df)
  }

  trial_counter <- 1

  while (length(index_pairs) > 0) {

    start_pos <- index_pairs[1]
    end_pos <- index_pairs[2]

    # assign current trial counter to inter trial chunk
    df$Trial[start_pos:end_pos] <- rep(trial_counter, length(start_pos:end_pos))


    # Fill up StudioEventData
    df$StudioEventData[start_pos:end_pos] <- df$StudioEventDat[start_pos]


    # remove start/end_pos
    index_pairs <- index_pairs[!index_pairs %in% c(start_pos, end_pos)]

    # increment trial counter
    trial_counter <- trial_counter + 1
  }

  return(df)

}
