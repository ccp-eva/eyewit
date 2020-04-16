allocateTrials <- function(df, index_pairs, fill_StudioEventData = TRUE) {

  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!isTRUE((all.equal(as.numeric(rownames(df)), 1:nrow(df))))) stop("The df is not in sequence. Do not remove any rows.")

  # Check if Trial column exists, if not create it
  if (!"Trial" %in% colnames(df)) {
    df <- cbind(Trial = NA, df)
  }

  trial_counter <- 1

  for (i in seq_along(index_pairs$start)) {

    start_pos <- index_pairs$start[i]
    end_pos <- index_pairs$end[i]

    # assign current trial counter to inter trial chunk
    df$Trial[start_pos:end_pos] <- rep(trial_counter, length(start_pos:end_pos))


    # Fill up StudioEventData
    if (fill_StudioEventData) df$StudioEventData[start_pos:end_pos] <- df$StudioEventDat[start_pos]

    # increment trial counter
    trial_counter <- trial_counter + 1
  }

  return(df)

}
