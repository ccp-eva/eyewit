allocate_trials <- function(df, index_pairs, chunks_per_trial = 1, reset_to_1 = TRUE, fill_StudioEventData = TRUE) {

  # Check if Trial column exists, if not create it
  if (!"Trial" %in% colnames(df)) {
    df <- add_column(df, Trial = NA, .before = 1)
  }

  if (length(index_pairs) > 2 && missing(chunks_per_trial)) {
    stop("You provided multiple startend lists, without specifying chunks_per_trial")
  }

  if (!missing(chunks_per_trial) && length(index_pairs) > 2) {
    bucket <- list(
      start = merge_startend_chunks(index_pairs, "start"),
      end = merge_startend_chunks(index_pairs, "end")
    )

    # clean index_pairs, since it contains multiple starts/ends
    rm(index_pairs)
    index_pairs <- bucket
  }

  trial_counter <- 0

  # if reset_to_1 == False, look for max number in Trials
  if (!reset_to_1) {
    trial_counter <- max(df$Trial, na.rm = TRUE)
  }

  for (i in seq_along(index_pairs$start)) {
    start_pos <- index_pairs$start[i]
    end_pos <- index_pairs$end[i]

    # increment trial counter
    if (!missing(chunks_per_trial)) {
      # check if i modulo chunks_per_trial is 1, if so increment, examples:
      #   1 %% 2 = 1 -> increment
      #   2 %% 2 = 0 -> dont increment (next chunk will also be 1)
      #   3 %% 2 = 1 -> increment, etc...
      if (i %% chunks_per_trial == 1) {
        trial_counter <- trial_counter + 1
      }
    } else {
      trial_counter <- trial_counter + 1
    }

    # assign current trial counter to inter trial chunk
    df$Trial[start_pos:end_pos] <- rep(trial_counter, length(start_pos:end_pos))


    # Fill up StudioEventData
    if (fill_StudioEventData) {
      df$StudioEventData[start_pos:end_pos] <- df$StudioEventData[start_pos]
    }
  }

  return(df)
}
