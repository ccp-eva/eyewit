#' Title
#'
#' @param df df
#' @param interface interface
#'
#' @return None
#' @export
#'
preflight <- function(df, interface) {

  message("====================================================================")
  message("=====================   \U1F9FE  PREFLIGHT REPORT   =====================")
  message("====================================================================")


  # check if a FixationIndex is present in raw data (Tobii Studio had it, Tobii ProLab is missing that; yet, eyewit heavily relies on it)
  # if not execute create_fi to create an fi column
  if (!"FixationIndex" %in% names(df)) {
  	cat("\tFixationIndex is missing\n")
  	df <- create_fi(df, interface$type_col, interface$type_index_col)
  }
  message("   \U1F5FA  Found/Created FixationIndex")

  # rename df columns based on key names defined in vendor_lookup
  names(df)[names(df) == names(vendor_lookup[[vendor]]$participant)] <- 'participant'
  names(df)[names(df) == names(vendor_lookup[[vendor]]$timestamp)] <- 'timestamp'
  names(df)[names(df) == names(vendor_lookup[[vendor]]$event)] <- 'event'
  names(df)[names(df) == names(vendor_lookup[[vendor]]$eventValue)] <- 'eventValue'
  names(df)[names(df) == names(vendor_lookup[[vendor]]$gazeType)] <- 'gazeType'
  names(df)[names(df) == names(vendor_lookup[[vendor]]$gazeDuration)] <- 'gazeDuration'
  names(df)[names(df) == names(vendor_lookup[[vendor]]$x)] <- 'x'
  names(df)[names(df) == names(vendor_lookup[[vendor]]$y)] <- 'y'
  message("   \U1F5FA  Renaming vendor-specific column names to eyewit generic names")



  # check if a mandatory columns is missing
  if (FALSE %in% (interface$mc %in% names(df))) {
    stop(paste('The following column(s) are missing:', readr::cols[which(readr::cols %in% names(df) == FALSE)]))
  }
  message("   \U1F4CA  All mandatory columns found")


  # check if timestamps are in microseconds (i.e., distance from one sample to another is great 1000)
  # ... divide timestamps by 1000
  if (df$timestamp |> diff() |> stats::median() > 2000) {
    df$timestamp <- floor(df$timestamp / 1000)
  }




  # create a lean df (only mandatory columns)
  df <- df[, c(interface$mc, interface$coi)]

  message("   \U2696   Created a lean df")







  # remove spaces, brackets, periods in column names
  names(df) <- gsub("\\s+", "", names(df))
  names(df) <- gsub("\\(|\\)", "", names(df))
  names(df) <- gsub("\\.", "", names(df))

  message("   \U1F485  Column names fixed (no spaces, brackets, periods)")


  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!isTRUE((all.equal(as.numeric(rownames(df)), 1:nrow(df))))) {
    stop("The df is not a incremental sequence. Do not remove any rows.")
  }

  message("   \U1F522  Dataframe is in incremental sequence")

  # check if aoi sets have no overlapping coordinates in a single set
  is_aoilist_intersecting(interface$aoisets)

  message("   \U2694  No intersecting AOIs found")

  message("\n   Single Samples Summary (i.e., the difference in time between each sample (row):")
  smmry <- df$timestamp |> diff() |> summary()
  print(smmry)
  message(
    "   Overall Sampling Frequency\n",
    "   - mean sampling frequency:\t", format(round(1000 / smmry[[4]], 2), nsmall = 1), "\n",
    "   - median sampling frequency:\t", format(round(1000 / smmry[[3]], 2), nsmall = 1)
  )
  rm(smmry)

  message("--------------------------------------------------------------------")

  .eyewit_utils <- list(
    fi2rn = fi2rn(df$fi)
  )

  return(df)
}
