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

  # check if a mandatory columns is missing
  if (FALSE %in% (interface$mc %in% names(df))) {
    stop(paste('The following column(s) are missing:', readr::cols[which(readr::cols %in% names(df) == FALSE)]))
  }

  message("   \U1F4CA  All mandatory columns found")


  # create a lean df (only mandatory columns)
  df <- df[, c(interface$mc, interface$coi)]

  message("   \U2696   Created a lean df")


  # renaming x and y coords columns to x/y (tidyverse: df <- rename(df, x = GazePointXADCSpx))
  names(df)[names(df) == interface$xy_columns$x] <- 'x'
  names(df)[names(df) == interface$xy_columns$y] <- 'y'

  message("   \U1F5FA  Renaming coordinate columns to x and y")


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
  smmry <- df$RecordingTimestamp |> diff() |> summary()
  print(smmry)
  message(
    "   Overall Sampling Frequency\n",
    "   - mean sampling frequency:\t", format(round(1000 / smmry[[4]], 2), nsmall = 1), "\n",
    "   - median sampling frequency:\t", format(round(1000 / smmry[[3]], 2), nsmall = 1)
  )
  rm(smmry)

  message("--------------------------------------------------------------------")

  return(df)
}
