preflight <- function(df, interface) {

  message("====================================================================")
  message("=====================   🧾  PREFLIGHT REPORT   =====================")
  message("====================================================================")

  # check if a mandatory columns is missing
  if (FALSE %in% (interface$mc %in% names(df))) {
    stop(paste('The following column(s) are missing:', cols[which(cols %in% names(df) == FALSE)]))
  }

  message("   📊  All mandatory columns found")


  # create a lean df (only mandatory columns)
  df <- df[, c(interface$mc, interface$coi)]

  message("   ⚖️  Created a lean df")


  # renaming x and y coords columns to x/y (tidyverse: df <- rename(df, x = GazePointXADCSpx))
  names(df)[names(df) == interface$xy_columns$x] <- 'x'
  names(df)[names(df) == interface$xy_columns$y] <- 'y'

  message("   🗺️  Renaming coordinate columns to x and y")


  # remove spaces, brackets, periods in column names
  names(df) <- gsub("\\s+", "", names(df))
  names(df) <- gsub("\\(|\\)", "", names(df))
  names(df) <- gsub("\\.", "", names(df))

  message("   💅  Column names fixed (no spaces, brackets, periods)")


  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!isTRUE((all.equal(as.numeric(rownames(df)), 1:nrow(df))))) {
    stop("The df is not a incremental sequence. Do not remove any rows.")
  }

  message("   🔢  Dataframe is in incremental sequence")

  # check if aoi sets have no overlapping coordinates in a single set
  is_aoilist_overlapping(interface$aoisets)

  message("   ⚔️  No intersecting AOIs found")

  cat("--------------------------------------------------------------------\n")
  message("--------------------------------------------------------------------\n")
  message("--------------------------------------------------------------------")

  return(df)
}
