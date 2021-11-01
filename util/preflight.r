preflight <- function(df, interface) {

  cat("====================================================================\n")
  cat("=====================   🧾  PREFLIGHT REPORT   =====================\n")
  cat("====================================================================\n")

  # check if a mandatory columns is missing
  if (FALSE %in% (interface$mc %in% names(df))) {
    stop(paste('The following column(s) are missing:', cols[which(cols %in% names(df) == FALSE)]))
  }

  cat("   📊  All mandatory columns found\n")


  # create a lean df (only mandatory columns)
  df <- df[, c(interface$mc, interface$coi)]

  cat("   ⚖️  Created a lean df\n")


  # renaming x and y coords columns to x/y (tidyverse: df <- rename(df, x = GazePointXADCSpx))
  names(df)[names(df) == interface$xy_columns$x] <- 'x'
  names(df)[names(df) == interface$xy_columns$y] <- 'y'

  cat("   🗺️  Renaming coordinate columns to x and y\n")


  # remove spaces, brackets, periods in column names
  names(df) <- gsub("\\s+", "", names(df))
  names(df) <- gsub("\\(|\\)", "", names(df))
  names(df) <- gsub("\\.", "", names(df))

  cat("   💅  Column names fixed (no spaces, brackets, periods)\n")


  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!isTRUE((all.equal(as.numeric(rownames(df)), 1:nrow(df))))) {
    stop("The df is not a incremental sequence. Do not remove any rows.")
  }

  cat("   🔢  Dataframe is in incremental sequence\n")

  # check if aoi sets have no overlapping coordinates in a single set
  is_aoilist_overlapping(interface$aoisets)

  cat("   ⚔️  No intersecting AOIs found\n")


  cat("--------------------------------------------------------------------\n")

  return(df)
}
