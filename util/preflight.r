preflight <- function(df, interface) {

  # check if a mandatory columns is missing
  if (FALSE %in% (interface$mc %in% names(df))) {
    stop(paste('The following column(s) are missing:', cols[which(cols %in% names(df) == FALSE)]))
  }

  # create a lean df (only mandatory columns)
  df <- df[, c(interface$mc, interface$coi)]

  # remove spaces, brackets, periods in column names
  names(df) <- gsub("\\s+", "", names(df))
  names(df) <- gsub("\\(|\\)", "", names(df))
  names(df) <- gsub("\\.", "", names(df))

  # renaming x and y coords columns to x/y (tidyverse: df <- rename(df, x = GazePointXADCSpx))
  names(df)[names(df) == interface$xy_columns$x] <- 'x'
  names(df)[names(df) == interface$xy_columns$y] <- 'y'

  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!isTRUE((all.equal(as.numeric(rownames(df)), 1:nrow(df))))) {
    stop("The df is not a incremental sequence. Do not remove any rows.")
  }

  # check if aoi sets have no overlapping coordinates in a single set
  # https://github.com/Kalaschnik/tobii-eyetracking-utilities/issues/25
  # todo
  return(df)
}
