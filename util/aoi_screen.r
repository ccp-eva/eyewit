aoi_screen <- function(df, rowname_pairs) {

  # check if rownames are sequential, if not stop execution
  if (!is_sequence(rownames(df))) {
    stop("The df is not in sequence. Do not remove any rows.")
  }

  # stop execution if they are unequal
  if (length(rowname_pairs) %% 2 != 0) {
    stop("Your index pairs (rowname pairs) are odd :( ... They need to be even!")
  }

  # create a new AOI Column and fill it with "NO EVAL" as the default
  df <- cbind(AOIScreen = "NO EVAL", df, stringsAsFactors = FALSE)


  aoi_screen <- matrix(c(0, 0, 2048, 1152), nrow = 2, ncol = 2)


  aoi_checker <- function(x, y) {

    # check if either x or y is NA (or both) if so return NA
    if (is.na(x) || is.na(y)) {
      return(NA)
    }

    if (x >= aoi_screen[1, 1] && y >= aoi_screen[2, 1] && x <= aoi_screen[1, 2] && y <= aoi_screen[2, 2])
      return(TRUE)

    return(FALSE)
  }

  while (length(rowname_pairs) > 0) {
    current_row_start_pos <- rowname_pairs[1]
    current_row_end_pos <- rowname_pairs[2]
    for (i in current_row_start_pos:current_row_end_pos) {
      df$AOIScreen[i] <- aoi_checker(df$GazePointX..ADCSpx.[i], df$GazePointY..ADCSpx.[i])
    }

    # remove current rowname pair
    rowname_pairs <- rowname_pairs[!rowname_pairs %in% c(current_row_start_pos, current_row_end_pos)]

  }
  return(df)
}
