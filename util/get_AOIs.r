get_AOIs <- function(df, aoi_body, rowname_pairs) {

  # check if rownames are sequential, if not stop execution
  if (!is_sequence(rownames(df))) {
    stop("The df is not in sequence. Do not remove any rows.")
  }

  # stop execution if they are unequal
  if (length(rowname_pairs) %% 2 != 0) {
    stop("Your index pairs (rowname pairs) are odd :( ... They need to be even!")
  }

  # create a new AOI Column and fill it with "NO EVAL" as the default
  df <- cbind(AOIFamBodyObj = "NO EVAL", df, stringsAsFactors = FALSE)


  # Define AOIs
  # We only define the coordinate pair for the top left and bottom right position ...
  # ... this creates a rectangular surface...
  # ... We store this in a matrix, where x1,y1 is the coordinate pair for the top left position and ...
  # ... where x2,y2 is the bottom right position of the rectangualr surface aoi
  # ... matrix(c(x1,y1,x2,y2)
  aoi_left <- matrix(c(79, 159, 759, 1089), nrow = 2, ncol = 2)
  aoi_center <- matrix(c(844, 794, 1204, 1154), nrow = 2, ncol = 2)
  aoi_right <- matrix(c(1305, 159, 1985, 1089), nrow = 2, ncol = 2)


  # AOI CHECKER
  # checks if a pair of given coordinates belong to any AOI rectangle, and returns the AOI as string ...
  # basically it checks for each x if its between x1 and x2, that is: |x1 <= x >= x2| same for each y
  aoi_checker <- function(x, y) {

    # check if either x or y is NA (or both) if so return NA
    if (is.na(x) || is.na(y)) {
      return(NA)
    }

    # check for aoi_left, that is check if x y is in the surface of aoi_left
    if (x >= aoi_left[1, 1] && y >= aoi_left[2, 1] && x <= aoi_left[1, 2] && y <= aoi_left[2, 2])
      return("left")  # if x and(!) y was found within the boundaries return "aoi_left"

    # Check for aoi_center
    if (x >= aoi_center[1, 1] && y >= aoi_center[2, 1] && x <= aoi_center[1, 2] && y <= aoi_center[2, 2])
      return("center")

    # Check for aoi_right
    if (x >= aoi_right[1, 1] && y >= aoi_right[2, 1] && x <= aoi_right[1, 2] && y <= aoi_right[2, 2])
      return("right")

    # if there is no match at all, the subject did not look in any of the given AOIs, thus return false
    return(FALSE)
  }

  while (length(rowname_pairs) > 0) {
    current_row_start_pos <- rowname_pairs[1]
    current_row_end_pos <- rowname_pairs[2]
    for (i in current_row_start_pos:current_row_end_pos) {
      df$AOIFamBodyObj[i] <- aoi_checker(df$GazePointX..ADCSpx.[i], df$GazePointY..ADCSpx.[i])
    }

    # remove current rowname pair
    rowname_pairs <- rowname_pairs[!rowname_pairs %in% c(current_row_start_pos, current_row_end_pos)]

  }

  return(df)

}
