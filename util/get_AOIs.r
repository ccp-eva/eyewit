get_AOIs <- function(df, aoi_collection, scope = 0, ) {
browser()
  # check if rownames are sequential, if not stop execution
  if (!is_sequence(rownames(df))) stop("The df is not in sequence. Do not remove any rows.")

  # destructure aoi_body
  column_name <- aoi_collection$column_name
  no_evaluation_label <- aoi_collection$no_evaluation_label
  missing_coordinate_label <- aoi_collection$missing_coordinate_label


  # get coordinate columns of df
  x_coords <- "GazePointX..ADCSpx."
  y_coords <- "GazePointY..ADCSpx."



  # create a new AOI Column and fill it with no_evaluation_label
  df[, column_name] <- no_evaluation_label
  # put it to first position
  df <- df[, c(which(colnames(df) == column_name), which(colnames(df) != column_name))]

  # If scope is not explicitly set, overwrite scope boundary to include all rows
  if (length(scope) == 0) {
    scope$start <- 1
    scope$end <- nrow(df)
  }

  # cont vectorized version of aoi checker
  for (i in seq_along(scope$start)) {
    print(scope$start[i])
    print(scope$end[i])

    for (aoi in aoi_collection$aoilist) {
      print(aoi)
    }

  }



  # AOI CHECKER
  # checks if a pair of given coordinates belong to any AOI rectangle, and returns the AOI as string ...
  # basically it checks for each x if its between x1 and x2, that is: |x1 <= x >= x2| same for each y
  aoi_checker <- function(aoi_body, x_vector, y_vector) {

    # if else, because if is not vectorized
    ifelse(
      # check for NA, if so return missing coordinate label
      is.na(x_vector) | is.na(y_vector), return(missing_coordinate_label),
      ifelse(
        # check if the current cell is already occupied with a value other than FALSE
        # ifelse(df$column_name)
        # check for if coordinates within boundaries and return the aoi_hit_name
        x_vector >= aoi_body$aoilist$my_first_aoi$x_topright &
        y_vector >= aoi_body$aoilist$my_first_aoi$y_topright &
        x_vector <= aoi_body$aoilist$my_first_aoi$x_bottomright &
        y_vector <= aoi_body$aoilist$my_first_aoi$y_bottomright,
        return(aoi_body$aoilist$my_first_aoi$aoi_hit_name),
        # else return FALSE
        FALSE # IDEA::: WRAP this in a for loop and use next HERE TO CHECK THE NEXT AOI RANGE, IF THE LAST AOI RANGE WAS HIT RETURN FALSE
      )
    )
  }

  # response <- mapply(aoi_checker,x_vector,y_vector)


  while (length(scope) > 0) {
    current_row_start_pos <- scope[1]
    current_row_end_pos <- scope[2]
    for (i in current_row_start_pos:current_row_end_pos) {
      df[i, column_name] <- aoi_checker(df$GazePointX..ADCSpx.[i], df$GazePointY..ADCSpx.[i])
    }

    # remove current rowname pair
    scope <- scope[!scope %in% c(current_row_start_pos, current_row_end_pos)]

  }

  return(df)

}
