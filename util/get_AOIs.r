getAOIs <- function(df, aoi_collection, scope = FALSE, return_df = TRUE, coordinate_system = "ADCS") {

  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!(all.equal(as.numeric(rownames(df)), 1:nrow(df)))) stop("The df is not in sequence. Do not remove any rows.")

  # destructure aoi_collection
  column_name <- aoi_collection$column_name
  no_evaluation_label <- aoi_collection$no_evaluation_label
  missing_coordinate_label <- aoi_collection$missing_coordinate_label

  # get df length
  df_row_count <- nrow(df)


  # If scope is not explicitly set, overwrite scope boundary to include all rows
  if (length(scope) == 1) {
    scope <- list(start = 1, end = df_row_count)
  }

  # todo check if there are multiple coordinate systems
  # get coordinate columns of df
  x_coords_column <- "GazePointX..ADCSpx."
  y_coords_column <- "GazePointY..ADCSpx."


  # setup aoi vector to be filled with the given no evaluation label
  aoi_vector <- rep(no_evaluation_label, times = df_row_count)


  # loop over of all rows/scope ranges
  for (seq in seq_along(scope$start)) {
    current_start <- scope$start[seq]
    current_end <- scope$end[seq]

    # loop within a range (i.e., over single rows)
    for (current_row in current_start:current_end) {

      # loop over the aoilist
      for (aoi in aoi_collection$aoilist) {
        row_x_value <- df[current_row, x_coords_column]
        row_y_value <- df[current_row, y_coords_column]

        # check if either x or y is NA (or both) if so return NA
        if (is.na(row_x_value) || is.na(row_y_value)) {
          aoi_vector[current_row] <- missing_coordinate_label
          break
        }

        # check the hit area
        if (row_x_value >= aoi$x_topright    && row_y_value >= aoi$y_topright &&
            row_x_value <= aoi$x_bottomright && row_y_value <= aoi$y_bottomright) {
          aoi_vector[current_row] <- aoi$hit_name
          break
        }

        # Finally, if current row’s x and y is neither NA nor in hit range assign FALSE
        aoi_vector[current_row] <- FALSE
      }
    }
  }


  if (return_df) {
    # insert aoi vector into input df
    df[, column_name] <- aoi_vector
    # put aoi column in first position
    df <- df[, c(which(colnames(df) == column_name), which(colnames(df) != column_name))]
    return(df)
  }

  return(aoi_vector)
}


