getAOIs <- function(df, aoi_collection, scope = c(0, 0), return_df = TRUE, coordinate_system = "ADCS") {

  # destructure aoi_collection
  column_name <- aoi_collection$column_name
  no_evaluation_label <- aoi_collection$no_evaluation_label
  missing_coordinate_label <- aoi_collection$missing_coordinate_label

  # get df length
  df_row_count <- nrow(df)

  # If scope is not explicitly set, overwrite scope boundary to include all rows
  if (missing(scope)) {
    scope <- list(start = 1, end = df_row_count)
  }

  # If scope was set, check if the user entered multiple ranges using c(scope1, scope2, etc.)
  if (!missing(scope) && length(scope) > 2) {
    # merge the multiple scope$start elements and sort them
    all_starts <- sort(unlist(scope[names(scope) == 'start'], use.names = FALSE))
    all_ends <- sort(unlist(scope[names(scope) == 'end'], use.names = FALSE))
    # overwrite input scope
    scope$start <- all_starts
    scope$end <- all_ends
  }

  # check if length is matching
  if (length(scope$start) != length(scope$end)) {
    stop("Your input scope is not equal")
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
        if (row_x_value >= aoi$x_topleft    && row_y_value >= aoi$y_topleft &&
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


