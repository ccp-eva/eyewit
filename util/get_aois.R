get_aois <- function(x, y, aoi_collection, scope = NA) {

  # destructure aoi_collection
  no_evaluation_label <- aoi_collection$no_evaluation_label
  missing_coordinate_label <- aoi_collection$missing_coordinate_label
  outside_aoi_label <- aoi_collection$outside_aoi_label

  # check if x/y length is matching
  if (length(x) != length(y)) {
    stop("Your x/y have a differnt length!")
  }

  # If scope is not explicitly set, overwrite scope boundary to include all rows
  if (missing(scope)) {
    scope <- list(start = 1, end = length(x))
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

  # setup aoi vector to be filled with the given no evaluation label
  aoi_vector <- rep(no_evaluation_label, times = length(x))


  # loop over of all rows/scope ranges
  for (seq in seq_along(scope$start)) {
    current_start <- scope$start[seq]
    current_end <- scope$end[seq]

    # loop within a range (i.e., over single rows)
    for (current_row in current_start:current_end) {

      # loop over the aoilist
      for (aoi in aoi_collection$aoilist) {
        xi <- x[current_row]
        yi <- y[current_row]

        # check if either x or y is NA (or both) if so return NA
        if (is.na(xi) || is.na(yi)) {
          aoi_vector[current_row] <- missing_coordinate_label
          break
        }

        # check the hit area
        if (xi >= aoi$x_topleft     && yi >= aoi$y_topleft &&
            xi <= aoi$x_bottomright && yi <= aoi$y_bottomright) {
          aoi_vector[current_row] <- aoi$hit_name
          break
        }

        # Finally, if current row’s x and y is neither NA nor in hit range assign FALSE
        aoi_vector[current_row] <- outside_aoi_label
      }
    }
  }

  return(aoi_vector)
}
