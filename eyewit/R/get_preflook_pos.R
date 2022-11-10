#' Title
#'
#' @param preflook_string_vector preflook_string_vector
#' @param familiar_object_vector familiar_object_vector
#'
#' @return None
#' @export
#'
get_preflook_pos <- function(preflook_string_vector, familiar_object_vector) {

  preflook_string_vector_length <- length(preflook_string_vector)
  familiar_object_vector_length <- length(familiar_object_vector)

  # check if both argument lengths are equal
  if (preflook_string_vector_length != familiar_object_vector_length) {
    stop("Unequal length of vectors")
  }

  # collector buckets
  fam_positions <- c()
  nov_positions <- c()

  for (i in 1:preflook_string_vector_length) {

    # store current familar object (e.g., 11_a)
    current_familiar_object <- familiar_object_vector[i]

    # store current preflook string (e.g., 1a_ObjectY_b_Obj_11_b_LEFT_ObjectY_a_Obj_11_a_RIGHT.wmv)
    current_preflook_string <- preflook_string_vector[i]

    # ugly data fix, lol. wtf.
    # check if preflook string contains a " (1)" pattern:
    if (grepl("\\s\\(\\d\\)", current_preflook_string)) {
    	current_preflook_string <-
    		substring(current_preflook_string, 1, nchar(current_preflook_string) - 4)
    }

    # the goal is to find the familiar object string (11_a) within the preflook string
    # it can either bei in the first part (LEFT) or at the right part (RIGHT)
    # destructure current preflook by "_"
    current_preflook_string <- unlist(strsplit(current_preflook_string, "_"))

    # store positions (LEFT key = Index 5 and 6; Right = Index key 11 and 12)
    LEFTpos <- current_preflook_string[6]
    RIGHTpos <- current_preflook_string[7]

    # Check if current familiar object is at LEFTpos or RIGHTpos
    # ... and add fam_positions and nov_positions (opposite)
    if (unlist(current_familiar_object)[8] == LEFTpos) {
      fam_positions <- c(fam_positions, "left")
      nov_positions <- c(nov_positions, "right")
    }

    if (unlist(current_familiar_object)[8] == RIGHTpos) {
      fam_positions <- c(fam_positions, "right")
      nov_positions <- c(nov_positions, "left")
    }

  }

  return(list(fam_pos = fam_positions, nov_pos = nov_positions))

}
