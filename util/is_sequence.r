#' Check if a vector is sequential
#'
#' Inspired by flodel.filter function: https://stackoverflow.com/a/16120454/2258480
#' Note that flodel.which is producing incorrect results
#' @param test_vector The vector to be tested for sequence.
#' @param sequence_length The minimal amount of consecutive elements (defaults the vector length).
#' @param step_size The increment value (defaults to 1).
#' @return Returns boolean.
#' @examples
#' is_sequence(1:10) # True
#' is_sequence(seq(0, 10, by=2),,2) # True
#' is_sequence(cbind(1,2,3,7,8,14),3) # True
#' is_sequence(cbind(1,2,5,12,13,14,15,16), 3) # True
#' is_sequence(cbind(1,2,5,12,13,14,15,16)) # False
is_sequence <- function(test_vector, sequence_length = length(test_vector), step_size = 1L) {
  if (sequence_length > length(test_vector)) return(FALSE)
  test_vector <- as.integer(test_vector)
  is.cons <- tail(test_vector, -1L) == head(test_vector, -1L) + step_size
  any(filter(is.cons, rep(1L, sequence_length - 1L), sides = 1, method = "convolution") == sequence_length - 1L,
      na.rm = TRUE)
}

