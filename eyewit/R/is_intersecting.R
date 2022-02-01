#' Check if two rectangles are intersecting each other
#'
#' Given a list of two rectangles—each containing four coordinates—this function returns a boolean
#' scalar (either TRUE or FALSE) if the two rectangles intersect or touch each other.
#'
#' This function is part various preflight checks. **It is important that you provide the key names
#' with the correct names** (i.e., x_topleft, y_topleft, x_bottomright, y_bottomright) for both
#' rectangles.
#'
#' @family preflight functions
#'
#' @param rect1,rect2 A named list with the key names: x_topleft, y_topleft, x_bottomright,
#'   y_bottomright.
#'
#' @return Returns a logical scalar
#' @export
#'
#' @examples
#' is_intersecting(
#'   list(x_topleft = 10, y_topleft = 10, x_bottomright = 20, y_bottomright = 20),
#'   list(x_topleft = 15, y_topleft = 15, x_bottomright = 25, y_bottomright = 25)
#' ) # Rectangles are clearly intersecting (⧉) → returns TRUE
#'
#' is_intersecting(
#'   list(x_topleft = 10, y_topleft = 10, x_bottomright = 15, y_bottomright = 30),
#'   list(x_topleft = 15, y_topleft = 10, x_bottomright = 20, y_bottomright = 30)
#' ) # The overlapping x at 15 is problematic (◫) → returns TRUE
#'
#' is_intersecting(
#'   list(x_topleft = 10, y_topleft = 10, x_bottomright = 20, y_bottomright = 20),
#'   list(x_topleft = 21, y_topleft = 10, x_bottomright = 31, y_bottomright = 20)
#' ) # No intersect (◻◻) → returns FALSE

is_intersecting <- function(rect1, rect2){

  x_intersect =
    max(rect1$x_topleft, rect2$x_topleft) <= min(rect1$x_bottomright, rect2$x_bottomright)
  y_intersect =
    max(rect1$y_topleft, rect2$y_topleft) <= min(rect1$y_bottomright, rect2$y_bottomright)

  return(x_intersect && y_intersect)
}
