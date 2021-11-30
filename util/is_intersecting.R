is_intersecting <- function(rect1, rect2){

  x1 <- rect1$x_topleft
  y1 <- rect1$y_topleft
  x2 <- rect1$x_bottomright
  y2 <- rect1$y_bottomright

  x3 <- rect2$x_topleft
  y3 <- rect2$y_topleft
  x4 <- rect2$x_bottomright
  y4 <- rect2$y_bottomright

  x_intersect = max(x1, x3) <= min(x2, x4)
  y_intersect = max(y1, y3) <= min(y2, y4)

  return(x_intersect && y_intersect)
}

is_aoilist_overlapping(interface$aoisets)

