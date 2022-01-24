test_that("Checking Rectangle Intersections", {
  expect_true(
    is_intersecting(
      list(x_topleft = 10, y_topleft = 10, x_bottomright = 20, y_bottomright = 20),
      list(x_topleft = 15, y_topleft = 15, x_bottomright = 25, y_bottomright = 25)
    )
  )

  expect_true(
    is_intersecting(
      list(x_topleft = 10, y_topleft = 10, x_bottomright = 15, y_bottomright = 30),
      list(x_topleft = 15, y_topleft = 10, x_bottomright = 20, y_bottomright = 30)
    )
  )

  expect_false(
    is_intersecting(
      list(x_topleft = 10, y_topleft = 10, x_bottomright = 20, y_bottomright = 20),
      list(x_topleft = 21, y_topleft = 10, x_bottomright = 31, y_bottomright = 20)
    )
  )
})
