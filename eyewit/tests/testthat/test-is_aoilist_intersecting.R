test_that("Checking AOI intersections", {

  aoiset1 <- list(
    scene1 = list(
      column_name = "set1_scene1",
      aoilist = list(
        aoi1 = list(hit_name = "left", x_topleft = 60, y_topleft = 290, x_bottomright = 690, y_bottomright = 1130),
        aoi2 = list(hit_name = "right", x_topleft = 1225, y_topleft = 290, x_bottomright = 1855, y_bottomright = 1130),
        aoi3 = list(hit_name = "top", x_topleft = 790, y_topleft = 10, x_bottomright = 1130, y_bottomright = 350),
        aoi4 = list(hit_name = "bottom", x_topleft = 790, y_topleft = 730, x_bottomright = 1130, y_bottomright = 1070)
      )
    ),
    scene2 = list(
      column_name = "set1_scene2",
      aoilist = list(
        aoi1 = list(hit_name = "top", x_topleft = 790, y_topleft = 10, x_bottomright = 1130, y_bottomright = 350),
        aoi2 = list(hit_name = "bottom", x_topleft = 790, y_topleft = 730, x_bottomright = 1130, y_bottomright = 1070)
      )
    )
  )

  # Identical to aoiset1, except set2_scene1 is overlapping. The "bottom" AOI uses an x_bottomright
  # of 1225 and thus reaches into the "right" aoi.
  # See the right visualization: https://raw.githubusercontent.com/kalaschnik/media/main/eyewit/unittest-intersecting-aois.png
  aoiset2 <- list(
    scene1 = list(
      column_name = "set2_scene1",
      aoilist = list(
        aoi1 = list(hit_name = "left", x_topleft = 60, y_topleft = 290, x_bottomright = 690, y_bottomright = 1130),
        aoi2 = list(hit_name = "right", x_topleft = 1225, y_topleft = 290, x_bottomright = 1855, y_bottomright = 1130),
        aoi3 = list(hit_name = "top", x_topleft = 790, y_topleft = 10, x_bottomright = 1130, y_bottomright = 350),
        aoi4 = list(hit_name = "bottom", x_topleft = 790, y_topleft = 730, x_bottomright = 1225, y_bottomright = 1070)
      )
    ),
    scene2 = list(
      column_name = "set2_scene2",
      aoilist = list(
        aoi1 = list(hit_name = "top", x_topleft = 790, y_topleft = 10, x_bottomright = 1130, y_bottomright = 350),
        aoi2 = list(hit_name = "bottom", x_topleft = 790, y_topleft = 730, x_bottomright = 1130, y_bottomright = 1070)
      )
    )
  )

  expect_false(is_aoilist_intersecting(aoiset1))
  expect_warning(is_aoilist_intersecting(aoiset2))
})
