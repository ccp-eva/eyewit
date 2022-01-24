#' Check if AOIs intersect
#'
#' The first input is the aoisets list as defined in the interface.R file. The function iterates
#' over all defined AOI sets and checks if the individual lists are intersecting.
#'
#' For reporting purposes, this function also requires a `column_name` and `hit_name` for
#' identifying the current set and the current AOI. In each iteration the function is calling
#' [is_intersecting].
#'
#' @family preflight functions
#'
#' @param aoisets An AOI object containing arbitrary AOI sets as defined in interface.R
#' @param verbose Enable verbose output (defaults to FALSE)
#'
#' @return Returns a logical scalar
#' @export
#'
#' @examples
#' # This example is checking two AOI sets. The first AOI set contains 4 AOIs, the second two.
#' # Running this set returns FALS, as all AOIs in each individual list are not overlapping.
#' aoisets_false <- list(
#'   aoiset1 = list(
#'     column_name = "set1",
#'     aoilist = list(
#'       aoi1 = list(hit_name = "left", x_topleft = 60, y_topleft = 290, x_bottomright = 690, y_bottomright = 1130),
#'       aoi2 = list(hit_name = "right", x_topleft = 1225, y_topleft = 290, x_bottomright = 1855, y_bottomright = 1130),
#'       aoi3 = list(hit_name = "top", x_topleft = 790, y_topleft = 10, x_bottomright = 1130, y_bottomright = 350),
#'       aoi4 = list(hit_name = "bottom", x_topleft = 790, y_topleft = 730, x_bottomright = 1130, y_bottomright = 1070)
#'     )
#'   ),
#'
#'   aoiset2 = list(
#'     column_name = "set2",
#'     aoilist = list(
#'       aoi1 = list(hit_name = "top", x_topleft = 790, y_topleft = 10, x_bottomright = 1130, y_bottomright = 350),
#'       aoi2 = list(hit_name = "bottom", x_topleft = 790, y_topleft = 730, x_bottomright = 1130, y_bottomright = 1070)
#'     )
#'   )
#' )
is_aoilist_intersecting <- function(aoisets, verbose = FALSE) {
  for (i in seq_along(aoisets)) {

    if (verbose) print(paste("Checking AOIs in Column:", aoisets[[i]]$column_name))

    number_of_aois_in_current_set <- length(aoisets[[i]]$aoilist)
    for (j in 1:number_of_aois_in_current_set) {

      for (k in 1:number_of_aois_in_current_set) {

        # skip iteration to avoid comparing rectangles with themselves
        if (j == k) next

        if (verbose) {
          print(paste(
            "Comparing AOIs: ",
            aoisets[[i]]$aoilist[[j]]$hit_name,
            "\U2194",
            aoisets[[i]]$aoilist[[k]]$hit_name
          ))
          print(paste(
            "Intersecting:",
            is_intersecting(aoisets[[i]]$aoilist[[j]], aoisets[[i]]$aoilist[[k]])
          ))
        }

        # check is_intersecting is true, if so stop
        if (is_intersecting(aoisets[[i]]$aoilist[[j]], aoisets[[i]]$aoilist[[k]])) {
          warning(
            paste(
              "You got intersecting AOIs at the column: ",
              aoisets[[i]]$column_name,", while comparing the two AOIs: ",
              aoisets[[i]]$aoilist[[j]]$hit_name, " and ", aoisets[[i]]$aoilist[[k]]$hit_name,
              sep = ""
            )
          )
          return(TRUE)
        }
      }
    }
  }

  return(FALSE)
}
