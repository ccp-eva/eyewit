is_aoilist_overlapping <- function(aoisets, verbose = FALSE) {
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
            "↔",
            aoisets[[i]]$aoilist[[k]]$hit_name
          ))
          print(paste(
            "Intersecting:",
            is_intersecting(aoisets[[i]]$aoilist[[j]], aoisets[[i]]$aoilist[[k]])
          ))
        }

        # check is_intersecting is true, if so stop
        if (is_intersecting(aoisets[[i]]$aoilist[[j]], aoisets[[i]]$aoilist[[k]])) {
          stop(
            paste(
              "You got intersecting AOIs at the column: ",
              aoisets[[i]]$column_name,", while comparing the two AOIs: ",
              aoisets[[i]]$aoilist[[j]]$hit_name, " and ", aoisets[[i]]$aoilist[[k]]$hit_name,
              sep = ""
            )
          )
        }
      }
    }
  }

  return(FALSE)
}
