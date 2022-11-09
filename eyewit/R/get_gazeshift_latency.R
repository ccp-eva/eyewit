#' Title
#'
#' https://github.com/Kalaschnik/media/blob/main/get_gazeshift_latency.png
#'
#' @param df A dataframe
#' @param aoisets a AOI set
#' @importFrom rlang sym
#'
#' @return None
#' @export
#'

get_gazeshift_latency <- function(df, aoisets) {

  # grab fixation index pairs
  # ... to get the rownumber for a given fi, use: fipair$end[fi]
  fi_pairs <- fi2rn(df$fi)

  # init named list
  latencies <- list(init = NULL)


  # iterate over all aoisets
  for (aoiset in aoisets) {

    curr_colname <- aoiset[["column_name"]]
    for (.list in aoiset[["aoilist"]]) {

      curr_hitname <- .list[["hit_name"]]


      fi_parents <- df %>%
        dplyr::filter(!is.na(fi)) %>%
        dplyr::filter(!!sym(curr_colname) == curr_hitname) %>%
        dplyr::mutate(
          FixationDiff = c(diff(fi), NA)
        ) %>%
        dplyr::filter(FixationDiff == 1) %>%
        dplyr::pull(fi)

      # OUTSIDE CHECK
      # If there is an "outside" fixation/saccade between two consecutive fixations, it will skew
      # the duration. Thus, if the given "outside" label is detected between two consecutive fixations,
      # the corresponding entry will be removed from the latencies and the fi_pairs list
      for (fi_parent in fi_parents) {
        if (is_hitname_in_range(df[[curr_colname]], aoiset$outside_aoi_label, fi_parent, fi_parent + 1)) {

          # remove that element from the fi_parent vector
          fi_parents <- fi_parents[fi_parents != fi_parent]
        }
      }

      # save fi_pairs in latencies list (first value is parent, second is child)
      latencies[[curr_colname]][[curr_hitname]]$fi_pairs <- list(
        fi_parents, fi_parents + 1
      )

      # get diffs
      # get rownumbers in order to get the corresponding RecordingTime
      rownums_parent <- fi_pairs$fiend[latencies[[curr_colname]][[curr_hitname]]$fi_pairs[[1]]]
      rownums_successor <- fi_pairs$fistart[latencies[[curr_colname]][[curr_hitname]]$fi_pairs[[2]]]

      # get RecordingTimestamp for rownums
      recording_timestamp_parent <- df$timestamp[rownums_parent]
      recording_timestamp_successor <- df$timestamp[rownums_successor]

      recording_timetamp_diffs <- recording_timestamp_successor - recording_timestamp_parent

      # save diff in latencies
      latencies[[curr_colname]][[curr_hitname]]$latencies <- recording_timetamp_diffs

      # add to hitnames_median
      latencies[[curr_colname]]$hitnames_median <- c(latencies[[curr_colname]]$hitnames_median, recording_timetamp_diffs)
    }

    # calculate median over all hit_names with curr_colname
    latencies[[curr_colname]]$hitnames_median <- stats::median(latencies[[curr_colname]]$hitnames_median)

    # fill grand median
    latencies$aois_median <- c(latencies[[curr_colname]]$hitnames_median, latencies$aois_median)
  }

  # calculate grand median
  latencies$aois_median <- stats::median(latencies$aois_median)

  # remove placeholder init value
  latencies$init <- NULL

  return(latencies)
}
