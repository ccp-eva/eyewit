# https://github.com/Kalaschnik/media/blob/main/get_gazeshift_latency.png
# requires magrittr and dplyr
get_gazeshift_latency <- function(df, aoisets) {

  # grab fixation index pairs
  # ... to get the rownumber for a given FixationIndex, use: fipair$end[FixationIndex]
  fi_pairs <- get_fixationindex_pairs(df$FixationIndex)

  # init named list
  latencies <- list(init = NULL)


  # iterate over all aoisets
  for (aoiset in aoisets) {

    curr_colname <- aoiset[["column_name"]]
    for (.list in aoiset[["aoilist"]]) {

      curr_hitname <- .list[["hit_name"]]


      fi_parents <- df %>%
        filter(!is.na(FixationIndex)) %>%
        filter(!!sym(curr_colname) == curr_hitname) %>%
        mutate(
          FixationDiff = c(diff(FixationIndex), NA)
        ) %>%
        filter(FixationDiff == 1) %>%
        pull(FixationIndex)

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
      recording_timestamp_parent <- df$RecordingTimestamp[rownums_parent]
      recording_timestamp_successor <- df$RecordingTimestamp[rownums_successor]

      recording_timetamp_diffs <- recording_timestamp_successor - recording_timestamp_parent

      # save diff in latencies
      latencies[[curr_colname]][[curr_hitname]]$latencies <- recording_timetamp_diffs

      # add to hitnames_median
      latencies[[curr_colname]]$hitnames_median <- c(latencies[[curr_colname]]$hitnames_median, recording_timetamp_diffs)
    }

    # calculate median over all hit_names with curr_colname
    latencies[[curr_colname]]$hitnames_median <- median(latencies[[curr_colname]]$hitnames_median)

    # fill grand median
    latencies$aois_median <- c(latencies[[curr_colname]]$hitnames_median, latencies$aois_median)
  }

  # calculate grand median
  latencies$aois_median <- median(latencies$aois_median)

  # remove placeholder init value
  latencies$init <- NULL

  return(latencies)
}
