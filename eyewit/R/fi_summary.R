#' Fixation Index Summary
#'
#' Given a df and, optionally, a set of AOI definitions, this function returns a df providing a
#' summary for all fixation indexes. Specifically, for every fixation index you will get the row
#' name range, timestamps, fixation durations, and the information which AOI was hit at the given
#' fixation index. See Arguments Details section further down for detailed explanation.
#'
#' @details # Argument Details
#' Depending on the arguments you provide, the returned df varies as such:
#'
#' ## df (mandatory)
#' If you only provide a df, this function is identical to [fi2rn]. It is thus cheaper to call
#' [fi2rn], if you only need to know the row range for a given fixation index.
#'
#' ## df, aoisets
#' Returns a tibble respecting all AOI definitions (or justa list depending on your input)
#
#' ```
#' tribble(
#'   ~FI, ~FIrnS, ~FIrnE, ~AOIColA,             ~AOIColN,
#'   1,   15,        30,      c("left"),            c("onscreen"),
#'   2,   45,        70,      c("right"),           c("onscreen"),
#'   3,   80,        99,      c("left", "outside"), c("onscreen"),
#' )
#' ```
#'
#' ## df, aoisets, show_non_hn_labels = TRUE
#' Returns a tibble respecting all AOI definitions, and includes a clean-column
#' without non-hitname labels. Non-hitname labels are defined in the `interface.R` file. Look for:
#' `no_evaluation_label`, `missing_coordinate_label`, and `outside_aoi_label`.
#' Compare this tribble with the previous one:
#'
#' ```
#' tribble(
#'   ~FI, ~FIrnS, ~FIrnE, ~AOIColA,             ~AOIColAClean, ~AOIColN,      ~AOIColNClean,
#'   1,   15,        30,      c("left"),            c("left"),     c("onscreen"), NA,
#'   2,   45,        70,      c("right"),           c("right"),    c("onscreen"), NA,
#'   3,   80,        99,      c("left", "outside"), c("left"),     c("onscreen"), NA,
#' )
#' ```
#'
#'
#' ## df, aoisets, trial range
#' If you provide a trial range, the returned df will be trimmed down to respect the row ranges. It
#' will use have an additional column `FGapDurTrlEnd` which is similar to `FGapDur`; yet, this
#' column will respect the trial range end in the calculation for the duration. For example,
#' if the fixation index 420 is the last one in trail 7, `FGapDur` will show the time difference
#' from 420 to 421. In contrast, `FGapDurTrlEnd` will show the time difference from 420 to the
#' trialrng end time (e.g., "MovieEnd"). Thus, `FGapDurTrlEnd` is naturally smaller than `FGapDur`.
#'
#' @param df A dataframe containing columns created by [get_aois].
#' @param aoisets *(Optional)* A list of all AOI sets, or a single AOI definition.
#' @param trialrange *(Optional)* A trial range (trlr) list.
#' @param show_non_hn_labels *(Optional)* TRUE or FALSE (default).
#'
#' @return Returns a dataframe (tibble) providing AOI information for all fixation indexes
#' @export
#'
#' @examples
#' \dontrun{
#' # Returns a df respecting the aoi definitions from the interface
#' fi_summary(df, aoisets)
#' }
fi_summary <- function(df,
                       aoisets = NA,
                       trialrange = NA,
                       show_non_hn_labels = FALSE) {

  # create tibble based on fi2rn() and map it in a df
  fi_df <- tibble::tibble(.rows = length(.eyewit_utils$fi2rn$fistart))
  fi_df$FI <- seq.int(.eyewit_utils$fi2rn$fistart)
  fi_df$FIrnS <- .eyewit_utils$fi2rn$fistart
  fi_df$FIrnE <- .eyewit_utils$fi2rn$fiend


  # if trialrange and aoisets are missing, return here
  if (missing(trialrange) && missing(aoisets)) {
    return(fi_df)
  }

  # if aoisets are set but trialrange is missing, continue here
  if (!missing(aoisets)) {

    # if a single AOI definition was provided, wrap set structure around it
    # check if aoisets has a "aoilist" key (name), if so it is a single aoi list
    if ("aoilist" %in% names(aoisets)) {
      aoisets <- list(aoidef = aoisets)
    }

    for (i in seq.int(aoisets)) {
      current_colname <- aoisets[[i]]$column_name

      # initialize list to store hit names in current AOI and fixation index
      # Note: We use a list since in rare cases multiple disjunct AOIs still can share the same fi,
      # which results in multiple entries; thus a list! See also:
      # ... bad_fixation_indexes variable and check_multiple_hit_names_in_same_fi function
      current_hitnames <- vector("list", nrow(fi_df))

      # add Trial column
      fi_df$Trial <- vector("integer", nrow(fi_df))

      # add columns for time tracking
      fi_df$FIrtsS <- vector("integer", nrow(fi_df)) # Fixation Index Recording Timestamp Start
      fi_df$FIrtsE <- vector("integer", nrow(fi_df)) # Fixation Index Recording Timestamp End
      fi_df$FIrtsE1 <- vector("integer", nrow(fi_df)) # The sample after FIrtsE
      fi_df$FDur <- vector("integer", nrow(fi_df))
      fi_df$FGapDur <- vector("integer", nrow(fi_df))

      # iterate over all fixation indexes of df and copy all values from the current aoi column
      # ... matching the current fixation index from df to fi_df
      for (fi in seq.int(nrow(fi_df))) {

        current_hitnames[[fi]] <-
          df[[current_colname]][which(df$fi == fi)] |>
          unique()

        fi_df$FIrtsS[fi] <- df$timestamp[which(df$fi == fi)] |> min()
        fi_df$FIrtsE[fi] <- df$timestamp[which(df$fi == fi)] |> max()
        # for end value use the i + 1 sample from the FIrtsE to get correct durations (compare with
        # ... GazeEventDuration column)
        fi_df$FIrtsE1[fi] <- df$timestamp[which(df$fi == fi) |> max() + 1]
        fi_df$FDur[fi] <- fi_df$FIrtsE1[fi] - fi_df$FIrtsS[fi]


        # Calculate gap duration between fixation indexes
        ending_times_of_former_fixation <- fi_df$FIrtsE1[1:length(fi_df$FIrtsE1) - 1]
        starting_times_of_latter_fixation <- fi_df$FIrtsS[2:length(fi_df$FIrtsS)]
        fixation_gap_duration <- starting_times_of_latter_fixation - ending_times_of_former_fixation
        fi_df$FGapDur <- c(fixation_gap_duration, NA)


        # if participants stare at the screen, it may happen that there are
        # ... two trials in one fixation index. If that is the case the smaller trial number
        # check if current fi has only NAs in Trials, if so assign NA
        na_trial <-
          df$Trial[which(df$fi == fi)] |> # get all trials matchinung the current fi
          unique() |> # reduce them to unique values (e.g., NA, 1) or (NA) or (1)
          is.na() |> # check for NA, yields to either (TRUE, FALSE) or (TRUE), (FALSE)
          all() # Check if multiple booleans are all TRUE, this is helpful for the case (TRUE,FALSE)
        if (na_trial) {
          fi_df$Trial[fi] <- NA
          next
        }

        fi_df$Trial[fi] <- df$Trial[which(df$fi == fi)] |> min(na.rm = TRUE)
      }


      # attach current_hitnames list to fi_df
      fi_df[[current_colname]] <- current_hitnames


      if (show_non_hn_labels) {
        # create new "clean" column next current column
        clean_colname <- paste0(current_colname, "Clean")
        fi_df[[clean_colname]] <- fi_df[[current_colname]]


        # collect all possible legit hit names in current AOI
        legit_hitnames <- c()
        for (aoi_item in aoisets[[i]]$aoilist) {
          legit_hitnames <- c(legit_hitnames, aoi_item$hit_name)
        }

        # remove meta labels for outside, no fixation data, and no eval range
        # specifically remove all entries that are not legit_hitnames
        for (fi in seq.int(nrow(fi_df))) {
          fi_df[[clean_colname]][[fi]] <-
            legit_hitnames[which(legit_hitnames %in% fi_df[[clean_colname]][[fi]])]
        }

        # check for character 0 and replace with NA (https://stackoverflow.com/a/44766446/2258480)
        fi_df[[clean_colname]] <-
          lapply(fi_df[[clean_colname]],
                 function(x) if (identical(x, character(0))) NA_character_ else x)
      }
    }
  }

  if (!missing(trialrange)) {

    # add fixation gap duration column which is sensitive to trialrange end
    # init it with the contents fo FGapDur
    fi_df <- tibble::add_column(fi_df, FGapDurTrlEnd = fi_df$FGapDur, .after = "FGapDur")

    # add fixation duration column which is sensitive to beginning and endings of trial ranges
    # That is overlapping fixation indexes are trimmed to respect the trial range.
    # This is identical to get_looks’ intra_scope_cut
    fi_df <- tibble::add_column(fi_df, FDurTrlTrimmed = fi_df$FDur, .after = "FDur")

    # prepare subsetting df
    fi_df_sub <- tibble::tibble()

    for (trial in seq.int(trialrange$start)) {

      current_trialrange_start <- trialrange$start[trial]
      current_trialrange_end <- trialrange$end[trial]

      subset_start <- which(fi_df$FIrnS <= current_trialrange_start) |> max()
      # it may be that there no more fi_df$FIrnE indexes greater than curren_trialrange_end
      # ... especially in the last trial. Thus, we use the last Fixation Index
      subset_end <- ifelse(
        any(fi_df$FIrnE >= current_trialrange_end),
        which(fi_df$FIrnE >= current_trialrange_end) |> min(),
        fi_df$FI |> max()
      )

      # the current_trialrange_start must be smaller/larger compared to fi_df$FIrnE/S
      # ... this makes sure to get the correct trial range for subsetting!
      if (!(current_trialrange_start <= fi_df$FIrnE[subset_start])) {
        subset_start <- subset_start + 1L
      }

      # same logic for current_trialrange_end...
      if (!(fi_df$FIrnS[subset_end] <= current_trialrange_end)) {
        subset_end <- subset_end - 1L
      }

      # overwrite fixation gap duration trial end for last entry
      # the condition is necessary for fixations hitting or exceed the MovieEnd Index and would
      # create negative gaps. If that is the case we can assign 0, because there was litterally
      # no gap from the end of the fixation to the end of the trial
      fi_df$FGapDurTrlEnd[subset_end] <- ifelse(
        df$timestamp[current_trialrange_end] - fi_df$FIrtsE1[subset_end] < 0,
        0,
        df$timestamp[current_trialrange_end] - fi_df$FIrtsE1[subset_end]
      )

      # subset
      fi_df_sub <- rbind(fi_df_sub, fi_df[subset_start:subset_end, ])

    } # loop end

    # overwrite fi_df with fi_df_sub if trialrange was set
    fi_df <- fi_df_sub


    # check if there are falsy leading and trailing durations
    trimmed_durations <- get_trimmed_fixation_durations(df, trialrange)
    if (FALSE %in% is.na(trimmed_durations$leading_durations)) {
      # iterate over leading and trailing positions and replace corresponding values in FDurTrlTrimmed
      for (trialpos in trimmed_durations$leading_trial_positions) {
        fi_df$FDurTrlTrimmed[which(fi_df$Trial == trialpos) |> min()] <-
          trimmed_durations$leading_durations[trialpos]
      }

      for (trialpos in trimmed_durations$trailing_trial_positions) {
        fi_df$FDurTrlTrimmed[which(fi_df$Trial == trialpos) |> max()] <-
          trimmed_durations$trailing_durations[trialpos]
      }
    }

    # Add Columns for Trial rn Start/End for comparison with FIrnS/E
    fi_df <- tibble::add_column(fi_df, TrlrnS = fi_df$Trial, .after = "Trial")
    fi_df <- tibble::add_column(fi_df, TrlrnE = fi_df$Trial, .after = "TrlrnS")
    # fill values based on provided trialrange
    fi_df$TrlrnS <- trialrange$start[fi_df$TrlrnS]
    fi_df$TrlrnE <- trialrange$end[fi_df$TrlrnE]

    # Add trial recording timestamps for reference
    fi_df <- tibble::add_column(fi_df, TrlrtsS = df$timestamp[fi_df$TrlrnS], .after = "TrlrnE")
    fi_df <- tibble::add_column(fi_df, TrlrtsE = df$timestamp[fi_df$TrlrnE], .after = "TrlrtsS")
  }

  return(fi_df)
}
