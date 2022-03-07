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
#' Returns a tibble respecting all AOI definitions
#
#' ```
#' tribble(
#'   ~FI, ~RowStart, ~RowEnd, ~AOIColA,             ~AOIColN,
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
#'   ~FI, ~RowStart, ~RowEnd, ~AOIColA,             ~AOIColAClean, ~AOIColN,      ~AOIColNClean,
#'   1,   15,        30,      c("left"),            c("left"),     c("onscreen"), NA,
#'   2,   45,        70,      c("right"),           c("right"),    c("onscreen"), NA,
#'   3,   80,        99,      c("left", "outside"), c("left"),     c("onscreen"), NA,
#' )
#' ```
#'
#'
#' ## df, aoisets, trial scope
#' If you provide a trial scopes, the returned df will be trimmed down to respect the row ranges.
#' *NB*, when using trial scopes, the last value for every scope end at **FixationGapDuration** will
#' show NA. This behavior is on purpose. Because if a trial block ends, usually another media
#' elements starts and there is a longer inactive scence, you want to avoid having these larger
#' values in your data. For example, the duration between the last fixation index of scene A and
#' first fixation index of scene B is naturally higher, and should not be evaluated.
#' Since a scene/trial was ending anyways, it is not a good proxy for detecting a look-away
#' during a trial. You can bypass this option by setting `na_trailing_gap_duration` to FALSE.
#'
#' @param df A dataframe containing columns created by [get_aois].
#' @param aoi_sets *[Optional]* A list of AOI sets.
#' @param scope *[Optional]* A trial scope list.
#' @param show_non_hn_labels *[Optional]* TRUE or FALSE (default).
#' @param na_trailing_gap_duration *[Optional]* TRUE (default) or FALSE.
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
                       scope = NA,
                       show_non_hn_labels = FALSE,
                       na_trailing_gap_duration = TRUE) {

  # create tibble based on fi2rn() and map it in a df
  fi_df <- tibble::tibble(.rows = length(.eyewit_utils$fi2rn$fistart))
  fi_df$FI <- seq.int(.eyewit_utils$fi2rn$fistart)
  fi_df$RowStart <- .eyewit_utils$fi2rn$fistart
  fi_df$RowEnd <- .eyewit_utils$fi2rn$fiend


  # if scope and aoisets are missing, return here
  if (missing(scope) && missing(aoisets)) {
    return(fi_df)
  }

  # if aoisets are set but scope is missing, continue here
  if (!missing(aoisets)) {
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
      fi_df$TSStart <- vector("integer", nrow(fi_df))
      fi_df$TSEnd <- vector("integer", nrow(fi_df))
      fi_df$TSEnd1 <- vector("integer", nrow(fi_df)) # The sample after the fi end
      fi_df$FixationDuration <- vector("integer", nrow(fi_df))
      fi_df$FixationGapDuration <- vector("integer", nrow(fi_df))

      # iterate over all fixation indexes of df and copy all values from the current aoi column
      # ... matching the current fixation index from df to fi_df
      for (fi in seq.int(nrow(fi_df))) {
        current_hitnames[[fi]] <-
          df[[current_colname]][which(df$FixationIndex == fi)] |>
          unique()

        fi_df$TSStart[fi] <- df$RecordingTimestamp[which(df$FixationIndex == fi)] |> min()
        fi_df$TSEnd[fi] <- df$RecordingTimestamp[which(df$FixationIndex == fi)] |> max()
        # for end value use the i + 1 sample from the TSEnd to get correct durations (compare with
        # ... GazeEventDuration column)
        fi_df$TSEnd1[fi] <- df$RecordingTimestamp[which(df$FixationIndex == fi) |> max() + 1]
        fi_df$FixationDuration[fi] <- fi_df$TSEnd1[fi] - fi_df$TSStart[fi]


        # Calculate gap duration from between fixation indexes
        ending_times_of_former_fixation <- fi_df$TSEnd1[1:length(fi_df$TSEnd1) - 1]
        starting_times_of_latter_fixation <- fi_df$TSStart[2:length(fi_df$TSStart)]
        fixation_gap_duration <- starting_times_of_latter_fixation - ending_times_of_former_fixation
        fi_df$FixationGapDuration <- c(fixation_gap_duration, NA)


        # if participants stare at the screen, it may happen that there are
        # ... two trials in one fixation index. If that is the case the smaller trial number
        # check if current fi has only NAs in Trials, if so assign NA
        na_trial <-
          df$Trial[which(df$FixationIndex == fi)] |> # get all trials matchinung the current fi
          unique() |> # reduce them to unique values (e.g., NA, 1) or (NA) or (1)
          is.na() |> # check for NA, yields to either (TRUE, FALSE) or (TRUE), (FALSE)
          all() # Check if multiple booleans are all TRUE, this is helpful for the case (TRUE,FALSE)
        if (na_trial) {
          fi_df$Trial[fi] <- NA
          next
        }
        fi_df$Trial[fi] <- df$Trial[which(df$FixationIndex == fi)] |> min(na.rm = TRUE)
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
          fi_df[[clean_colname]][[fi]] <- legit_hitnames[which(legit_hitnames %in% fi_df$AOIActionPhaseBody[[fi]])]
        }

        # check for character 0 and replace with NA (https://stackoverflow.com/a/44766446/2258480)
        fi_df[[clean_colname]] <-
          lapply(fi_df$AOIActionPhaseBodyClean, function(x) if (identical(x, character(0))) NA_character_ else x)
      }
    }
  }

  if (!missing(scope)) {

    # prepare subsetting df
    fi_df_sub <- tibble::tibble()

    for (trial in seq.int(scope$start)) {
      current_scope_start <- scope$start[trial]
      current_scope_end <- scope$end[trial]

      subset_start <- which(fi_df$RowStart <= current_scope_start) |> max()
      # it may be that there no more fi_df$RowEnd indexes which are greater than curren_scope_end
      # ... especially in the last trial. Thus, we use the last Fixation Index
      subset_end <- ifelse(
        any(fi_df$RowEnd >= current_scope_end),
        which(fi_df$RowEnd >= current_scope_end) |> min(),
        fi_df$FI |> max()
      )

      # the current_scope_start must be within:
      # ... fi_df$RowStart[subset_start] and fi_df$RowEnd[subset_start]
      # if that is not the case increment fi by to get an accurate subset
      if (
        !(fi_df$RowStart[subset_start] <= current_scope_start &&
          current_scope_start <= fi_df$RowEnd[subset_start])
      ) {
        subset_start <- subset_start + 1L
      }

      # same logic for current_scope_end...
      if (
        !(fi_df$RowStart[subset_end] <= current_scope_end &&
          current_scope_end <= fi_df$RowEnd[subset_end])
      ) {
        subset_end <- subset_end - 1L
      }

      # remove misleading gap_duration for last trials indexes
      if (na_trailing_gap_duration) {
        fi_df$FixationGapDuration[subset_end] <- NA
      }

      # subset
      fi_df_sub <- rbind(fi_df_sub, fi_df[subset_start:subset_end, ])
    }

    # overwrite fi_df with fi_df_sub if scope was set
    fi_df <- fi_df_sub
  }

  return(fi_df)
}
