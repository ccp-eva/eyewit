# load eyewit
# library(eyewit)

# install CRAN packages
lapply(c("tidyverse", "styler", "lintr"), require, character.only = TRUE)

# import user interface
source("interface.R")

# read raw data filenames
participants <- list.files(interface$raw_dir)

# take a random sample from raw folder to determine vendor labels, and only read headers
sample <- readr::read_tsv(file.path(interface$raw_dir, sample(participants, 1)), col_types = readr::cols(), n_max = 0)
(vendor <- vendor_check(sample))

# incomplete subjects (i.e., not having 2 pretest & 12 test trials)
incomplete_subjets <- c()

# Loop over all participants
for (subject in participants) {

  print(subject)

  # remove later
  # subject <- participants[1]

  # read tsv files
  df_raw <- readr::read_tsv(file.path(interface$raw_dir, subject), col_types = readr::cols())

  # run preflight checks & diagnostics, returns a lean df
  df <- preflight(df_raw, interface)

  # get start and end index pairs for inter_trial chunks
  startend_pretest_action <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[1])
  startend_pretest_outcome <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[2])
  startend_test_action <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[3])
  startend_test_outcome <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[4])

  # test if subject has consistent start/end indexes and get check trial count to match 2 & 12
  if (
    get_trial_count(c(startend_pretest_action, startend_pretest_outcome)) != 2 &&
      get_trial_count(c(startend_test_action, startend_test_outcome)) != 12
  ) {
    incomplete_subjets <- c(incomplete_subjets, subject)
    stop("Bad Trial count")
  }

  # track current trials
  current_pretest_trials <- get_trial_count(c(startend_pretest_action, startend_pretest_outcome))
  current_test_trials <- get_trial_count(c(startend_test_action, startend_test_outcome))


  # Allocate Trials and fill-up eventValue
  df <- allocate_trials(df, c(startend_pretest_action, startend_pretest_outcome), 2)
  df <- allocate_trials(df, c(startend_test_action, startend_test_outcome), 2, reset_to_1 = TRUE)

  # track video names
  names_test_action <- df$eventValue[startend_test_action$start] |>
    unique() |>
    as.character()

  names_test_outcome <- df$eventValue[startend_test_outcome$start] |>
    unique() |>
    as.character()

  # Insert AOI Columns
  df <- tibble::add_column(df, "{interface$aoisets$actionphasebody$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$actionphasebody, startend_test_action), .before = 1)

  df <- tibble::add_column(df, "{interface$aoisets$actionphaseface$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$actionphaseface, startend_test_action), .after = 1)

  df <- tibble::add_column(df, "{interface$aoisets$outcomephase$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$outcomephase, startend_test_outcome), .after = 2)

  df <- tibble::add_column(df, "{interface$aoisets$screen$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$screen), .after = 3)

  # helper variable
  fi_pairs <- fi2rn(df$FixationIndex)
  # Get inner AOI gaze shift latencies (used in get_looks implicitly for given)
  gazeshifts <- get_gazeshift_latency(df, interface$aoisets)
  # get detailed information about single fixation indexes (trial-scoped)
  # fi_summary_overal <- fi_summary(df, interface$aoisets, show_non_hn_labels = TRUE)
  # fi_summary_test_action <- fi_summary(df, interface$aoisets, startend_test_action, TRUE)
  # fi_summary_test_outcome <- fi_summary(df, interface$aoisets, startend_test_outcome, TRUE)


  ##################################################################################################
  # Initialize empty subject tibble (the better data.frame)
  df_subject <- tibble::tibble(.rows = current_test_trials)

  # Build Summary table
  # ================================================================================================
  # NAME INFORMATIONS
  # ------------------------------------------------------------------------------------------------
  df_subject$ID <- value_parser_by_key(interface$keys_filename, subject)$id
  df_subject$Sex <- value_parser_by_key(interface$keys_filename, subject)$sex
  df_subject$Age_Days <- value_parser_by_key(interface$keys_filename, subject)$age_days
  df_subject$Exp <- value_parser_by_key(interface$keys_filename, subject, trim_right = 4)$experiment
  df_subject$Rec <- value_parser_by_key(interface$keys_filename, subject)$rec


  df_subject$TestPhase <- value_parser_by_key(interface$keys_videoname, names_test_outcome)$test_phase
  df_subject$ConSoc <- value_parser_by_key(interface$keys_videoname, names_test_outcome)$con_soc
  df_subject$ConOut <- value_parser_by_key(interface$keys_videoname, names_test_outcome)$con_object_change

  # helper for Condition column
  soc_out_vec <- vector(mode = "integer", length = current_test_trials)
  for (ci in 1:current_test_trials) {
    soc_out_vec[ci] <- interface$condition_soc_out_mapping[[paste0(df_subject$ConSoc[ci], df_subject$ConOut[ci])]]
  }
  df_subject$Condition <- soc_out_vec

  df_subject$Dyad <- value_parser_by_key(interface$keys_videoname, names_test_outcome)$dyad
  df_subject$Object <- value_parser_by_key(interface$keys_videoname, names_test_outcome, trim_right = 4)$object_id
  df_subject$ObjectPos <- value_parser_by_key(interface$keys_videoname, names_test_outcome, trim_right = 4)$object_position
  df_subject$ObjectPosAct <- value_parser_by_key(interface$keys_videoname, names_test_action, trim_right = 4)$object_position

  df_subject$TrialRun <- 1:current_test_trials
  df_subject$TrialCon <- c(rep(1, current_test_trials / 2), rep(2, current_test_trials / 2))


  df_subject$OutcomeDuration <-
    df$timestamp[startend_test_outcome$end] - df$timestamp[startend_test_outcome$start + 1]

  df_subject$TwoSecCheck <- get_looks(
    df = df,
    aoi_collection = interface$aoisets$screen,
    scope = startend_test_outcome,
    intra_scope_window = c(120, "end"),
    lookaway_stop = 2000, TRUE)$lookaway_collection$onscreen$lookaway_stop_applied

  # ------------------------------------------------------------------------------------------------
  # Looking Times - OUTCOME
  # ------------------------------------------------------------------------------------------------

  df_subject$TotalLTScreenOut <-
    get_looks(df, interface$aoisets$screen, startend_test_outcome, c(120, "end"), omit_first_overflow_fi = TRUE)$looking_times


  df_subject$TotalLTObjectOut <- dplyr::if_else(
    df_subject$ObjectPos == "OBEN",
    get_looks(df, interface$aoisets$outcomephase, startend_test_outcome, c(120, "end"), omit_first_overflow_fi = TRUE)$looking_times$top,
    get_looks(df, interface$aoisets$outcomephase, startend_test_outcome, c(120, "end"), omit_first_overflow_fi = TRUE)$looking_times$bottom
  )

  df_subject$LTScreenOut <-
    get_looks(
      df = df,
      aoi_collection = interface$aoisets$screen,
      scope = startend_test_outcome,
      intra_scope_window = c(120, "end"),
      lookaway_stop = 2000,
      omit_first_overflow_fi = TRUE)$lookaway_collection$onscreen$durations

  df_subject$LTObjectOut <- dplyr::if_else(
    df_subject$ObjectPos == "OBEN",
    get_looks(df, interface$aoisets$outcomephase, startend_test_outcome, c(120, "end"), 2000, TRUE)$lookaway_collection$top$durations,
    get_looks(df, interface$aoisets$outcomephase, startend_test_outcome, c(120, "end"), 2000, TRUE)$lookaway_collection$bottom$durations
  )

  df_subject$FirstLookDurationObjectOutTop <-
    get_looks(
      df = df,
      aoi_collection = interface$aoisets$outcomephase,
      scope = startend_test_outcome,
      intra_scope_window = c(120, "end"),
      omit_first_overflow_fi = TRUE,
      first_look_emergency_cutoff =
        round(
          median(gazeshifts$AOIOutcomePhase$top$latencies) +
          3 * sd(gazeshifts$AOIOutcomePhase$top$latencies)
        )
      )$first_looks_collection$top$durations

  df_subject$FirstLookDurationObjectOutBottom <-
    get_looks(
      df = df,
      aoi_collection = interface$aoisets$outcomephase,
      scope = startend_test_outcome,
      intra_scope_window = c(120, "end"),
      omit_first_overflow_fi = TRUE,
      first_look_emergency_cutoff =
        round(
          median(gazeshifts$AOIOutcomePhase$bottom$latencies) +
            3 * sd(gazeshifts$AOIOutcomePhase$bottom$latencies)
        )
    )$first_looks_collection$bottom$durations

  df_subject$FirstLookDurationObjectOut <- dplyr::if_else(
    df_subject$ObjectPos == "OBEN",
    df_subject$FirstLookDurationObjectOutTop,
    df_subject$FirstLookDurationObjectOutBottom
  )

  df_subject$FirstLookDurationObjectOutTopReason <-
    get_looks(
      df = df,
      aoi_collection = interface$aoisets$outcomephase,
      scope = startend_test_outcome,
      intra_scope_window = c(120, "end"),
      omit_first_overflow_fi = TRUE,
      first_look_emergency_cutoff =
        round(
          median(gazeshifts$AOIOutcomePhase$top$latencies) +
            3 * sd(gazeshifts$AOIOutcomePhase$top$latencies)
        )
    )$first_looks_collection$top$ending_reason

  df_subject$FirstLookDurationObjectOutBottomReason <-
    get_looks(
      df = df,
      aoi_collection = interface$aoisets$outcomephase,
      scope = startend_test_outcome,
      intra_scope_window = c(120, "end"),
      omit_first_overflow_fi = TRUE,
      first_look_emergency_cutoff =
        round(
          median(gazeshifts$AOIOutcomePhase$top$latencies) +
            3 * sd(gazeshifts$AOIOutcomePhase$top$latencies)
        )
    )$first_looks_collection$bottom$ending_reason

  df_subject$FirstLookDurationObjectOutReason <- dplyr::if_else(
    df_subject$ObjectPos == "OBEN",
    df_subject$FirstLookDurationObjectOutTopReason,
    df_subject$FirstLookDurationObjectOutBottomReason
  )

  df_subject$FirstLookDurationScreen <-
    get_looks(
      df = df,
      aoi_collection = interface$aoisets$screen,
      scope = startend_test_outcome,
      intra_scope_window = c(120, "end"),
      omit_first_overflow_fi = TRUE,
      first_look_emergency_cutoff =
        round(
          median(gazeshifts$AOIScreen$onscreen$latencies) +
            3 * sd(gazeshifts$AOIScreen$onscreen$latencies)
        )
    )$first_looks_collection$onscreen$durations

  # ------------------------------------------------------------------------------------------------
  # Looking Times - ACTION
  # ------------------------------------------------------------------------------------------------

  df_subject$TotalLTScreenAct <-
    get_looks(df, interface$aoisets$screen, startend_test_action, c(3201, 18200), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$TotalLTObjectAct <- dplyr::if_else(
    df_subject$ObjectPos == "OBEN",
    get_looks(df, interface$aoisets$actionphasebody, startend_test_action, c(3201, 18200), omit_first_overflow_fi = TRUE)$looking_times$top,
    get_looks(df, interface$aoisets$actionphasebody, startend_test_action, c(3201, 18200), omit_first_overflow_fi = TRUE)$looking_times$bottom
  )

  df_subject$TotalLTActorLeftAct <-
    get_looks(df, interface$aoisets$actionphasebody, startend_test_action, c(3201, 18200), omit_first_overflow_fi = TRUE)$looking_times$left

  df_subject$TotalLTActorRightAct <-
    get_looks(df, interface$aoisets$actionphasebody, startend_test_action, c(3201, 18200), omit_first_overflow_fi = TRUE)$looking_times$right

  df_subject$TotalLTActorSumAct <- df_subject$TotalLTActorLeftAct + df_subject$TotalLTActorRightAct

  df_subject$TotalLTFaceLeftAct <-
    get_looks(df, interface$aoisets$actionphaseface, startend_test_action, c(3201, 18200), omit_first_overflow_fi = TRUE)$looking_times$left

  df_subject$TotalLTFaceRightAct <-
    get_looks(df, interface$aoisets$actionphaseface, startend_test_action, c(3201, 18200), omit_first_overflow_fi = TRUE)$looking_times$right

  df_subject$TotalLTFaceSumAct <- df_subject$TotalLTFaceLeftAct + df_subject$TotalLTFaceRightAct

  # ------------------------------------------------------------------------------------------------
  # Looking Times - INCLUSION ACTION
  # ------------------------------------------------------------------------------------------------

  df_subject$LTScreenAct_5to6 <-
    get_looks(df, interface$aoisets$screen, startend_test_action, c(5201, 6200), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenAct_7to8 <-
    get_looks(df, interface$aoisets$screen, startend_test_action, c(7201, 8200), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenAct_9to10 <-
    get_looks(df, interface$aoisets$screen, startend_test_action, c(9201, 10200), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenAct_11to12 <-
    get_looks(df, interface$aoisets$screen, startend_test_action, c(11201, 12200), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenAct_13to14 <-
    get_looks(df, interface$aoisets$screen, startend_test_action, c(13201, 14200), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenAct_15to16 <-
    get_looks(df, interface$aoisets$screen, startend_test_action, c(15201, 16200), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$InterPhaseCheckerSoc <-
    dplyr::if_else(
      df_subject$LTScreenAct_5to6 > 0 |
      df_subject$LTScreenAct_9to10 > 0 |
      df_subject$LTScreenAct_13to14 > 0,
      TRUE, FALSE
    )

  df_subject$InterPhaseCheckerGazing <-
    dplyr::if_else(
      df_subject$LTScreenAct_7to8 > 0 |
        df_subject$LTScreenAct_11to12 > 0 |
        df_subject$LTScreenAct_15to16 > 0,
      TRUE, FALSE
    )

  df_subject$InterPhaseCheckerValid <- df_subject$InterPhaseCheckerSoc & df_subject$InterPhaseCheckerGazing

  # write tables for individual participants
  # replace with readr functions ones this is clear: https://github.com/tidyverse/readr/issues/1388
  write.table(df_subject, paste0(interface$output_dir, subject), sep = '\t', row.names = FALSE)
}

# Read in tsv files from pre-processing folder
tsv_files <- list.files(interface$output_dir, full.names = TRUE)

# Creating data frame
overall.data <- tsv_files %>%
  map(read_tsv) %>%    # read in all the files individually, using the function read_tsv() from the readr package
  reduce(rbind)        # reduce with rbind into one dataframe
