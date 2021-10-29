rm(list = ls(all.names = TRUE)) # Clear workspace
graphics.off() # close all open graphics

# import user interface
source("interface.R")

# install packages
lapply(interface$dependency_list, require, character.only = TRUE)


# import utility functions
list.files("util", "*.R$", full.names = TRUE, ignore.case = TRUE) %>% sapply(source)


# read raw data filenames
participants <- list.files(interface$raw_dir)

# incomplete subjects (i.e., not having 2 pretest & 12 test trials)
incomplete_subjets <- c()

# Loop over all participants
for (subject in participants) {

  # remove later
  subject <- participants[1]

  # read tsv files
  df_raw <- read_tsv(file.path(interface$raw_dir, subject))

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


  # Allocate Trials and Fillup StudioEventData Label
  df <- allocate_trials(df, c(startend_pretest_action, startend_pretest_outcome), 2)
  df <- allocate_trials(df, c(startend_test_action, startend_test_outcome), 2, reset_to_1 = TRUE)

  # track video names
  names_test_action <- df$StudioEventData[startend_test_action$start] %>%
    unique() %>%
    as.character()

  names_test_outcome <- df$StudioEventData[startend_test_outcome$start] %>%
    unique() %>%
    as.character()

  # Insert AOI Columns
  df <- add_column(df, "{interface$aoisets$actionphasebody$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$actionphasebody, startend_test_action), .before = 1)

  df <- add_column(df, "{interface$aoisets$actionphaseface$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$actionphaseface, startend_test_action), .after = 1)

  df <- add_column(df, "{interface$aoisets$outcomephase$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$outcomephase, startend_test_outcome), .after = 2)

  df <- add_column(df, "{interface$aoisets$screen$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$screen), .after = 3)

  ##################################################################################################
  # Initialize empty subject tibble (the better data.frame)
  df_subject <- tibble(.rows = current_test_trials)

  # Build Summary table
  # ================================================================================================
  # NAME INFORMATIONS
  # ------------------------------------------------------------------------------------------------
  df_subject$ID <- value_parser_by_key(interface$keys_filename, subject)$id
  df_subject$Trial <- 1:current_test_trials
  df_subject$TrialCon <- c(rep(1, 6), rep(2, 6))
  df_subject$Sex <- value_parser_by_key(interface$keys_filename, subject)$sex
  df_subject$Age_Days <- value_parser_by_key(interface$keys_filename, subject)$age_days
  df_subject$Rec <- value_parser_by_key(interface$keys_filename, subject)$rec
  df_subject$Exp <- value_parser_by_key(interface$keys_filename, subject, trim_right = 4)$experiment


  # ------------------------------------------------------------------------------------------------
  # Looking Times
  # ------------------------------------------------------------------------------------------------
  df_subject$TotalLTScreenOut <-
    get_looks(df, interface$aoisets$screen, startend_test_outcome, c(120, "end"))$looking_times






  # OVERALL SCREEN (no specific AOI): Looking times over total duration of the video
  df_subject$Overall_LT_ScreenFam <- get_looks(
    df,
    aoi_screen,
    startend_test
  )$looking_times

  df_subject$Overall_LT_ScreenPrefLook <- get_looks(
    df,
    aoi_screen,
    preflook_startend
  )$looking_times

  df_subject$Inter_PropOverall_LT_Screen <- ifelse(
    df_subject$Overall_LT_ScreenFam <= 11000,
    df_subject$Overall_LT_ScreenFam / 11000,
    df_subject$Overall_LT_ScreenFam / max(df_subject$Overall_LT_ScreenFam)
  )

  # OVERALL SCREEN (no specific AOI): Looking times only in INTEARCTION PHASES
  # ... (in the beginning and ending of the video sequence)
  df_subject$InterPhase_LT_Soc_Begin <- get_looks(
    df,
    aoi_screen,
    startend_test,
    c(2000, 3000)
  )$looking_times

  df_subject$InterPhase_LT_Soc_End <- get_looks(
    df,
    aoi_screen,
    startend_test,
    c(10000, 11000)
  )$looking_times

  df_subject$InterPhase_LT_Soc_Total <-
    df_subject$InterPhase_LT_Soc_Begin + df_subject$InterPhase_LT_Soc_End

  # OVERALL SCREEN (no specific AOI): Looking times only in GAZING PHASE
  df_subject$InterPhase_LT_Gazing <- get_looks(
    df,
    aoi_screen,
    startend_test,
    c(4000, 9000)
  )$looking_times

  # AOIs: Looking times over TOTAL duration of the video
  df_subject$Inter_LT_ActorLeft <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test
  )$looking_times$left

  df_subject$Inter_LT_ActorRight <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test
  )$looking_times$right

  df_subject$Inter_LT_ActorsTotal <- df_subject$Inter_LT_ActorLeft + df_subject$Inter_LT_ActorRight

  df_subject$Inter_LT_Actors_PROP <- df_subject$Inter_LT_ActorsTotal / df_subject$Overall_LT_ScreenFam

  df_subject$Inter_LT_FaceLeft <- get_looks(
    df,
    aoi_fam_face,
    startend_test
  )$looking_times$left

  df_subject$Inter_LT_FaceRight <- get_looks(
    df,
    aoi_fam_face,
    startend_test
  )$looking_times$right

  df_subject$Inter_LT_FacesTotal <- df_subject$Inter_LT_FaceLeft + df_subject$Inter_LT_FaceRight

  df_subject$Inter_LT_Faces_PROP <- df_subject$Inter_LT_FacesTotal / df_subject$Overall_LT_ScreenFam

  df_subject$Inter_LT_Object <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test
  )$looking_times$center

  df_subject$Inter_LT_FacesObject_PROP <-
    df_subject$Inter_LT_FacesTotal / (df_subject$Inter_LT_FacesTotal + df_subject$Inter_LT_Object)

  df_subject$Inter_LT_Object_PROP <- df_subject$Inter_LT_Object / df_subject$Overall_LT_ScreenFam

  # AOIs: Looking times in INTERACTION PHASES
  df_subject$InterPhase_LT_FaceLeft_Soc_Begin <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(2000, 3000)
  )$looking_times$left

  df_subject$InterPhase_LT_FaceRight_Soc_Begin <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(2000, 3000)
  )$looking_times$right

  df_subject$InterPhase_LT_FaceTotal_Soc_Begin <-
    df_subject$InterPhase_LT_FaceLeft_Soc_Begin + df_subject$InterPhase_LT_FaceRight_Soc_Begin

  df_subject$InterPhase_LT_FaceTotal_Soc_Begin_PROP <-
    df_subject$InterPhase_LT_FaceTotal_Soc_Begin / df_subject$InterPhase_LT_Soc_Begin

  df_subject$InterPhase_LT_FaceLeft_Soc_End <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(10000, 11000)
  )$looking_times$left

  df_subject$InterPhase_LT_FaceRight_Soc_End <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(10000, 11000)
  )$looking_times$right

  df_subject$InterPhase_LT_FaceTotal_Soc_End <-
    df_subject$InterPhase_LT_FaceRight_Soc_End + df_subject$InterPhase_LT_FaceLeft_Soc_End

  df_subject$InterPhase_LT_FaceTotal_Soc_End_PROP <-
    df_subject$InterPhase_LT_FaceTotal_Soc_End / df_subject$InterPhase_LT_Soc_End

  df_subject$InterPhase_LT_FaceLeft_Soc_Total <-
    df_subject$InterPhase_LT_FaceLeft_Soc_Begin + df_subject$InterPhase_LT_FaceLeft_Soc_End

  df_subject$InterPhase_LT_FaceRight_Soc_Total <-
    df_subject$InterPhase_LT_FaceRight_Soc_Begin + df_subject$InterPhase_LT_FaceRight_Soc_End

  df_subject$InterPhase_LT_FaceTotal_Soc_Total <-
    df_subject$InterPhase_LT_FaceTotal_Soc_Begin + df_subject$InterPhase_LT_FaceTotal_Soc_End

  df_subject$InterPhase_LT_FaceTotal_Soc_Total_PROP <-
    df_subject$InterPhase_LT_FaceTotal_Soc_Total / df_subject$InterPhase_LT_Soc_Total

  df_subject$InterPhase_LT_Object_Soc_Begin <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test,
    c(2000, 3000)
  )$looking_times$center

  df_subject$InterPhase_LT_Object_Soc_Begin_PROP <-
    df_subject$InterPhase_LT_Object_Soc_Begin / df_subject$InterPhase_LT_Soc_Total

  df_subject$InterPhase_LT_Object_Soc_End <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test,
    c(10000, 11000)
  )$looking_times$center

  df_subject$InterPhase_LT_Object_Soc_End_PROP <-
    df_subject$InterPhase_LT_Object_Soc_End / df_subject$InterPhase_LT_Soc_Total

  df_subject$InterPhase_LT_Object_Soc_Total <-
    df_subject$InterPhase_LT_Object_Soc_Begin + df_subject$InterPhase_LT_Object_Soc_End

  df_subject$InterPhase_LT_Object_Soc_Total_PROP <-
    df_subject$InterPhase_LT_Object_Soc_Total / df_subject$InterPhase_LT_Soc_Total

  # AOIs: Looking times in GAZING PHASE
  df_subject$InterPhase_LT_FaceLeft_Gazing <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(4000, 9000)
  )$looking_times$left

  df_subject$InterPhase_LT_FaceRight_Gazing <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(4000, 9000)
  )$looking_times$right

  df_subject$InterPhase_LT_FaceTotal_Gazing <-
    df_subject$InterPhase_LT_FaceLeft_Gazing + df_subject$InterPhase_LT_FaceRight_Gazing

  df_subject$InterPhase_LT_FaceTotal_Gazing_PROP <-
    df_subject$InterPhase_LT_FaceTotal_Gazing / df_subject$InterPhase_LT_Gazing

  df_subject$InterPhase_LT_Object_Gazing <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test,
    c(4000, 9000)
  )$looking_times$center

  df_subject$InterPhase_LT_Object_Gazing_PROP <-
    df_subject$InterPhase_LT_Object_Gazing / df_subject$InterPhase_LT_Gazing

  # Checker for trial inclusion
  df_subject$InterPhase_Checker_Soc <- ifelse(
    df_subject$InterPhase_LT_Soc_Begin == 0, FALSE, TRUE
  ) # & df_subject$InterPhase_LT_Soc_End == 0

  df_subject$InterPhase_Checker_Gazing <- ifelse(
    df_subject$InterPhase_LT_Gazing == 0, FALSE, TRUE
  )

  df_subject$InterPhase_Checker_valid <- ifelse(
    df_subject$InterPhase_Checker_Soc == TRUE & df_subject$InterPhase_Checker_Gazing == TRUE, 1, 0
  )

  # ------------------------------------------------------------------------------------------------
  # Gaze Shifts
  # ------------------------------------------------------------------------------------------------
  # Get Gaze Shift from Face/Object AOI within the familiarization phase (from 4000ms to 9000ms) of actor left to center object
  df_subject$ObjectOriginSocialLeft <- get_looks(
    df,
    aoi_fam_face_object,
    startend_test,
    c(4000, 9000)
  )$gaze_shifts$left$center

  # ------------------------------------------------------------------------------------------------
  # Preferential Looking Phase
  # ------------------------------------------------------------------------------------------------
  df_subject$PrefLook_Object_Fam <- get_objects(df, startend_test)$familiar
  df_subject$PrefLook_Object_Fam_Pos <- get_preflook_pos(
    as.vector(unique(df$StudioEventData[preflook_startend$start])),
    get_objects(df, startend_test)$familiar
  )$fam_pos

  df_subject$PrefLook_Object_Nov <- get_objects(df, startend_test)$novel
  df_subject$PrefLook_Object_Nov_Pos <- get_preflook_pos(
    as.vector(unique(df$StudioEventData[preflook_startend$start])),
    get_objects(df, startend_test)$familiar
  )$nov_pos

  df_subject$PrefLook_LT_Object_Left <- get_looks(
    df,
    aoi_preflook,
    preflook_startend
  )$looking_times$left

  df_subject$PrefLook_LT_Object_Right <- get_looks(
    df,
    aoi_preflook,
    preflook_startend
  )$looking_times$right

  df_subject$PrefLook_LT_Total <- df_subject$PrefLook_LT_Object_Left + df_subject$PrefLook_LT_Object_Right
  df_subject$PrefLook_LT_Object_Fam <-
    ifelse(df_subject$PrefLook_Object_Fam_Pos == "right",
      df_subject$PrefLook_LT_Object_Right,
      ifelse(df_subject$PrefLook_Object_Fam_Pos == "left",
        df_subject$PrefLook_LT_Object_Left, NA
      )
    )
  df_subject$PrefLook_LT_Object_Nov <-
    ifelse(df_subject$PrefLook_Object_Nov_Pos == "right",
      df_subject$PrefLook_LT_Object_Right,
      ifelse(df_subject$PrefLook_Object_Nov_Pos == "left",
        df_subject$PrefLook_LT_Object_Left, NA
      )
    )
  df_subject$PrefLook_LT_Object_Nov_PROP <-
    df_subject$PrefLook_LT_Object_Nov / df_subject$PrefLook_LT_Total

  # ------------------------------------------------------------------------------------------------
  # First Looks
  # ------------------------------------------------------------------------------------------------
  df_subject$PrefLook_FL <- get_looks(df, aoi_preflook, preflook_startend)$first_look
  df_subject$PrefLook_FL_Meaning <-
    ifelse(df_subject$PrefLook_FL == df_subject$PrefLook_Object_Fam_Pos,
      "familiar",
      ifelse(df_subject$PrefLook_FL == df_subject$PrefLook_Object_Nov_Pos,
        "novel", NA
      )
    )
}

