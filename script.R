# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics


if (!require(tidyverse)) install.packages(tidyerse)
library(tidyverse)


# import utility functions
list.files("util", "*.R$", full.names = TRUE, ignore.case = TRUE) %>% sapply(source)

# import user interface
source("interface.R")

# read raw data filenames
participants <- list.files(raw_dir)


# Loop over all participants
for (subject in participants) {

  # remove later
  subject <- participants[1]

  # read tsv files
  df_raw <- read_tsv(file.path(raw_dir, subject))

  # run preflight & diagnostics
  df_raw <- preflight(df_raw, mc)

  # create a lean df including mandatory and columns of interest
  df <- df_raw[, c(mc, coi)]

  # get start and end index pairs for inter_trial chunks
  startend_pretest <- get_start_end_pos(
    df,
    inter_trial_chunk_patterns[1],
    "MovieStart",
    "MovieEnd"
  )
  startend_test <- get_start_end_pos(
    df,
    inter_trial_chunk_patterns[2],
    "MovieStart",
    "MovieEnd"
  )



  # Allocate Trials and Fillup StudioEventData Label
  df <- allocate_trials(df, startend_pretest)
  df <- allocate_trials(df, startend_test)


  # track the number of max trials
  total_trials <- max(df$Trial, na.rm = TRUE)

  # track vector of all inter names (important for dfX_base performance)
  inter_vectors <- df$StudioEventData[startend_test$start] %>%
    unique() %>%
    as.character()


  # Insert AOI Columns
  df <- get_aois(df, aoiset_actionphasebody, startend_test)
  df <- get_aois(df, aoiset_actionphaseface, startend_test)
  df <- get_aois(df, aoiset_outcomephase, startend_test)
  df <- get_aois(df, aoiset_screen, startend_test)


  ##################################################################################################
  # Initialize empty Dataframe with 0 columns and row count equals current total_trials
  dfX_base <- data.frame(matrix(NA, nrow = total_trials, ncol = 0), stringsAsFactors = FALSE)

  # Build Summary table
  # ================================================================================================
  # NAME INFORMATIONS
  # ------------------------------------------------------------------------------------------------
  dfX_base$ID <- value_parser_by_key(lut_filename, current_subject)$ID
  dfX_base$Sex <- value_parser_by_key(lut_filename, current_subject)$Sex
  dfX_base$Age_Days <- value_parser_by_key(lut_filename, current_subject)$Age_Days
  dfX_base$Trial <- 1:total_trials
  dfX_base$Condition <- value_parser_by_key(lut_fam_phase, inter_vectors)$Condition
  dfX_base$Con_Object <- value_parser_by_key(lut_fam_phase, inter_vectors)$Con_Object
  dfX_base$Con_SocInt <- value_parser_by_key(lut_fam_phase, inter_vectors)$Con_SocInt
  dfX_base$Dyad <- value_parser_by_key(lut_fam_phase, inter_vectors)$Dyad
  # ------------------------------------------------------------------------------------------------
  # Familiarization Phase
  # ------------------------------------------------------------------------------------------------
  # OVERALL SCREEN (no specific AOI): Looking times over total duration of the video
  dfX_base$Overall_LT_ScreenFam <- get_looks(
    df,
    aoi_screen,
    startend_test
  )$looking_times

  dfX_base$Overall_LT_ScreenPrefLook <- get_looks(
    df,
    aoi_screen,
    preflook_startend
  )$looking_times

  dfX_base$Inter_PropOverall_LT_Screen <- ifelse(
    dfX_base$Overall_LT_ScreenFam <= 11000,
    dfX_base$Overall_LT_ScreenFam / 11000,
    dfX_base$Overall_LT_ScreenFam / max(dfX_base$Overall_LT_ScreenFam)
  )

  # OVERALL SCREEN (no specific AOI): Looking times only in INTEARCTION PHASES
  # ... (in the beginning and ending of the video sequence)
  dfX_base$InterPhase_LT_Soc_Begin <- get_looks(
    df,
    aoi_screen,
    startend_test,
    c(2000, 3000)
  )$looking_times

  dfX_base$InterPhase_LT_Soc_End <- get_looks(
    df,
    aoi_screen,
    startend_test,
    c(10000, 11000)
  )$looking_times

  dfX_base$InterPhase_LT_Soc_Total <-
    dfX_base$InterPhase_LT_Soc_Begin + dfX_base$InterPhase_LT_Soc_End

  # OVERALL SCREEN (no specific AOI): Looking times only in GAZING PHASE
  dfX_base$InterPhase_LT_Gazing <- get_looks(
    df,
    aoi_screen,
    startend_test,
    c(4000, 9000)
  )$looking_times

  # AOIs: Looking times over TOTAL duration of the video
  dfX_base$Inter_LT_ActorLeft <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test
  )$looking_times$left

  dfX_base$Inter_LT_ActorRight <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test
  )$looking_times$right

  dfX_base$Inter_LT_ActorsTotal <- dfX_base$Inter_LT_ActorLeft + dfX_base$Inter_LT_ActorRight

  dfX_base$Inter_LT_Actors_PROP <- dfX_base$Inter_LT_ActorsTotal / dfX_base$Overall_LT_ScreenFam

  dfX_base$Inter_LT_FaceLeft <- get_looks(
    df,
    aoi_fam_face,
    startend_test
  )$looking_times$left

  dfX_base$Inter_LT_FaceRight <- get_looks(
    df,
    aoi_fam_face,
    startend_test
  )$looking_times$right

  dfX_base$Inter_LT_FacesTotal <- dfX_base$Inter_LT_FaceLeft + dfX_base$Inter_LT_FaceRight

  dfX_base$Inter_LT_Faces_PROP <- dfX_base$Inter_LT_FacesTotal / dfX_base$Overall_LT_ScreenFam

  dfX_base$Inter_LT_Object <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test
  )$looking_times$center

  dfX_base$Inter_LT_FacesObject_PROP <-
    dfX_base$Inter_LT_FacesTotal / (dfX_base$Inter_LT_FacesTotal + dfX_base$Inter_LT_Object)

  dfX_base$Inter_LT_Object_PROP <- dfX_base$Inter_LT_Object / dfX_base$Overall_LT_ScreenFam

  # AOIs: Looking times in INTERACTION PHASES
  dfX_base$InterPhase_LT_FaceLeft_Soc_Begin <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(2000, 3000)
  )$looking_times$left

  dfX_base$InterPhase_LT_FaceRight_Soc_Begin <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(2000, 3000)
  )$looking_times$right

  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin <-
    dfX_base$InterPhase_LT_FaceLeft_Soc_Begin + dfX_base$InterPhase_LT_FaceRight_Soc_Begin

  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin_PROP <-
    dfX_base$InterPhase_LT_FaceTotal_Soc_Begin / dfX_base$InterPhase_LT_Soc_Begin

  dfX_base$InterPhase_LT_FaceLeft_Soc_End <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(10000, 11000)
  )$looking_times$left

  dfX_base$InterPhase_LT_FaceRight_Soc_End <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(10000, 11000)
  )$looking_times$right

  dfX_base$InterPhase_LT_FaceTotal_Soc_End <-
    dfX_base$InterPhase_LT_FaceRight_Soc_End + dfX_base$InterPhase_LT_FaceLeft_Soc_End

  dfX_base$InterPhase_LT_FaceTotal_Soc_End_PROP <-
    dfX_base$InterPhase_LT_FaceTotal_Soc_End / dfX_base$InterPhase_LT_Soc_End

  dfX_base$InterPhase_LT_FaceLeft_Soc_Total <-
    dfX_base$InterPhase_LT_FaceLeft_Soc_Begin + dfX_base$InterPhase_LT_FaceLeft_Soc_End

  dfX_base$InterPhase_LT_FaceRight_Soc_Total <-
    dfX_base$InterPhase_LT_FaceRight_Soc_Begin + dfX_base$InterPhase_LT_FaceRight_Soc_End

  dfX_base$InterPhase_LT_FaceTotal_Soc_Total <-
    dfX_base$InterPhase_LT_FaceTotal_Soc_Begin + dfX_base$InterPhase_LT_FaceTotal_Soc_End

  dfX_base$InterPhase_LT_FaceTotal_Soc_Total_PROP <-
    dfX_base$InterPhase_LT_FaceTotal_Soc_Total / dfX_base$InterPhase_LT_Soc_Total

  dfX_base$InterPhase_LT_Object_Soc_Begin <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test,
    c(2000, 3000)
  )$looking_times$center

  dfX_base$InterPhase_LT_Object_Soc_Begin_PROP <-
    dfX_base$InterPhase_LT_Object_Soc_Begin / dfX_base$InterPhase_LT_Soc_Total

  dfX_base$InterPhase_LT_Object_Soc_End <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test,
    c(10000, 11000)
  )$looking_times$center

  dfX_base$InterPhase_LT_Object_Soc_End_PROP <-
    dfX_base$InterPhase_LT_Object_Soc_End / dfX_base$InterPhase_LT_Soc_Total

  dfX_base$InterPhase_LT_Object_Soc_Total <-
    dfX_base$InterPhase_LT_Object_Soc_Begin + dfX_base$InterPhase_LT_Object_Soc_End

  dfX_base$InterPhase_LT_Object_Soc_Total_PROP <-
    dfX_base$InterPhase_LT_Object_Soc_Total / dfX_base$InterPhase_LT_Soc_Total

  # AOIs: Looking times in GAZING PHASE
  dfX_base$InterPhase_LT_FaceLeft_Gazing <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(4000, 9000)
  )$looking_times$left

  dfX_base$InterPhase_LT_FaceRight_Gazing <- get_looks(
    df,
    aoi_fam_face,
    startend_test,
    c(4000, 9000)
  )$looking_times$right

  dfX_base$InterPhase_LT_FaceTotal_Gazing <-
    dfX_base$InterPhase_LT_FaceLeft_Gazing + dfX_base$InterPhase_LT_FaceRight_Gazing

  dfX_base$InterPhase_LT_FaceTotal_Gazing_PROP <-
    dfX_base$InterPhase_LT_FaceTotal_Gazing / dfX_base$InterPhase_LT_Gazing

  dfX_base$InterPhase_LT_Object_Gazing <- get_looks(
    df,
    aoi_fam_body_object,
    startend_test,
    c(4000, 9000)
  )$looking_times$center

  dfX_base$InterPhase_LT_Object_Gazing_PROP <-
    dfX_base$InterPhase_LT_Object_Gazing / dfX_base$InterPhase_LT_Gazing

  # Checker for trial inclusion
  dfX_base$InterPhase_Checker_Soc <- ifelse(
    dfX_base$InterPhase_LT_Soc_Begin == 0, FALSE, TRUE
  ) # & dfX_base$InterPhase_LT_Soc_End == 0

  dfX_base$InterPhase_Checker_Gazing <- ifelse(
    dfX_base$InterPhase_LT_Gazing == 0, FALSE, TRUE
  )

  dfX_base$InterPhase_Checker_valid <- ifelse(
    dfX_base$InterPhase_Checker_Soc == TRUE & dfX_base$InterPhase_Checker_Gazing == TRUE, 1, 0
  )

  # ------------------------------------------------------------------------------------------------
  # Gaze Shifts
  # ------------------------------------------------------------------------------------------------
  # Get Gaze Shift from Face/Object AOI within the familiarization phase (from 4000ms to 9000ms) of actor left to center object
  dfX_base$ObjectOriginSocialLeft <- get_looks(
    df,
    aoi_fam_face_object,
    startend_test,
    c(4000, 9000)
  )$gaze_shifts$left$center

  # ------------------------------------------------------------------------------------------------
  # Preferential Looking Phase
  # ------------------------------------------------------------------------------------------------
  dfX_base$PrefLook_Object_Fam <- get_objects(df, startend_test)$familiar
  dfX_base$PrefLook_Object_Fam_Pos <- get_preflook_pos(
    as.vector(unique(df$StudioEventData[preflook_startend$start])),
    get_objects(df, startend_test)$familiar
  )$fam_pos

  dfX_base$PrefLook_Object_Nov <- get_objects(df, startend_test)$novel
  dfX_base$PrefLook_Object_Nov_Pos <- get_preflook_pos(
    as.vector(unique(df$StudioEventData[preflook_startend$start])),
    get_objects(df, startend_test)$familiar
  )$nov_pos

  dfX_base$PrefLook_LT_Object_Left <- get_looks(
    df,
    aoi_preflook,
    preflook_startend
  )$looking_times$left

  dfX_base$PrefLook_LT_Object_Right <- get_looks(
    df,
    aoi_preflook,
    preflook_startend
  )$looking_times$right

  dfX_base$PrefLook_LT_Total <- dfX_base$PrefLook_LT_Object_Left + dfX_base$PrefLook_LT_Object_Right
  dfX_base$PrefLook_LT_Object_Fam <-
    ifelse(dfX_base$PrefLook_Object_Fam_Pos == "right",
      dfX_base$PrefLook_LT_Object_Right,
      ifelse(dfX_base$PrefLook_Object_Fam_Pos == "left",
        dfX_base$PrefLook_LT_Object_Left, NA
      )
    )
  dfX_base$PrefLook_LT_Object_Nov <-
    ifelse(dfX_base$PrefLook_Object_Nov_Pos == "right",
      dfX_base$PrefLook_LT_Object_Right,
      ifelse(dfX_base$PrefLook_Object_Nov_Pos == "left",
        dfX_base$PrefLook_LT_Object_Left, NA
      )
    )
  dfX_base$PrefLook_LT_Object_Nov_PROP <-
    dfX_base$PrefLook_LT_Object_Nov / dfX_base$PrefLook_LT_Total

  # ------------------------------------------------------------------------------------------------
  # First Looks
  # ------------------------------------------------------------------------------------------------
  dfX_base$PrefLook_FL <- get_looks(df, aoi_preflook, preflook_startend)$first_look
  dfX_base$PrefLook_FL_Meaning <-
    ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Fam_Pos,
      "familiar",
      ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Nov_Pos,
        "novel", NA
      )
    )
}
