# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

# set working directory
# setwd(file.path("C:", "Users", "steven", "WorkSpaces", "R", "test"))

# import utility functions
sapply(list.files(c("util"), pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv)

# import user interface
source("interface.R")

# reads all files in recs folder
flist <- list.files(recs_dir)


# Loop over all subjects
for (i in 1:length(flist)) {

  current_subject <- flist[i]

  # read tsv files
  df0_raw <- read.table(file = file.path(recs_dir, current_subject), sep = "\t", header = TRUE)

  # run preflight diagnostics of the raw data file
  preflight_status <- preflight(df0_raw, mc)

  # create a lean df including mandatory and columns of interest (todo: this df is not necessary)
  df1_mccoi <- df0_raw[, c(mc, coi)]


  # get start and end index pairs for inter_trial chunks
  familiarization_attention_startend <- get_start_end_pos(df1_mccoi, inter_trial_chunk_patterns[1], "MovieStart", "MovieEnd")
  familiarization_startend <- get_start_end_pos(df1_mccoi, inter_trial_chunk_patterns[2], "MovieStart", "MovieEnd")
  preflook_attention_startend <- get_start_end_pos(df1_mccoi, inter_trial_chunk_patterns[3], "MovieStart", "MovieEnd")
  preflook_startend <- get_start_end_pos(df1_mccoi, inter_trial_chunk_patterns[4], "MovieStart", "MovieEnd")



  # Allocate Trials and Fillup StudioEventData Label (todo: this df is not necessary)
  df2_trial <- df1_mccoi
  df2_trial <- allocate_trials(df2_trial, familiarization_attention_startend) # keep this enabled to track valid trials
  df2_trial <- allocate_trials(df2_trial, familiarization_startend)
  df2_trial <- allocate_trials(df2_trial, preflook_attention_startend) # keep this enabled to track valid trials
  df2_trial <- allocate_trials(df2_trial, preflook_startend)


  # track the number of max trials (todo use start end positions to determien trial length, also create a function that checks trial length consistency over all startend positions)
  total_trials <- max(df2_trial$Trial, na.rm = TRUE)

  # track vector of all inter names (important for dfX_base performance)
  inter_vectors <- as.character(unique(df2_trial$StudioEventData[familiarization_startend$start]))

  # track valid trials
  # todo


  # AOI Columns
  df3_aoi <- df2_trial

  # AOI column for Familiarization Phase for Body & Object (left, right, center)
  df3_aoi <- get_aois(df3_aoi, aoi_fam_body_object, familiarization_startend)
  # AOI column for Familiarization Phase for faces (left & right)
  df3_aoi <- get_aois(df3_aoi, aoi_fam_face, familiarization_startend)
  # AOI column for Familiarization Phase for Face & Object (left, right, center)
  df3_aoi <- get_aois(df3_aoi, aoi_fam_face_object, familiarization_startend)
  # AOI column for Preferential Looking Phase for objects (left & right)
  df3_aoi <- get_aois(df3_aoi, aoi_preflook, preflook_startend)
  # AOI column for Familiarization Phase & Preferential Looking Phase for screen (TRUE/FALSE)
  df3_aoi <- get_aois(df3_aoi, aoi_screen, c(familiarization_startend, preflook_startend))


  ####################################################################################################
  # Initialize empty Dataframe with 0 columns and row count equals current total_trials
  dfX_base <- data.frame(matrix(NA, nrow = total_trials, ncol = 0), stringsAsFactors = FALSE)

  # Build Summary table
  # ==================================================================================================
  # NAME INFORMATIONS
  # --------------------------------------------------------------------------------------------------
  dfX_base$ID <- value_parser_by_key(lut_filename, current_subject)$ID
  dfX_base$Sex <- value_parser_by_key(lut_filename, current_subject)$Sex
  dfX_base$Age_Days <- value_parser_by_key(lut_filename, current_subject)$Age_Days
  dfX_base$Trial <- 1:total_trials
  dfX_base$Condition <- value_parser_by_key(lut_fam_phase, inter_vectors)$Condition
  dfX_base$Con_Object <- value_parser_by_key(lut_fam_phase, inter_vectors)$Con_Object
  dfX_base$Con_SocInt <- value_parser_by_key(lut_fam_phase, inter_vectors)$Con_SocInt
  dfX_base$Dyad <- value_parser_by_key(lut_fam_phase, inter_vectors)$Dyad
  # --------------------------------------------------------------------------------------------------
  # Familiarization Phase
  # --------------------------------------------------------------------------------------------------
  # OVERALL SCREEN (no specific AOI): Looking times over total duration of the video
  dfX_base$Overall_LT_ScreenFam <- get_looks(df3_aoi, aoi_screen, familiarization_startend)$looking_times
  dfX_base$Overall_LT_ScreenPrefLook <- get_looks(df3_aoi, aoi_screen, preflook_startend)$looking_times
  dfX_base$Inter_PropOverall_LT_Screen <- ifelse(dfX_base$Overall_LT_ScreenFam <= 11000, dfX_base$Overall_LT_ScreenFam/11000, dfX_base$Overall_LT_ScreenFam/max(dfX_base$Overall_LT_ScreenFam))
  # OVERALL SCREEN (no specific AOI): Looking times only in INTEARCTION PHASES (in the beginning and ending of the video sequence)
  dfX_base$InterPhase_LT_Soc_Begin <- get_looks(df3_aoi, aoi_screen, familiarization_startend, c(2000, 3000))$looking_times
  dfX_base$InterPhase_LT_Soc_End <- get_looks(df3_aoi, aoi_screen, familiarization_startend, c(10000, 11000))$looking_times
  dfX_base$InterPhase_LT_Soc_Total <- dfX_base$InterPhase_LT_Soc_Begin + dfX_base$InterPhase_LT_Soc_End
  # OVERALL SCREEN (no specific AOI): Looking times only in GAZING PHASE
  dfX_base$InterPhase_LT_Gazing <- get_looks(df3_aoi, aoi_screen, familiarization_startend, c(4000, 9000))$looking_times

  # AOIs: Looking times over TOTAL duration of the video
  dfX_base$Inter_LT_ActorLeft <- get_looks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$left
  dfX_base$Inter_LT_ActorRight <- get_looks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$right
  dfX_base$Inter_LT_ActorsTotal <- dfX_base$Inter_LT_ActorLeft + dfX_base$Inter_LT_ActorRight
  dfX_base$Inter_LT_Actors_PROP <- dfX_base$Inter_LT_ActorsTotal / dfX_base$Overall_LT_ScreenFam
  dfX_base$Inter_LT_FaceLeft <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_times$left
  dfX_base$Inter_LT_FaceRight <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_times$right
  dfX_base$Inter_LT_FacesTotal <- dfX_base$Inter_LT_FaceLeft + dfX_base$Inter_LT_FaceRight
  dfX_base$Inter_LT_Faces_PROP <- dfX_base$Inter_LT_FacesTotal / dfX_base$Overall_LT_ScreenFam
  dfX_base$Inter_LT_Object <- get_looks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$center
  dfX_base$Inter_LT_FacesObject_PROP <- dfX_base$Inter_LT_FacesTotal / (dfX_base$Inter_LT_FacesTotal + dfX_base$Inter_LT_Object)
  dfX_base$Inter_LT_Object_PROP <- dfX_base$Inter_LT_Object / dfX_base$Overall_LT_ScreenFam
  # AOIs: Looking times in INTERACTION PHASES
  dfX_base$InterPhase_LT_FaceLeft_Soc_Begin <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Soc_Begin <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin <- dfX_base$InterPhase_LT_FaceLeft_Soc_Begin + dfX_base$InterPhase_LT_FaceRight_Soc_Begin
  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin_PROP <- dfX_base$InterPhase_LT_FaceTotal_Soc_Begin / dfX_base$InterPhase_LT_Soc_Begin
  dfX_base$InterPhase_LT_FaceLeft_Soc_End <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Soc_End <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Soc_End <- dfX_base$InterPhase_LT_FaceRight_Soc_End + dfX_base$InterPhase_LT_FaceLeft_Soc_End
  dfX_base$InterPhase_LT_FaceTotal_Soc_End_PROP <- dfX_base$InterPhase_LT_FaceTotal_Soc_End / dfX_base$InterPhase_LT_Soc_End
  dfX_base$InterPhase_LT_FaceLeft_Soc_Total <- dfX_base$InterPhase_LT_FaceLeft_Soc_Begin + dfX_base$InterPhase_LT_FaceLeft_Soc_End
  dfX_base$InterPhase_LT_FaceRight_Soc_Total <- dfX_base$InterPhase_LT_FaceRight_Soc_Begin + dfX_base$InterPhase_LT_FaceRight_Soc_End
  dfX_base$InterPhase_LT_FaceTotal_Soc_Total <- dfX_base$InterPhase_LT_FaceTotal_Soc_Begin + dfX_base$InterPhase_LT_FaceTotal_Soc_End
  dfX_base$InterPhase_LT_FaceTotal_Soc_Total_PROP <- dfX_base$InterPhase_LT_FaceTotal_Soc_Total / dfX_base$InterPhase_LT_Soc_Total
  dfX_base$InterPhase_LT_Object_Soc_Begin <- get_looks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(2000, 3000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Soc_Begin_PROP <- dfX_base$InterPhase_LT_Object_Soc_Begin / dfX_base$InterPhase_LT_Soc_Total
  dfX_base$InterPhase_LT_Object_Soc_End <- get_looks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(10000, 11000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Soc_End_PROP <- dfX_base$InterPhase_LT_Object_Soc_End / dfX_base$InterPhase_LT_Soc_Total
  dfX_base$InterPhase_LT_Object_Soc_Total <- dfX_base$InterPhase_LT_Object_Soc_Begin + dfX_base$InterPhase_LT_Object_Soc_End
  dfX_base$InterPhase_LT_Object_Soc_Total_PROP <- dfX_base$InterPhase_LT_Object_Soc_Total / dfX_base$InterPhase_LT_Soc_Total
  # AOIs: Looking times in GAZING PHASE
  dfX_base$InterPhase_LT_FaceLeft_Gazing <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend, c(4000, 9000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Gazing <- get_looks(df3_aoi, aoi_fam_face, familiarization_startend, c(4000, 9000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Gazing <- dfX_base$InterPhase_LT_FaceLeft_Gazing + dfX_base$InterPhase_LT_FaceRight_Gazing
  dfX_base$InterPhase_LT_FaceTotal_Gazing_PROP <- dfX_base$InterPhase_LT_FaceTotal_Gazing / dfX_base$InterPhase_LT_Gazing
  dfX_base$InterPhase_LT_Object_Gazing <- get_looks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(4000, 9000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Gazing_PROP <- dfX_base$InterPhase_LT_Object_Gazing / dfX_base$InterPhase_LT_Gazing

  # Checker for trial inclusion
  dfX_base$InterPhase_Checker_Soc <- ifelse(dfX_base$InterPhase_LT_Soc_Begin == 0, FALSE, TRUE) # & dfX_base$InterPhase_LT_Soc_End == 0
  dfX_base$InterPhase_Checker_Gazing <- ifelse(dfX_base$InterPhase_LT_Gazing == 0, FALSE, TRUE)
  dfX_base$InterPhase_Checker_valid <-  ifelse(dfX_base$InterPhase_Checker_Soc == TRUE & dfX_base$InterPhase_Checker_Gazing == TRUE, 1, 0)

  # -------------------------------------------------------------------------------------------
  # Gaze Shifts
  # -------------------------------------------------------------------------------------------
  # Get Gaze Shift from Face/Object AOI within the familiarization phase (from 4000ms to 9000ms) of actor left to center object
  dfX_base$ObjectOriginSocialLeft <- get_looks(df3_aoi, aoi_fam_face_object, familiarization_startend, c(4000, 9000))$gaze_shifts$left$center

  # -------------------------------------------------------------------------------------------
  # Preferential Looking Phase
  # -------------------------------------------------------------------------------------------
  dfX_base$PrefLook_Object_Fam <- get_objects(df3_aoi, familiarization_startend)$familiar
  dfX_base$PrefLook_Object_Fam_Pos <- get_preflook_pos(as.vector(unique(df3_aoi$StudioEventData[preflook_startend$start])), get_objects(df3_aoi, familiarization_startend)$familiar)$fam_pos
  dfX_base$PrefLook_Object_Nov <- get_objects(df3_aoi, familiarization_startend)$novel
  dfX_base$PrefLook_Object_Nov_Pos <- get_preflook_pos(as.vector(unique(df3_aoi$StudioEventData[preflook_startend$start])), get_objects(df3_aoi, familiarization_startend)$familiar)$nov_pos
  dfX_base$PrefLook_LT_Object_Left <- get_looks(df3_aoi, aoi_preflook, preflook_startend)$looking_times$left
  dfX_base$PrefLook_LT_Object_Right <- get_looks(df3_aoi, aoi_preflook, preflook_startend)$looking_times$right
  dfX_base$PrefLook_LT_Total <- dfX_base$PrefLook_LT_Object_Left + dfX_base$PrefLook_LT_Object_Right
  dfX_base$PrefLook_LT_Object_Fam <-
    ifelse(dfX_base$PrefLook_Object_Fam_Pos == "right",
           dfX_base$PrefLook_LT_Object_Right,
           ifelse(dfX_base$PrefLook_Object_Fam_Pos == "left",
                  dfX_base$PrefLook_LT_Object_Left, NA))
  dfX_base$PrefLook_LT_Object_Nov <-
    ifelse(dfX_base$PrefLook_Object_Nov_Pos == "right",
           dfX_base$PrefLook_LT_Object_Right,
           ifelse(dfX_base$PrefLook_Object_Nov_Pos == "left",
                  dfX_base$PrefLook_LT_Object_Left, NA))
  dfX_base$PrefLook_LT_Object_Nov_PROP <- dfX_base$PrefLook_LT_Object_Nov / dfX_base$PrefLook_LT_Total

  # -------------------------------------------------------------------------------------------
  # First Looks
  # -------------------------------------------------------------------------------------------
  dfX_base$PrefLook_FL <- get_looks(df3_aoi, aoi_preflook, preflook_startend)$first_look
  dfX_base$PrefLook_FL_Meaning <-
    ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Fam_Pos,
           "familiar",
           ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Nov_Pos,
                  "novel", NA))
}

