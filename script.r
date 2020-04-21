# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

# set working directory
# setwd(file.path("C:", "Users", "steven", "WorkSpaces", "R", "test"))

# import utility functions & mute RStudio diagnostics
# !diagnostics suppress=allocateTrials, getAOIs, getCS, getLooks, getObjects, getPrefLookPositions, getStartEndPositions
sapply(list.files(c("util"), pattern = "*.r$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv)

# import user interface
# !diagnostics suppress=coi, inter_trial_chunk_patterns, aoi_fam_body_object, aoi_fam_face, aoi_preflook, aoi_screen
source("interface.r")

# Set directories
recs_dir <- "./recs/"

# reads all files in recs folder
flist <- list.files(recs_dir)


# Loop over all subjects
for (i in 1:length(flist)) {

  # read tsv files
  df0_raw <- read.table(file = file.path(recs_dir, flist[i]), sep = "\t", header = TRUE)

  # create COI df
  df1_coi <- df0_raw[, coi]

  # get start and end index pairs for inter_trial chunks
  familiarization_attention_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[1], "MovieStart", "MovieEnd")
  familiarization_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[2], "MovieStart", "MovieEnd")
  preflook_attention_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[3], "MovieStart", "MovieEnd")
  preflook_startend <- getStartEndPositions(df1_coi, inter_trial_chunk_patterns[4], "MovieStart", "MovieEnd")


  # Allocate Trials and Fillup StudioEventData Label
  df2_trial <- df1_coi
  df2_trial <- allocateTrials(df2_trial, familiarization_attention_startend) # keep this enabled to track valid trials
  df2_trial <- allocateTrials(df2_trial, familiarization_startend)
  df2_trial <- allocateTrials(df2_trial, preflook_attention_startend) # keep this enabled to track valid trials
  df2_trial <- allocateTrials(df2_trial, preflook_startend)


  # track the number of max trials
  total_trials <- max(df2_trial$Trial, na.rm = TRUE)

  # track vector of all inter names (important for dfX_base performance)
  inter_vectors <- as.character(unique(df2_trial$StudioEventData[familiarization_startend$start]))

  # track valid trials
  # todo


  # AOI Columns
  df3_aoi <- df2_trial

  # AOI column for Familiarization Phase for Body & Object (left, right, center)
  df3_aoi <- getAOIs(df3_aoi, aoi_fam_body_object, familiarization_startend)
  # AOI column for Familiarization Phase for faces (left & right)
  df3_aoi <- getAOIs(df3_aoi, aoi_fam_face, familiarization_startend)
  # AOI column for Preferential Looking Phase for objects (left & right)
  df3_aoi <- getAOIs(df3_aoi, aoi_preflook, preflook_startend)
  # AOI column for Familiarization Phase for screen (TRUE/FALSE)
  df3_aoi <- getAOIs(df3_aoi, aoi_screen, familiarization_startend)


  ####################################################################################################
  # Initialize empty Dataframe with 0 columns and row count equals current total_trials
  dfX_base <- data.frame(matrix(NA, nrow = total_trials, ncol = 0), stringsAsFactors = FALSE)

  # Build Summary table
  # ==================================================================================================
  # NAME INFORMATIONS
  # --------------------------------------------------------------------------------------------------
  dfX_base$ID <- unlist(strsplit(flist[i], split = "_"))[2]
  dfX_base$Sex <- unlist(strsplit(flist[i], split = "_"))[3]
  dfX_base$Age_Days <- unlist(strsplit(flist[i], split = "_"))[4]
  dfX_base$Trial <- 1:total_trials
  dfX_base$Condition <- unlist(lapply(strsplit(inter_vectors, split = "_"), `[[`, 4))
  dfX_base$Con_Object <- unlist(lapply(strsplit(inter_vectors, split = "_"), `[[`, 6))
  dfX_base$Con_SocInt <- unlist(lapply(strsplit(inter_vectors, split = "_"), `[[`, 5))
  dfX_base$Dyad <- unlist(lapply(strsplit(inter_vectors, split = "_"), `[[`, 7))
  # --------------------------------------------------------------------------------------------------
  # Familiarization Phase
  # --------------------------------------------------------------------------------------------------
  dfX_base$Overall_LT_ScreenFam <- getLooks(df3_aoi, aoi_screen, familiarization_startend, stop_if_multiple_hit_names_in_single_fixation = FALSE)$looking_times
  dfX_base$Overall_LT_ScreenPrefLook <- getLooks(df3_aoi, aoi_screen, preflook_startend, stop_if_multiple_hit_names_in_single_fixation = FALSE)$looking_times
  dfX_base$Inter_PropOverall_LT_Screen <- dfX_base$Overall_LT_ScreenFam/11000
  dfX_base$Inter_LT_ActorLeft <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$left
  dfX_base$Inter_LT_ActorRight <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$right
  dfX_base$Inter_LT_ActorsTotal <- dfX_base$Inter_LT_ActorLeft + dfX_base$Inter_LT_ActorRight
  dfX_base$Inter_LT_Actors_PROP <- dfX_base$Inter_LT_ActorsTotal/dfX_base$Overall_LT_ScreenFam
  dfX_base$Inter_LT_FaceLeft <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_times$left
  dfX_base$Inter_LT_FaceRight <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend)$looking_times$right
  dfX_base$Inter_LT_FacesTotal <- dfX_base$Inter_LT_FaceLeft + dfX_base$Inter_LT_FaceRight
  dfX_base$Inter_LT_Faces_PROP <- dfX_base$Inter_LT_FacesTotal/dfX_base$Overall_LT_ScreenFam
  dfX_base$Inter_LT_Object <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend)$looking_times$center
  dfX_base$Inter_LT_FacesObject_PROP <- dfX_base$Inter_LT_FacesTotal/(dfX_base$Inter_LT_FacesTotal + dfX_base$Inter_LT_Object)
  dfX_base$Inter_LT_Object_PROP <- dfX_base$Inter_LT_Object/dfX_base$Overall_LT_ScreenFam
  dfX_base$InterPhase_LT_Soc_Begin <- getLooks(df3_aoi, aoi_screen, familiarization_startend, c(2000, 3000))$looking_times
  dfX_base$InterPhase_LT_Soc_End <- getLooks(df3_aoi, aoi_screen, familiarization_startend, c(10000, 11000), stop_if_multiple_hit_names_in_single_fixation = FALSE)$looking_times
  dfX_base$InterPhase_LT_Gazing <- getLooks(df3_aoi, aoi_screen, familiarization_startend, c(4000, 9000), stop_if_multiple_hit_names_in_single_fixation = FALSE)$looking_times
  dfX_base$InterPhase_Checker_Soc <- ifelse(dfX_base$InterPhase_LT_Soc_Begin == 0 & dfX_base$InterPhase_LT_Soc_End == 0, FALSE, TRUE)
  dfX_base$InterPhase_Checker_Gazing <- ifelse(dfX_base$InterPhase_LT_Gazing == 0, FALSE, TRUE)
  dfX_base$InterPhase_LT_FaceLeft_Soc_Begin <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Soc_Begin <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(2000, 3000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin <- dfX_base$InterPhase_LT_FaceLeft_Soc_Begin + dfX_base$InterPhase_LT_FaceRight_Soc_Begin
  dfX_base$InterPhase_LT_FaceTotal_Soc_Begin_PROP <- "todo"
  dfX_base$InterPhase_LT_Object_Soc_Begin <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(2000, 3000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Soc_Begin_PROP <- "todo"
  dfX_base$InterPhase_LT_FaceLeft_Soc_End <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Soc_End <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(10000, 11000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Soc_End <- "todo"
  dfX_base$InterPhase_LT_FaceTotal_Soc_End_PROP <- "todo"
  dfX_base$InterPhase_LT_Object_Soc_End <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(10000, 11000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Soc_End_PROP <- "todo"
  dfX_base$InterPhase_LT_FaceLeft_Soc_Total <- "todo"
  dfX_base$InterPhase_LT_FaceRight_Soc_Total <- "todo"
  dfX_base$InterPhase_LT_FaceTotal_Soc_Total <- "todo"
  dfX_base$InterPhase_LT_FaceTotal_Soc_Total_PROP <- "todo"
  dfX_base$InterPhase_LT_Object_Soc_Total <- "todo"
  dfX_base$InterPhase_LT_Object_Soc_Total_PROP <- "todo"

  dfX_base$InterPhase_LT_FaceLeft_Gazing <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(4000, 9000))$looking_times$left
  dfX_base$InterPhase_LT_FaceRight_Gazing <- getLooks(df3_aoi, aoi_fam_face, familiarization_startend, c(4000, 9000))$looking_times$right
  dfX_base$InterPhase_LT_FaceTotal_Gazing <- "todo"
  dfX_base$InterPhase_LT_FaceTotal_Gazing_PROP <- "todo"
  dfX_base$InterPhase_LT_Object_Gazing <- getLooks(df3_aoi, aoi_fam_body_object, familiarization_startend, c(4000, 9000))$looking_times$center
  dfX_base$InterPhase_LT_Object_Gazing_PROP <- "todo"


  # -------------------------------------------------------------------------------------------
  # Preferential Looking Phase
  # -------------------------------------------------------------------------------------------
  dfX_base$PrefLook_Object_Fam <- getObjects(df3_aoi, familiarization_startend)$familiar
  dfX_base$PrefLook_Object_Fam_Pos <- getPrefLookPositions(as.vector(unique(df3_aoi$StudioEventData[preflook_startend$start])), getObjects(df3_aoi, familiarization_startend)$familiar)$fam_pos
  dfX_base$PrefLook_Object_Nov <- getObjects(df3_aoi, familiarization_startend)$novel
  dfX_base$PrefLook_Object_Nov_Pos <- getPrefLookPositions(as.vector(unique(df3_aoi$StudioEventData[preflook_startend$start])), getObjects(df3_aoi, familiarization_startend)$familiar)$nov_pos
  dfX_base$PrefLook_LT_Object_Left <- getLooks(df3_aoi, aoi_preflook, preflook_startend)$looking_times$left
  dfX_base$PrefLook_LT_Object_Right <- getLooks(df3_aoi, aoi_preflook, preflook_startend)$looking_times$right
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
  dfX_base$PrefLook_FL <- getLooks(df3_aoi, aoi_preflook, preflook_startend)$first_look
  dfX_base$PrefLook_FL_Meaning <-
    ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Fam_Pos,
           "familiar",
           ifelse(dfX_base$PrefLook_FL == dfX_base$PrefLook_Object_Nov_Pos,
                  "novel", NA))

}

