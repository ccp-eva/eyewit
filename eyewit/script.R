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
  # subject <- participants[37]

  # read tsv files
  df_raw <- readr::read_tsv(file.path(interface$raw_dir, subject), col_types = readr::cols())

  # run preflight checks & diagnostics, returns a lean df
  df <- preflight(df_raw, interface)

  # get start and end index pairs for inter_trial chunks
  startend_fam <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[1], "VideoStimulusStart", "VideoStimulusEnd")
  startend_preflook <- get_start_end_pos(df, interface$inter_trial_chunk_patterns[2], "VideoStimulusStart", "VideoStimulusEnd")

  # test if subject has consistent start/end indexes and get check trial count to match 16
  if (get_trial_count(c(startend_fam, startend_preflook)) != 16) {
    incomplete_subjets <- c(incomplete_subjets, subject)
    stop("Bad Trial count")
  }

  # track current trials
  current_test_trials <- get_trial_count(c(startend_fam, startend_preflook))


  # Allocate Trials and fill-up eventValue
  df <- allocate_trials(df, c(startend_fam, startend_preflook), 2)

  # track video names
  names_fam <- df$eventValue[startend_fam$start] |>
    unique() |>
    as.character()

  names_preflook <- df$eventValue[startend_preflook$start] |>
    unique() |>
    as.character()

  # Insert AOI Columns
  df <- tibble::add_column(df, "{interface$aoisets$aoifamphase_obj_r_prox$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$aoifamphase_obj_r_prox, startend_fam), .before = 1)

  df <- tibble::add_column(df, "{interface$aoisets$aoifamphase_obj_r_nprox$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$aoifamphase_obj_r_nprox, startend_fam), .after = 1)

  df <- tibble::add_column(df, "{interface$aoisets$aoifamphase_obj_l_prox$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$aoifamphase_obj_l_prox, startend_fam), .after = 2)

  df <- tibble::add_column(df, "{interface$aoisets$aoifamphase_obj_l_nprox$column_name}" :=
    get_aois(df$x, df$y, interface$aoisets$aoifamphase_obj_l_nprox, startend_fam), .after = 3)

  df <- tibble::add_column(df, "{interface$aoisets$preflook$column_name}" :=
		get_aois(df$x, df$y, interface$aoisets$preflook, startend_preflook), .after = 4)

  df <- tibble::add_column(df, "{interface$aoisets$screen$column_name}" :=
		get_aois(df$x, df$y, interface$aoisets$screen), .after = 5)

  # helper variable
  fi_pairs <- fi2rn(df$fi)
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


  df_subject$ConEye <- value_parser_by_key(interface$keys_fam, names_fam)$con_eye
  df_subject$ConProx <- value_parser_by_key(interface$keys_fam, names_fam)$con_proximity
  df_subject$Condition <- value_parser_by_key(interface$keys_fam, names_fam)$condition
  df_subject$ObjActor <- value_parser_by_key(interface$keys_fam, names_fam)$obj_handling_actor
  df_subject$FamObj <- value_parser_by_key(interface$keys_fam, names_fam)$obj_id
  df_subject$FamObjPos_Fam <- value_parser_by_key(interface$keys_fam, names_fam)$position_obj
  df_subject$TrialRun <- value_parser_by_key(interface$keys_fam, names_fam)$running_trial
  df_subject$TrialCon <- value_parser_by_key(interface$keys_fam, names_fam)$running_trial

  df_subject$TrialCon <- c(
  	rep(1, current_test_trials / 4),
  	rep(2, current_test_trials / 4),
  	rep(3, current_test_trials / 4),
  	rep(4, current_test_trials / 4)
  )

  # ------------------------------------------------------------------------------------------------
  # Looking Times - Preflook
  # ------------------------------------------------------------------------------------------------

  df_subject$TotalLTScreenPreflook <-
    get_looks(df, interface$aoisets$screen, startend_preflook, omit_first_overflow_fi = TRUE)$looking_times

  df_subject$PrefLook_LT_Obj_Left <-
  	get_looks(df, interface$aoisets$preflook, startend_preflook, omit_first_overflow_fi = TRUE)$looking_times$left

  df_subject$PrefLook_LT_Obj_Right <-
  	get_looks(df, interface$aoisets$preflook, startend_preflook, omit_first_overflow_fi = TRUE)$looking_times$right

  df_subject$PrefLook_LT_Obj_Total <- df_subject$PrefLook_LT_Obj_Left + df_subject$PrefLook_LT_Obj_Right


	df_subject$PrefLook_Obj_Fam <- df_subject$FamObj
	df_subject$PrefLook_Obj_Fam_Pos <- get_preflook_pos(names_preflook, strsplit(names_fam, "_"))$fam_pos

	df_subject$PrefLook_Obj_Nov_Pos <- dplyr::if_else(
		df_subject$PrefLook_Obj_Fam_Pos == "left", "right", "left"
	)

	df_subject$PrefLook_LT_Obj_Fam <- dplyr::if_else(
		df_subject$PrefLook_Obj_Fam_Pos == "left", df_subject$PrefLook_LT_Obj_Left, df_subject$PrefLook_LT_Obj_Right
	)

	df_subject$PrefLook_LT_Obj_Nov <- dplyr::if_else(
		df_subject$PrefLook_Obj_Nov_Pos == "left", df_subject$PrefLook_LT_Obj_Left, df_subject$PrefLook_LT_Obj_Right
	)

	df_subject$PrefLook_PropScore <- df_subject$PrefLook_LT_Obj_Nov / df_subject$PrefLook_LT_Obj_Total

  # ------------------------------------------------------------------------------------------------
  # Looking Times - Familiarization
  # ------------------------------------------------------------------------------------------------

  df_subject$TotalLTScreenFam <-
    get_looks(df, interface$aoisets$screen, startend_fam, omit_first_overflow_fi = TRUE)$looking_times


	df_subject$TotalLTFaceLeftFam <- NA
	df_subject$TotalLTFaceRightFam <- NA

	# Fill-up TotalLTFaceLeftFam & TotalLTFaceRighFam based on right/left and prox/nprox
	for (trial in seq.int(current_test_trials)) {

		if (df_subject$ConProx[trial] == "nprox" && df_subject$FamObjPos_Fam[trial] == "right") {
			df_subject$TotalLTFaceLeftFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_r_nprox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_left[trial]
		}

		if (df_subject$ConProx[trial] == "nprox" && df_subject$FamObjPos_Fam[trial] == "left") {
			df_subject$TotalLTFaceLeftFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_l_nprox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_left[trial]
		}

		if (df_subject$ConProx[trial] == "prox" && df_subject$FamObjPos_Fam[trial] == "right") {
			df_subject$TotalLTFaceLeftFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_r_prox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_left[trial]
		}

		if (df_subject$ConProx[trial] == "prox" && df_subject$FamObjPos_Fam[trial] == "left") {
			df_subject$TotalLTFaceLeftFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_l_prox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_left[trial]
		}

		if (df_subject$ConProx[trial] == "nprox" && df_subject$FamObjPos_Fam[trial] == "right") {
			df_subject$TotalLTFaceRightFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_r_nprox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_right[trial]
		}

		if (df_subject$ConProx[trial] == "nprox" && df_subject$FamObjPos_Fam[trial] == "left") {
			df_subject$TotalLTFaceRightFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_l_nprox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_right[trial]
		}

		if (df_subject$ConProx[trial] == "prox" && df_subject$FamObjPos_Fam[trial] == "right") {
			df_subject$TotalLTFaceRightFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_r_prox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_right[trial]
		}

		if (df_subject$ConProx[trial] == "prox" && df_subject$FamObjPos_Fam[trial] == "left") {
			df_subject$TotalLTFaceRightFam[trial] <-
				get_looks(df, interface$aoisets$aoifamphase_obj_l_prox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$face_right[trial]
		}
	}

	# Object Looking Times in Fam
	df_subject$TotalLTObjFamLeft <-
		get_looks(df, interface$aoisets$aoifamphase_obj_l_prox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$object_left

	df_subject$TotalLTObjFamRight <-
		get_looks(df, interface$aoisets$aoifamphase_obj_r_prox, startend_fam, omit_first_overflow_fi = TRUE)$looking_times$object_right

	df_subject$TotalLTObjFam <- if_else(
		df_subject$FamObjPos_Fam == "left", df_subject$TotalLTObjFamLeft, df_subject$TotalLTObjFamRight
	)

  # ------------------------------------------------------------------------------------------------
  # Looking Times - INCLUSION ACTION
  # ------------------------------------------------------------------------------------------------

  df_subject$LTScreenFam_0to1 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(0, 1000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_1to2 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(1001, 2000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_2to3 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(2001, 3000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_3to4 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(3001, 4000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_4to5 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(4001, 5000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_5to6 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(5001, 6000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_6to7 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(6001, 7000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_7to8 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(7001, 8000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_8to9 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(8001, 9000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_9to10 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(9001, 10000), omit_first_overflow_fi = TRUE)$looking_times

  df_subject$LTScreenFam_10to11 <-
  	get_looks(df, interface$aoisets$screen, startend_fam, c(10001, 11000), omit_first_overflow_fi = TRUE)$looking_times







#   df_subject$InterPhaseCheckerSoc <-
#     dplyr::if_else(
#       df_subject$LTScreenAct_5to6 > 0 |
#       df_subject$LTScreenAct_9to10 > 0 |
#       df_subject$LTScreenAct_13to14 > 0,
#       TRUE, FALSE
#     )
#
#   df_subject$InterPhaseCheckerGazing <-
#     dplyr::if_else(
#       df_subject$LTScreenAct_7to8 > 0 |
#         df_subject$LTScreenAct_11to12 > 0 |
#         df_subject$LTScreenAct_15to16 > 0,
#       TRUE, FALSE
#     )
#
#   df_subject$InterPhaseCheckerValid <- df_subject$InterPhaseCheckerSoc & df_subject$InterPhaseCheckerGazing

  # write tables for individual participants
  # replace with readr functions ones this is clear: https://github.com/tidyverse/readr/issues/1388
  # write.table(df_subject, paste0(interface$output_dir, subject), sep = '\t', row.names = FALSE)
}

# Read in tsv files from pre-processing folder
# tsv_files <- list.files(interface$output_dir, full.names = TRUE)

# # Creating data frame
# overall.data <- tsv_files %>%
#   map(read_tsv) %>%    # read in all the files individually, using the function read_tsv() from the readr package
#   reduce(rbind)        # reduce with rbind into one dataframe
