# load eyewit (for package dev load it: devtools::load_all("."))
# library(eyewit)

# install CRAN packages
# install.packages("tidyverse")
library(tidyverse)

# import user interface
source("interface.R")

# read raw data filenames
participants <- list.files(interface$raw_dir)

# take a random sample from raw folder to determine vendor labels, and only read headers
sample <- readr::read_tsv(file.path(interface$raw_dir, sample(participants, 1)), col_types = readr::cols(), n_max = 0)
vendor <- vendor_check(sample)$vendor
types <- vendor_check(sample)$types

# incomplete subjects (i.e., not having 2 pretest & 12 test trials)
incomplete_subjets <- c()

# Loop over all participants
for (subject in participants) {

  print(subject)

  # remove later
  # subject <- participants[1]

  # read tsv files
  df_raw <- readr::read_tsv(file.path(interface$raw_dir, subject), col_types = types)

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

	################################
	######## OTHER MEASURES ########
	################################

	##### MEASURES BASED ON FIRST LOOK (FL) MEASURE

  df_subject$PrefLook_FL_Left <- get_looks(
  	df,
  	interface$aoisets$preflook,
  	startend_preflook,
  	omit_first_overflow_fi = TRUE,
  	first_look_emergency_cutoff =
  		round(
  			median(gazeshifts$aoiPrefLook$left$latencies) +
  				3 * sd(gazeshifts$aoiPrefLook$left$latencies)
  		)
  )$first_looks_collection$left$durations

	# FYI You can retrieve the ending_reasons (either outside)
	# df_subject$PrefLook_FL_Left <- get_looks(
	# 	df,
	# 	interface$aoisets$preflook,
	# 	startend_preflook,
	# 	omit_first_overflow_fi = TRUE,
	# 	first_look_emergency_cutoff =
	# 		round(
	# 			median(gazeshifts$aoiPrefLook$left$latencies) +
	# 				3 * sd(gazeshifts$aoiPrefLook$left$latencies)
	# 		)
	# )$first_looks_collection$ending_reasong

	df_subject$PrefLook_FL_Right <- get_looks(
		df,
		interface$aoisets$preflook,
		startend_preflook,
		omit_first_overflow_fi = TRUE,
		first_look_emergency_cutoff =
			round(
				median(gazeshifts$aoiPrefLook$right$latencies) +
					3 * sd(gazeshifts$aoiPrefLook$right$latencies)
			)
	)$first_looks_collection$right$durations

	df_subject$PrefLook_FL_Obj_Nov <- dplyr::if_else(
		df_subject$PrefLook_Obj_Nov_Pos == "left", df_subject$PrefLook_FL_Left, df_subject$PrefLook_FL_Right
	)

	df_subject$PrefLook_FL_Obj_Fam <- dplyr::if_else(
		df_subject$PrefLook_Obj_Fam_Pos == "left", df_subject$PrefLook_FL_Left, df_subject$PrefLook_FL_Right
	)

	df_subject$PrefLook_FL_Screen_Omit <- get_looks(
		df,
		interface$aoisets$screen,
		startend_preflook,
		omit_first_overflow_fi = TRUE,
		first_look_emergency_cutoff =
			round(
				median(gazeshifts$aoiScreen$onscreen$latencies) +
					3 * sd(gazeshifts$aoiScreen$onscreen$latencies)
			)
	)$first_looks_collection$onscreen$durations

	df_subject$PrefLook_FL_Screen_NoOmit <- get_looks(
		df,
		interface$aoisets$screen,
		startend_preflook,
		omit_first_overflow_fi = FALSE,
		first_look_emergency_cutoff =
			round(
				median(gazeshifts$aoiScreen$onscreen$latencies) +
					3 * sd(gazeshifts$aoiScreen$onscreen$latencies)
			)
	)$first_looks_collection$onscreen$durations

	# =========================================
	# Gap2FLScreen
	# returns either a time difference in ms or NA:
	# - NA means there was no fixation at all in this trial
	# - any numbers means the gap in ms until the initiation of the first screen fixation
	# (this should actually be a separate function, lol)

	# init with 0
	df_subject$Gap2FLScreen <- 0
	for (pfsi in seq.int(df_subject$Gap2FLScreen)) {

		# boolean that checks if there is a screen fixation at the first sample for each trial
		isAoiScreenFixAtFirstSample <- (df[startend_preflook$start[pfsi] + 1,"gazeType"] == "Fixation")[1] && (df[startend_preflook$start[pfsi] + 1,"aoiScreen"] == "onscreen")[1]

		# There are 2 cases,
		# (1) isAoiScreenFixAtFirstSample is FALSE that means there is no fixation when the trial starts
		# (2) isAoiScreenFixAtFirstSample is TRUE there is a fixation when the trial starts, which need to be shifted the next fixation (similar to the omit within get_looks)

		# init diff container with zeros
		time_diff <- 0
		starting_timestamp <- df[startend_preflook$start[pfsi],"timestamp"] |> as.integer()
		# init ending timestamp of first fixation
		ending_timestamp <- NA
		# if there is no initial fixation get the time in ms when the first fixation at screen appears
		# This is case (1)
		if (!isAoiScreenFixAtFirstSample) {

			# filter for fixations being onscreen
			temp <- df |>
				dplyr::slice(startend_preflook$start[pfsi]:startend_preflook$end[pfsi]) |>
				dplyr::filter(gazeType == "Fixation") |>
				dplyr::filter(aoiScreen == "onscreen")


			# check if there is any fixation in this trial at all, if not assign NA
			if (nrow(temp) != 0) {
				ending_timestamp <- temp$timestamp[1]
				time_diff <- ending_timestamp - starting_timestamp
			}
			if (nrow(temp) == 0) {
				time_diff <- NA
			}
		}

		# if true, jump the next fixation (i.e., omit the first ongoing fixation)
		# This is case (2)
		if (isAoiScreenFixAtFirstSample) {

			first_fi_to_skip <- df$fi[startend_preflook$start[pfsi]]
			next_fi_to_use <- first_fi_to_skip + 1
			next_fi_to_use_rn <- which(df$fi == next_fi_to_use)[1]

			current_trial_start_time <- df$timestamp[startend_preflook$start[pfsi]]
			current_next_fi_start_time <- df$timestamp[next_fi_to_use_rn]

			time_diff <- current_next_fi_start_time - current_trial_start_time
		}

		df_subject$Gap2FLScreen[pfsi] <- time_diff
	}

	############################################################

	# Orignal Idea (NoOmit might be problematic!):
	df_subject$PrefLook_FL_Screen_starttocutoff <- df_subject$PrefLook_FL_Screen_Omit + df_subject$Gap2FLScreen

	############################################################
	# TODO #####################################################
	############################################################
	# think about using Omit vs NoOmit
	# df_subject$PrefLook_FL_Screen_starttocutoff <- ifelse(df_subject$Gap2FLScreen == 0, df_subject$PrefLook_FL_Screen_NoOmit, df_subject$PrefLook_FL_Screen_Omit + df_subject$Gap2FLScreen)

	############################################################

	# init
	# iterate over all screen durations rowwise within df_subject
	df_subject$PrefLook_LT_Obj_Left_FL <- NA
	# iterate over all screen durations rowwise within df_subject
	for (i_screen_lt in seq.int(df_subject$PrefLook_FL_Screen_Omit)) {
		print(paste0("Index: ", i_screen_lt, " Screen Duration: ", df_subject$PrefLook_FL_Screen_Omit[i_screen_lt]))
		df_subject$PrefLook_LT_Obj_Left_FL[i_screen_lt] <- get_looks(
			df,
			interface$aoisets$preflook,
			startend_preflook,
			intra_scope_window = c(
				"start",
				ifelse(
					is.na(df_subject$Gap2FLScreen[i_screen_lt]),
					"end",
					df_subject$PrefLook_FL_Screen_starttocutoff[i_screen_lt]
				)
			),
			omit_first_overflow_fi = TRUE
		)$looking_times$left[i_screen_lt] # ... only get the i'th item from get_looks
	}

	# same for right
	df_subject$PrefLook_LT_Obj_Right_FL <- NA
	for (i_screen_lt in seq.int(df_subject$PrefLook_FL_Screen_Omit)) {
		print(paste0("Index: ", i_screen_lt, " Screen Duration: ", df_subject$PrefLook_FL_Screen_Omit[i_screen_lt]))
		df_subject$PrefLook_LT_Obj_Right_FL[i_screen_lt] <- get_looks(
			df,
			interface$aoisets$preflook,
			startend_preflook,
			intra_scope_window = c(
				"start",
				ifelse(
					is.na(df_subject$Gap2FLScreen[i_screen_lt]),
					"end",
					df_subject$PrefLook_FL_Screen_starttocutoff[i_screen_lt]
				)
			),
			omit_first_overflow_fi = TRUE
		)$looking_times$right[i_screen_lt] # ... only get the i'th item from get_looks
	}

	df_subject$PrefLook_LT_Obj_Nov_FL <- dplyr::if_else(
		df_subject$PrefLook_Obj_Nov_Pos == "left", df_subject$PrefLook_LT_Obj_Left_FL, df_subject$PrefLook_LT_Obj_Right_FL
	)

	df_subject$PrefLook_LT_Obj_Fam_FL <- dplyr::if_else(
		df_subject$PrefLook_Obj_Fam_Pos == "left", df_subject$PrefLook_LT_Obj_Left_FL, df_subject$PrefLook_LT_Obj_Right_FL
	)

	df_subject$PrefLook_PropSore_FL <- df_subject$PrefLook_LT_Obj_Nov_FL / (df_subject$PrefLook_LT_Obj_Nov_FL + df_subject$PrefLook_LT_Obj_Fam_FL)





	##### MEASURES BASED ON 2 SEC LOOK-AWAY MEASURE

	df_subject$PrefLook_2sec_Obj_Left <-
		get_looks(
			df = df,
			aoi_collection = interface$aoisets$preflook,
			scope = startend_preflook,
			lookaway_stop = 2000,
			omit_first_overflow_fi = TRUE)$lookaway_collection$left$durations

	df_subject$PrefLook_2sec_Obj_Right <-
		get_looks(
			df = df,
			aoi_collection = interface$aoisets$preflook,
			scope = startend_preflook,
			lookaway_stop = 2000,
			omit_first_overflow_fi = TRUE)$lookaway_collection$right$durations

	df_subject$PrefLook_2sec_Obj_Nov <- dplyr::if_else(
		df_subject$PrefLook_Obj_Nov_Pos == "left", df_subject$PrefLook_2sec_Obj_Left, df_subject$PrefLook_2sec_Obj_Right
	)

	df_subject$PrefLook_2sec_Obj_Fam <- dplyr::if_else(
		df_subject$PrefLook_Obj_Fam_Pos == "left", df_subject$PrefLook_2sec_Obj_Left, df_subject$PrefLook_2sec_Obj_Right
	)

	df_subject$PrefLook_2sec_Screen <-
		get_looks(
			df = df,
			aoi_collection = interface$aoisets$screen,
			scope = startend_preflook,
			lookaway_stop = 2000,
			omit_first_overflow_fi = TRUE)$lookaway_collection$onscreen$durations

	df_subject$PrefLook_2sec_Screen_starttocutoff <- ifelse(is.na(df_subject$PrefLook_2sec_Screen), NA, 10000)

	######## SYNTAX FOR fi_summary ########
	# fi_summary(df, AOI_COLLECTION, SCOPE/STARTEND/PHASE(i.e. preflook))
	########################################

	dataScreen <- fi_summary(df, interface$aoisets$screen, startend_preflook)
	dataPrefLook <- fi_summary(df, interface$aoisets$preflook, startend_preflook)
	# data <- fi_summary(df, interface$aoisets$preflook, startend_preflook) # example to use with screen aoi set
	# data <- fi_summary(df, interface$aoisets$preflook) # get summary for ALL fixation indexes regardless of phase

	# Filter for Gaps greater 2000 ms
	dataScreen <- dataScreen |> filter(FGapDurTrlEnd > 2000)
	dataPrefLook <- dataPrefLook |> filter(FGapDurTrlEnd > 2000)

	# only keep data from the first row for each trial (making trials unique)
	dataScreen <- dataScreen[!duplicated(dataScreen[ , "Trial"]),]
	dataPrefLook <- dataPrefLook[!duplicated(dataPrefLook[ , "Trial"]),]

	# remove rows that contain NA in Trial column (since there was no fixation at all)
	dataScreen <- dataScreen |> filter(!is.na(Trial))
	dataPrefLook <- dataPrefLook |> filter(!is.na(Trial))

	# data, may contain "NO EVAL" (future bugfix)
	# remove rows, if any, that contain "NO EVAL", as we only look for the preflook phase
	dataPrefLookNoEvalTrials <- dataPrefLook$Trial[which(dataPrefLook$aoiPrefLook == "NO EVAL")]
	if (length(dataPrefLookNoEvalTrials != 0)) {
		dataScreen <- dataScreen |> dplyr::filter(!Trial %in% dataPrefLookNoEvalTrials)
	}
	dataScreen$PrefLook_2sec_Screen_starttocutoff <- dataScreen$FIrtsE1 - df$timestamp[startend_preflook$start[dataScreen$Trial]]

	df_subject$PrefLook_2sec_Screen_starttocutoff[dataScreen$Trial] <- dataScreen$PrefLook_2sec_Screen_starttocutoff



	# init
	# iterate over all screen durations rowwise within df_subject
	df_subject$PrefLook_LT_Obj_Left_2sec <- NA
	# iterate over all screen durations rowwise within df_subject
	for (i_screen_lt in seq.int(df_subject$PrefLook_2sec_Screen)) {
		print(paste0("Index: ", i_screen_lt, " Screen Duration: ", df_subject$PrefLook_2sec_Screen[i_screen_lt]))
		df_subject$PrefLook_LT_Obj_Left_2sec[i_screen_lt] <- get_looks(
			df,
			interface$aoisets$preflook,
			startend_preflook,
			intra_scope_window = c(
				"start",
				ifelse(
					is.na(df_subject$Gap2FLScreen[i_screen_lt]),
					"end",
					df_subject$PrefLook_2sec_Screen_starttocutoff[i_screen_lt]
				)
			),
			omit_first_overflow_fi = TRUE
		)$looking_times$left[i_screen_lt] # ... only get the i'th item from get_looks
	}

	df_subject$PrefLook_LT_Obj_Right_2sec <- NA
	# iterate over all screen durations rowwise within df_subject
	for (i_screen_lt in seq.int(df_subject$PrefLook_2sec_Screen)) {
		print(paste0("Index: ", i_screen_lt, " Screen Duration: ", df_subject$PrefLook_2sec_Screen[i_screen_lt]))
		df_subject$PrefLook_LT_Obj_Right_2sec[i_screen_lt] <- get_looks(
			df,
			interface$aoisets$preflook,
			startend_preflook,
			intra_scope_window = c(
				"start",
				ifelse(
					is.na(df_subject$Gap2FLScreen[i_screen_lt]),
					"end",
					df_subject$PrefLook_2sec_Screen_starttocutoff[i_screen_lt]
				)
			),
			omit_first_overflow_fi = TRUE
		)$looking_times$right[i_screen_lt] # ... only get the i'th item from get_looks
	}

	df_subject$PrefLook_LT_Obj_Fam_2sec <- dplyr::if_else(
		df_subject$PrefLook_Obj_Fam_Pos == "left", df_subject$PrefLook_LT_Obj_Left_2sec, df_subject$PrefLook_LT_Obj_Right_2sec
	)

	df_subject$PrefLook_LT_Obj_Nov_2sec <- dplyr::if_else(
		df_subject$PrefLook_Obj_Nov_Pos == "left", df_subject$PrefLook_LT_Obj_Left_2sec, df_subject$PrefLook_LT_Obj_Right_2sec
	)

	df_subject$PrefLook_PropScore_2sec <- df_subject$PrefLook_LT_Obj_Nov_2sec / (df_subject$PrefLook_LT_Obj_Nov_2sec + df_subject$PrefLook_LT_Obj_Fam_2sec)



	# ================================================================================================
	# TIME-COURSE PLOT
	# ------------------------------------------------------------------------------------------------
	df_time <- timebinning(df, df_subject, startend_preflook, 500)
	# Sort like in the word file:
	df_time <- df_time |> dplyr::arrange(TrialCon, Condition, BinNumber)
	# Remove last bin
	last_time_bin <- df_time$BinNumber |> max()
	df_time <- df_time |> dplyr::filter(BinNumber != last_time_bin)


  # write tables for individual participants
  # write.table(df_subject, paste0(interface$output_dir, subject), sep = '\t', row.names = FALSE)
  # write.table(df_time, paste0(interface$output_dir, sub("\\.tsv$", "", subject), "_time.tsv"), sep = '\t', row.names = FALSE)
}

# Read in tsv files from pre-processing folder
# tsv_files <- list.files(interface$output_dir, full.names = TRUE)

# # Creating data frame
# overall.data <- tsv_files %>%
#   map(read_tsv) %>%    # read in all the files individually, using the function read_tsv() from the readr package
#   reduce(rbind)        # reduce with rbind into one dataframe
