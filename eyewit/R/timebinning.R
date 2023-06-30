#' Time Binning
#'
#' Returns binned looking times for an individual subject
#'
#' @param df A dataframe
#' @param scope A starend list to determine the length in ms for each trial
#' @param bin_width The size of the bin
#' @param df_subject A dedicated df for an individual subject, containing a "Condition" column
#'
#' @return A df for an individual subject
#' @export
#'
#' @examples
#' \dontrun{
#' }
timebinning <- function(df, df_subject, scope, bin_width = 500) {

	# todo a better(?) implementation/function signature might be: script.R: timebinning(df, df_subject, max_length_of_media_in_ms, bin_width)

	# get the average length of scope in ms
	scope_length <- c()
	for (i in seq.int(scope$start)) {
		scope_length <- c(scope_length, (df[scope$end[i], "timestamp"] - df[scope$start[i], "timestamp"])[1,1])
	}

	# this is needed for the last (residual) bin
	scope_length <- scope_length |> mean() |> round()

	# create time intervals based on bin_width and scope_length
	bins <- list(
		start = seq(1, scope_length, bin_width),
		end = seq(bin_width, scope_length, bin_width)
	)
	# replace 1 as start value with 0
	bins$start[1] <- 0

	# check if end contains the last bin (which is smaller than actual bin size as it contains the rest)
	if (!scope_length %in% bins$end) {
		# add it
		bins$end <- c(bins$end, scope_length)
	}

	# emergency check. they mus be equal
	stopifnot(length(bins$start) == length(bins$end))

	# ensure df_subject is sorted along trials
	stopifnot(df_subject$TrialRun == 1:nrow(df_subject))

	bin_length <- length(bins$start)
	df_subject_length <- nrow(df_subject)

	# Create the df structure for subject
	df_time <- tibble::tibble(.rows = bin_length * df_subject_length)
	df_time$ID <- df_subject$ID[1]
	df_time$BinNumber <- rep(1:bin_length, df_subject_length)
	df_time$BinRange <- rep(paste0(bins$start, "-", bins$end), df_subject_length)
	df_time$TrialRun <- rep(1:df_subject_length, each = bin_length)

	# init Condition and TrialCon, etc (they will be overwritten when iterating over df_subject)
	df_time$Condition <- NA
	df_time$TrialCon <- NA
	df_time$Fam_Pos <- NA
	df_time$Nov_Pos <- NA
	df_time$PrefLook_LT_Obj_Left <- NA
	df_time$PrefLook_LT_Obj_Right <- NA



	# iterate over df_subject to get Condition, TrialCondition, Object positions
	for (current_trial in seq.int(df_subject$TrialRun)) {
		current_start_i <- bin_length * current_trial - (bin_length - 1)
		current_end_i <- bin_length * current_trial
		current_condition <- df_subject$Condition[current_trial]
		current_trialcondition <- df_subject$TrialCon[current_trial]
		current_fam_pos <- df_subject$PrefLook_Obj_Fam_Pos[current_trial]
		current_nov_pos <- df_subject$PrefLook_Obj_Nov_Pos[current_trial]

		# feed in df_time
		df_time$Condition[current_start_i:current_end_i] <- current_condition
		df_time$TrialCon[current_start_i:current_end_i] <- current_trialcondition
		df_time$Fam_Pos[current_start_i:current_end_i] <- current_fam_pos
		df_time$Nov_Pos[current_start_i:current_end_i] <- current_nov_pos
	}

	# iterative over all time bins
	# You want to omit overflowing fixation indexes to get a sharp cut at the exact time positions (i.e., use omit_first_overflow_fi = FALSE and use: intra_scope_cut = TRUE)
	for (bin_index in seq.int(bins$start)) {
		current_bin_start <- bins$start[bin_index]
		current_bin_end <- bins$end[bin_index]
		bin_indexes_over_all_trials <- which(df_time$BinNumber == bin_index)
		df_time$PrefLook_LT_Obj_Left[bin_indexes_over_all_trials] <- get_looks(df, interface$aoisets$preflook, scope, c(current_bin_start,current_bin_end), omit_first_overflow_fi = FALSE)$looking_times$left
		df_time$PrefLook_LT_Obj_Right[bin_indexes_over_all_trials] <- get_looks(df, interface$aoisets$preflook, scope, c(current_bin_start,current_bin_end), omit_first_overflow_fi = FALSE)$looking_times$right
	}

	df_time$PrefLook_LT_Obj_Fam <- dplyr::if_else(df_time$Fam_Pos == "left", df_time$PrefLook_LT_Obj_Left, df_time$PrefLook_LT_Obj_Right)
	df_time$PrefLook_LT_Obj_Nov <- dplyr::if_else(df_time$Nov_Pos == "left", df_time$PrefLook_LT_Obj_Left, df_time$PrefLook_LT_Obj_Right)

	df_time$PrefLook_PropScore <- df_time$PrefLook_LT_Obj_Nov / (df_time$PrefLook_LT_Obj_Fam + df_time$PrefLook_LT_Obj_Nov)


	return(df_time)
}
