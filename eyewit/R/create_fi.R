#' Create a Fixation Index Column based on gaze type and gaze type index
#'
#' @description
#' eyewit requires a FixationIndex column (i.e., `fi`) in which fixation indexes (and only
#' fixation indexes) are in a sequential order (i.e., 1, 2, 3, NA, NA, 4, 5, ...).
#'
#' @details
#' Tobii Studio had a FixationIndex column on which eyewit heavily relies on. Since Tobii ProLab
#' put that information in two columns: `Eye movement type` and `Eye movement type index`, we need
#' to filter `Eye movement type` for "Fixation" only to create a new column `FixationIndex` with
#' the corresponding values from `Eye movement type index`.
#'
#' @param df raw dataframe
#' @param type_col the column name of the column that specifies gaze type (fixation, saccaded, etc)
#' @param type_index_col the column name containing indexes for the specific type_col
#'
#' @return df including a FixationIndex column
#' @export
create_fi <- function(df, type_col, type_index_col) {
	if ("FixationIndex" %in% names(df)) {
		cat("FixationIndex is already present, doing nothing...")
		return(df)
	}

	df$FixationIndex <- ifelse(df[[type_col]] == "Fixation", df[[type_index_col]], NA)

	return(df)
}
