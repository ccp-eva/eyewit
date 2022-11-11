#' vendor_check
#'
#' @param df Your raw dataframe
#'
#' @return Returns a `vendor_lookup` key as string
#' @export
#'
vendor_check <- function(df) {

	# extract x/y coordinate labels from all vendors, which will serve as an id for now
	vendor_x <- c()
	vendor_y <- c()

	for (i in vendor_lookup) {
		vendor_x <- c(vendor_x, names(i$x))
		vendor_y <- c(vendor_y, names(i$y))
	}

	target_vendor_x <- which(vendor_x %in% names(df))
	target_vendor_y <- which(vendor_y %in% names(df))

	# check if there is one and only one string match within the column names
	stopifnot(length(target_vendor_x) == 1)

	# target_vendor_x and target_vendor_y must share the same TRUE index
	stopifnot(target_vendor_x == target_vendor_y)


	# set-up col type specification based on vendor_lookup
	vendor_types <- c()
	for (key in vendor_lookup[[target_vendor_x]]) {
		vendor_types <- c(vendor_types, key)
	}


	return(
		list(
			vendor = names(vendor_lookup)[target_vendor_x],
			types = vendor_types
		)
	)
}


