#' vendor_check
#'
#' @param df Your raw dataframe
#'
#' @return Returns a `vendor_lookup` key as string
#' @export
#'
vendor_check <- function(df) {
	p_color <- crayon::make_style("white")
	p_weight <- crayon::make_style("bold")
	p_bg <- crayon::make_style(grDevices::rgb(0, 0.42, 0.40), bg = TRUE)
	p_h1 <- crayon::combine_styles(p_color, p_weight, p_bg)
	p_text <- crayon::combine_styles(p_color, p_bg)
	cat(p_h1("\tVendor/Software: "))

	# extract x/y coordinate labels from all vendors, which will serve as an id for now
	vendor_x <- c()
	vendor_y <- c()
	for (i in vendor_lookup) {
		vendor_x <- c(vendor_x, i$x)
		vendor_y <- c(vendor_y, i$y)
	}

	target_vendor_x <- which(vendor_x %in% names(df))
	target_vendor_y <- which(vendor_y %in% names(df))

	# check if there is one and only one string match within the column names
	stopifnot(length(target_vendor_x) == 1)

	# target_vendor_x and target_vendor_y must share the same TRUE index
	stopifnot(target_vendor_x == target_vendor_y)

	return(names(vendor_lookup)[target_vendor_x])
}


