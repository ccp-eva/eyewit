# tasks checks if all mandatory columns exist
preflight <- function(df, cols) {

  # check if a mandatory columns is missing
  if (FALSE %in% (cols %in% names(df))) {
    stop(paste('The following column(s) are missing:', cols[which(cols %in% names(df) == FALSE)]))
  }

  # check if rownames are equal to a sequence of corresponding rownumbers
  if (!isTRUE((all.equal(as.numeric(rownames(df)), 1:nrow(df))))) {
    stop("The df is not in sequence. Do not remove any rows.")
  }


  return("No Errors")
}
