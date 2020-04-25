# tasks checks if all mandatory columns exist
preflight <- function(df, cols) {
  # check if a mandatory columns is missing
  if (FALSE %in% (cols %in% names(df))) {
    stop(paste('The following column(s) are missing:', cols[which(cols %in% names(df) == FALSE)]))
  }

  return("healthy")
}
