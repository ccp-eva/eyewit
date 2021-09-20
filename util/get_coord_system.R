get_coord_system <- function(df) {

  # rather use this to check?
  # if("d" %in% colnames(dat))
  # {
  #   cat("Yep, it's in there!\n");
  # }

  # check if there are multiple coordinate systems in df, specifically MCS and ADCS
  is_ADCS <- TRUE %in% grepl("GazePointX.*ADCS", names(df))
  is_MCS <- TRUE %in% grepl("GazePointX.*MCS", names(df))

  if (is_ADCS && is_MCS) {
    print("There are multiple coordinate systems in your dataframe. Which one should I use?")
    print("1 — ADCS (Active Display Coordinate System")
    print("2 — MCS (Media Coordinate System")
    n <- 0
    while (n < 1 || n > 2) {
      n <- readline("Select the number of the above systems: ")
      n <- ifelse(grepl("\\D", n), 0, as.integer(n))
    }

    return

  }
}
