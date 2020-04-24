destructureString <- function(keys, values, delimiter = "_") {
  # R is just sick: https://stackoverflow.com/a/61407360/2258480
  response <- setNames(asplit(do.call(rbind, strsplit(values, delimiter)), 2), keys)

  # check if key and value matchin length
  if (length(keys) != length(response)) {
    warning("Your key positions to not match with you value positions. Please check!")
  }

  return(response)
}
