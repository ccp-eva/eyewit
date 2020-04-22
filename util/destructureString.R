destructureString <- function(string, lut, delimiter = "_") {
  destructured <- unlist(strsplit(string, split = delimiter))
  if (length(lut) != length(destructured)) {
    warning("The provided name mapping does not match with the string!")
  }
  return(setNames(as.list(destructured), lut))
}
