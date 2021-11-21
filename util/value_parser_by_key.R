# usage:

# keys <- c("age", "sex", "location", "language")
# delimited_string <- "36_f_leipzig_german.mp4"
# delimiter = "_"
# drop_nth_delimiter = 2
# trim_right = 4

# location <- value_parser_by_key(keys, delimited_string, trim_right = 4)$language # german
# location <- value_parser_by_key(c("age"), delimited_string, drop_nth_delimiter = c(1,2,3))$age # 36fleipziggerman.mp4

value_parser_by_key <- function(keys, delimited_string, delimiter = "_", drop_nth_delimiter, trim_right) {

  # check argument
  if (!missing(trim_right)) {
    delimited_string <- str_sub(delimited_string, end = (trim_right + 1) * -1)
  }

  # check argumet
  if (!missing(drop_nth_delimiter)) {

    for (i in seq_along(drop_nth_delimiter)) {

      # delimiter positions change for each iteration
      delimiter_positions <- unlist(gregexpr(pattern = "_", delimited_string))

      current_pos <- drop_nth_delimiter[i]

      # check if user position is in range
      if (!current_pos %in% seq.int(delimiter_positions)) {
        stop(paste("There is no'", delimiter, "' at positon ", current_pos , ". Use relative indexing! E.g., 'How_are_you?' has only two underscores; refer to the first one with 1 and to the second with 2, to remove both use c(1,2)", sep = ""))
      }

      # remove delimiter at position
      delimited_string <- sub(sprintf("^(.{%s,%s}).", delimiter_positions[current_pos] - 1, delimiter_positions[current_pos] - 1), "\\1", delimited_string)

      # update drop_nth_delimiter
      drop_nth_delimiter <- drop_nth_delimiter - 1
    }
  }



  response <- tryCatch(
    expr = {
      # message("Assign keys to delimited values in all list items")
      # R is just sick: https://stackoverflow.com/a/61407360/2258480
      setNames(asplit(do.call(rbind, strsplit(delimited_string, delimiter)), 2), keys)
    },
    error = function(e) {
      message("error")
      print(e)
    },
    warning = function(w) {
      message("warning")
      print(w)
    }#, finally = {message("...")}
  )

  # check if key and value matchin length
  if (length(keys) != length(response)) {
    stop("Your key positions to not match with you value positions. Please check!")
  }

  return(response)
}
