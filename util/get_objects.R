get_objects <- function(df, fam_index_pairs) {

  # familar objects
  string_list <- as.vector(unique(df$StudioEventData[fam_index_pairs$start]))

  # function to extract the sequence "Obj_xxx" untile a period "." appears
  slice_object_string <- function(string) {
    gsub(".*Obj_(.*)\\..*", "\\1", c(string))
  }

  familiar_objects <- unlist(lapply(string_list, slice_object_string))

  # novel objects
  # helper function to extract last n right characters (vectorized)
  substrRight <- function(x, n){
    substr(x, nchar(x) - n + 1, nchar(x))
  }

  # to get novel objects just change the postfix letter of familiar from "a" to "b" or from "b" to "a"
  swap_ab <- function(string) {

    stringprefix <- substr(string, 1, nchar(string) - 1)
    stringpostfix <- substrRight(string, 1)

    if (stringpostfix == "a") {
      return(paste(stringprefix, "b", sep = ""))
    }

    if (stringpostfix == "b") {
      return(paste(stringprefix, "a", sep = ""))
    }
  }

  novel_objects <- unlist(lapply(familiar_objects, swap_ab))

  return(list(familiar = familiar_objects, novel = novel_objects))

}
