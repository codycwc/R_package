#' Clean Column Names
#'
#' Converts column names to lowercase and replaces non-alphanumeric characters.
#'
#' @param data A data frame.
#' @return A data frame with cleaned column names.
#' @examples
#' clean_names(data.frame("First Name" = 1, "Value-2" = 3))
#' @export
clean_names <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  new_names <- tolower(names(data))
  new_names <- gsub("[^a-z0-9]+", "_", new_names)
  new_names <- gsub("_+", "_", new_names)
  new_names <- gsub("^_|_$", "", new_names)

  names(data) <- new_names
  data
}
