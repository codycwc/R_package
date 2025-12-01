#' Summarize Missing Values in a Data Frame
#'
#' Counts missing values in each column of a data frame.
#'
#' @param data A data frame.
#'
#' @return A tibble with columns `column` and `missing_count`.
#' @examples
#' df <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
#' summarize_missing(df)
#' @export
summarize_missing <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }
  dplyr::tibble(
    variable = names(data),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    pct_missing = sapply(data, function(x) mean(is.na(x))) * 100
  )
}
