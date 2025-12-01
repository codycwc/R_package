#' Quick Summary Statistics for Numeric Columns
#'
#' Computes mean, median, sd, min, and max for all numeric variables in a data frame.
#'
#' @param data A data frame.
#'
#' @return A tidy tibble of summary statistics.
#' @examples
#' quick_describe(mtcars)
#' @export
quick_describe <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  numeric_cols <- dplyr::select_if(data, is.numeric)

  if (ncol(numeric_cols) == 0) {
    stop("No numeric columns found in `data`.")
  }

  dplyr::summarise_all(
    numeric_cols,
    list(
      mean = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
  ) |>
    tidyr::pivot_longer(
      everything(),
      names_to = c("variable", ".value"),
      names_sep = "_"
    )
}
