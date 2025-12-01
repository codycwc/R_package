#' Detect Outliers in Numeric Data
#'
#' Identifies outliers using the 1.5 * IQR rule.
#'
#' @param x A numeric vector.
#'
#' @return A list with outliers and thresholds.
#' @examples
#' debug_outliers(c(1, 2, 100, 3, 2, 150))
#' @export
debug_outliers <- function(x) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  tryCatch(
    {
      q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1

      outliers <- x[x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)]

      list(
        outliers = outliers,
        message = "Outlier detection successfully completed."
      )
    },
    error = function(e) {
      message("An error occured: ", e$message)
      return(NULL)
    }
  )
}
