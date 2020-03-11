
#' Combine bounds for presentation
#'
#' @param df a dataframe or tibble.
#' @param label name of the new column.
#' @param low name of the column containing the lower bound of the confidence interval, default is NULL.
#' @param high name of the column containing the upper bound of the confidence interval, default is NULL.
#'
#' @details If both low and high are null, will try and search for the appropriate columns to combine.
#'
#' @export
combine_ci <- function(df, label, low = NULL, high = NULL) {

  if (is.null(low) && is.null(high)) {
    out <- tidyr::unite(df, !! label, contains("low"), contains("high"), sep = ", ")
    out <- dplyr::mutate(out, !! label := paste0("(", .data[[label]], ")"))
  } else {
    out <- tidyr::unite(df, !! label, low, high, sep = ", ")
    out <- dplyr::mutate(out, !! label := paste0("(", .data[[label]], ")"))
  }

  return(out)
}

#' Format digits to be a specific length
#'
#' @param x a numeric object.
#' @param n the number of digits to follow the decimal.
#'
#' @return a character representation of the original input with the formatted digits.
#' @export
#'
#' @examples
#' format_digits(1.34523435353, 2)
format_digits <- function(x, n) {
  format(round(as.double(x), digits = n), nsmall = n, trim = TRUE)
}

#' Wrap text in paranthesis
#'
#' @param x a character or numeric value.
#'
#' @return a character representation of the original input surrounded in parenthesis.
#' @export
#'
#' @examples
#' wrap_parenthesis(55)
wrap_parenthesis <- function(x) {
  paste0("(", x, ")")
}
