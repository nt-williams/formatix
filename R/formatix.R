
#' Combine bounds for presentation.
#'
#' @param df a dataframe or tibble
#' @param label name of the new column
#'
#' @return
#' @export
combine_ci <- function(df, label) {
  out <- tidyr::unite(df, !! label, contains("low"), contains("high"), sep = ", ")
  out <- dplyr::mutate(out, !! label := paste0("(", .data[[label]], ")"))
  return(out)
}

#' Format digits to be a specific length.
#'
#' @param x a numeric object
#' @param n the number of digits to follow the decimal
#'
#' @return a character representation of the original input with the formatted digits
#' @export
#'
#' @examples
#' format_digits(1.34523435353, 2)
format_digits <- function(x, n) {
  format(round(as.double(x), digits = n), nsmall = n, trim = TRUE)
}

#' Wrap text in paranthesis.
#'
#' @param x a character or numeric value
#'
#' @return a character representation of the original input surrounded in parenthesis
#' @export
#'
#' @examples
#' wrap_parenthesis(55)
wrap_parenthesis <- function(x) {
  paste0("(", x, ")")
}
