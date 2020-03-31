
#' Deconstruct a dataframe or list into vectors available in the global environment
#'
#' @param x a dataframe or list to deconstruct
#'
#' @export
#'
#' @examples
#' deconstruct(mtcars)
deconstruct <- function(x, envir) {
  . <- DataFrameToList(x)
  n. <- names(.)

  for (i in 1:length(.)) {
    assign(n.[i], .[[i]], envir = envir)
  }
}
