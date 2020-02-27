
#' @export
new_glue <- function(..., stnc) {
  dots <- list(...)
  n <- length(dots)
  for (i in 1:n) {
    assign(names(dots[i]), dots[[i]])
  }
  eval(as.call(list(call("::",
                         pkg = substitute(glue),
                         name = substitute(glue)),
                    stnc)))
}

#' Function generator for constructing interpretations
#'
#' @param stnc The sentence to be used for constructing interpretations. Values are dynamically changed through \code{glue::glue()}.
#'
#' @return A character string of the constructed interpretation.
#' @export
#'
#' @examples
#' new_interpretation <- interpreter("My p-value is {P}")
#' new_interpretation(P = 0.03)
interpreter <- function(stnc) {
  body <- as.call(list(call("::",
                            pkg = substitute(formatix),
                            name = substitute(new_glue)),
                       substitute(...),
                       stnc = stnc))
  arg <- alist(... = )
  as.function(c(arg, body))
}

