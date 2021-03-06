% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatix.R
\name{format_digits}
\alias{format_digits}
\title{Format digits to be a specific length}
\usage{
format_digits(x, n)
}
\arguments{
\item{x}{a numeric object.}

\item{n}{the number of digits to follow the decimal.}
}
\value{
a character representation of the original input with the formatted digits.
}
\description{
Format digits to be a specific length
}
\examples{
format_digits(1.34523435353, 2)
}
