#' Capitalize the first letter of a string
#' @param x A character string.
#' @return The input string with the first letter capitalized.
capitalise <- function(x) {
  substring(x, 1, 1) <- toupper(substring(x, 1, 1))
  x
}
