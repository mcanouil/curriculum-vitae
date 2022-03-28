capitalise <- function(x) {
  substring(x, 1, 1) <- toupper(substring(x, 1, 1))
  x
}
