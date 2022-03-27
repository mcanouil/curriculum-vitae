title_section <- function(author = NULL) {
  c(
    "# Main",
    sprintf("## %s {{#title}}", author)
  )
}
