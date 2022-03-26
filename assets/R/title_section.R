title_section <- function(author = NULL) {
  c(
    "# Main",
    glue::glue("## {author} {{#title}}")
  )
}
