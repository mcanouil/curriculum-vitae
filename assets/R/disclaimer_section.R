disclaimer_section <- function(text = NULL) {
  glue::glue(
    "## Disclaimer {{#disclaimer}}",
    if (is.null(text)) "\n\n" else "\n\n{text}\n\n",
    "Last updated on {Sys.Date()}.\n\n"
  )
}
