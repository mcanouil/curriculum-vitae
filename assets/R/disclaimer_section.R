disclaimer_section <- function(text = NULL) {
  sprintf(
    "## Disclaimer {{#disclaimer}}\n\n%sLast updated on {Sys.Date()}.\n\n",
    if (is.null(text)) "" else sprintf("%s\n\n", text)
  )
}
