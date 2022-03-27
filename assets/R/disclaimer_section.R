disclaimer_section <- function(text = NULL) {
  sprintf(
    "## Disclaimer {#disclaimer}\n\n%sLast updated on %s.\n\n",
    if (is.null(text)) "" else sprintf("%s\n\n", text),
    Sys.Date()
  )
}
