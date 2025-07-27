disclaimer_section <- function(text = NULL) {
  paste0(
    "## Disclaimer {#disclaimer style='width: var(--sidebar-width); padding-left: var(--sidebar-horizontal-padding);'}\n\n\n\n",
    if (is.null(text)) "" else paste0(text, "\n\n"),
    "Last updated on ", Sys.Date(), ".\n\n"
  )
}