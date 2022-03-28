skills_section <- function(xlsx = "data/cv.xlsx", sheet = "skills") {
  text <- read_excel_sheet(xlsx, sheet)[
    j = list(what = paste(
      paste(what[-length(what)], collapse = ", "),
      tail(what, 1),
      sep = " and "
    )),
    by = "level"
  ][
    j = sprintf(
      '- <u style="color: var(--main-color);">*%s:*</u> %s',
      capitalise(level),
      what
    )
  ]

  sprintf("## Computer Skills {#skills}\n\n%s\n\n", paste(text, collapse = "\n"))
}
