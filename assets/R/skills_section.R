skills_section <- function(xlsx = "data/cv.xlsx", sheet = "skills") {
  text <- read_excel_sheet(xlsx, sheet)[
    j = list(what = paste(
      paste(what[-length(what)], collapse = ", "),
      tail(what, 1),
      sep = " and "
    )),
    by = "level"
  ][
    j = paste0(
      '- <u style="color: var(--main-color);">*', capitalise(level), ':*</u> ', what
    )
  ]

  paste0("## Technical Skills {#skills}\n\n", paste(text, collapse = "\n"), "\n\n")
}