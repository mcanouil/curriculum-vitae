languages_section <- function(xlsx = "data/cv.xlsx", sheet = "languages") {
  # text <- read_excel_sheet(xlsx, sheet)[
  #   j = list(what = paste(
  #     paste(what[-length(what)], collapse = ", "),
  #     tail(what, 1),
  #     sep = " and "
  #   )),
  #   by = "level"
  # ][
  #   j = sprintf(
  #     '- <u style="color: var(--main-color);">*%s:*</u> %s',
  #     capitalise(level),
  #     what
  #   )
  # ]
  

  text <- read_excel_sheet(xlsx, sheet)[
    j = paste0(
      '- <u style="color: var(--main-color);">*', what, ':*</u> ', level
    )
  ]

  paste0("## Languages {#skills}\n\n", paste(text, collapse = "\n"), "\n\n")
}