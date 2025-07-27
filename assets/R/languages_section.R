#' @description
#' Create a section for languages in a CV
#' @param xlsx Path to the Excel file containing the CV data
#' @param sheet Name of the sheet in the Excel file that contains language data
#' @return A formatted string for the languages section of a CV
languages_section <- function(
    xlsx = "data/cv.xlsx",
    sheet = "languages"
    ) {
  text <- read_excel_sheet(xlsx, sheet)[
    j = paste0(
      '- <u style="color: var(--main-color);">*', what, ':*</u> ', level
    )
  ]

  paste0("## Languages {#skills}\n\n", paste(text, collapse = "\n"), "\n\n")
}