profil_section <- function(xlsx = "data/cv.xlsx", sheet = "profil", use_headings = TRUE) {
  dt <- read_excel_sheet(xlsx, sheet)[show == 1]
  
  if (use_headings) {
    # Create heading levels with title
    dt[, level := vapply(
      X = level,
      FUN = function(.x) {
        paste(rep("#", each = as.numeric(.x) + 2), collapse = "")
      },
      FUN.VALUE = character(1)
    )]
    dt[, output := paste0(level, " ", title, "\n\n", paragraph, "\n\n")]
  } else {
    # No headings or titles, just paragraphs
    dt[, output := paste0(paragraph, "\n\n")]
  }
  
  return(dt$output)
}
