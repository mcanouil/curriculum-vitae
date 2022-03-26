profil_section <- function(xlsx = "data/cv.xlsx", sheet = "profil") {
  read_excel_sheet(xlsx, sheet)[show == 1][
    j = level := vapply(
      X = level,
      FUN = function(.x) {
        paste(rep("#", each = as.numeric(.x) + 2), collapse = "")
      },
      FUN.VALUE = character(1)
    )
  ][
    j = sprintf("%s %s\n\n%s\n\n", level, title, paragraph)
  ]
}
