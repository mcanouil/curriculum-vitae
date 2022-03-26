read_excel_sheet <- function(xlsx, sheet) {
  data.table::setDT(readxl::read_xlsx(xlsx, sheet))[
    j = lapply(
      X = .SD,
      FUN = function(x) {
        x[is.na(x)] <- ""
        x
      }
    )
  ]
}
