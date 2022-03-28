education_section <- function(xlsx = "data/cv.xlsx", sheet = "education", page_break_after = FALSE) {
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = sprintf(
      "### %s\n\n%s\n\n%s\n\n%s - %s\n\n%s\n\n\n\n",
      degree, university, city, start, end, description
    )
  ]

  if (page_break_after) {
    c("## Education {data-icon=graduation-cap data-concise=true .break-after-me}", text)
  } else {
    c("## Education {data-icon=graduation-cap data-concise=true}", text)
  }
}
