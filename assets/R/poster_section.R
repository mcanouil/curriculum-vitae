poster_section <- function(xlsx = "data/cv.xlsx", sheet = "poster", page_break_after = FALSE, colour = "#333333") {
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = sprintf(
      "### %s\n\n%s\n\n%s\n\n%s\n\n::: aside\n%s\n:::\n\n\n\n",
      title, organiser, city, date, add_github_logo(url, colour)
    )
  ]

  if (page_break_after) {
    c(sprintf("## Poster communications (%s) {data-icon=file .break-after-me}", length(text)), text)
  } else {
    c(sprintf("## Poster communications (%s) {data-icon=file}", length(text)), text)
  }
}
