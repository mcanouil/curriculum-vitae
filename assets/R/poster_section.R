poster_section <- function(xlsx = "data/cv.xlsx", sheet = "poster", page_break_after = FALSE, colour = "#333333") {
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = paste0(
      "### ", title, "\n\n",
      organiser, "\n\n",
      city, "\n\n",
      date, "\n\n",
      "::: aside\n", add_github_logo(url, colour), "\n:::\n\n\n\n"
    )
  ]

  if (page_break_after) {
    c(paste0("## Poster communications (", length(text), ") {data-icon=file .break-after-me}"), text)
  } else {
    c(paste0("## Poster communications (", length(text), ") {data-icon=file}"), text)
  }
}