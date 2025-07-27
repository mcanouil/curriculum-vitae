oral_section <- function(
    xlsx = "data/cv.xlsx",
    sheet = "oral",
    page_break_after = FALSE,
    colour = "#333333"
    ) {
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = paste0(
      "### ", title, "\n\n",
      organiser, "\n\n",
      city, "\n\n",
      date, "\n\n",
      "::: aside\n", add_item_logo(url, colour, type = "presentation"), "\n:::\n\n\n\n"
    )
  ]

  if (page_break_after) {
    c("## Presentations  {data-icon=comment-dots .break-after-me}", text)
  } else {
    c("## Presentations {data-icon=comment-dots}", text)
  }
}