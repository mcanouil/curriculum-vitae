awards_section <- function(xlsx = "data/cv.xlsx", sheet = "awards", page_break_after = FALSE, colour = "#333333") {
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = sprintf(
      "### %s\n\n%s\n\n%s\n\n%s\n\n%s\n\n::: aside\n%s\n:::\n\n\n\n",
      name, institute, city, date, description, add_github_logo(url, colour)
    )
  ]

  if (page_break_after) {
    c(sprintf("## Awards (%s) {{data-icon=trophy .break-after-me}}", length(text)), text)
  } else {
    c(sprintf("## Awards (%s) {{data-icon=trophy}}", length(text)), text)
  }
}
