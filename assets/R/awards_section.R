awards_section <- function(xlsx = "data/cv.xlsx", sheet = "awards", page_break_after = FALSE) {
  text <- read_excel_sheet(xlsx, sheet) |>
    dplyr::slice(dplyr::n():1) |>
    glue::glue_data(.sep = "\n\n",
      "### {name}",
      "{institute}",
      "{city}",
      "{date}",
      "{description}",
      "::: aside\n{add_github_logo(url)}\n:::",
      "\n\n"
    )

  if (page_break_after) {
    c(glue::glue("## Awards ({length(text)}) {{data-icon=trophy .break-after-me}}"), text)
  } else {
    c(glue::glue("## Awards ({length(text)}) {{data-icon=trophy}}"), text)
  }
}
