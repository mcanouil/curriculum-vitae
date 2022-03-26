workshop_section <- function(xlsx = "data/cv.xlsx", sheet = "workshop", page_break_after = FALSE, colour = "#333333") {
  text <- read_excel_sheet(xlsx, sheet) |>
    dplyr::slice(dplyr::n():1) |>
    glue::glue_data(.sep = "\n\n",
      "### {title}",
      "{type}",
      "{city}",
      "{date}",
      "::: aside\n{add_github_logo(url, colour)}\n:::",
      "\n\n"
    )

  if (page_break_after) {
    c(glue::glue("## Workshop Experience ({length(text)}) {{data-icon=chalkboard-teacher .break-after-me}}"), text)
  } else {
    c(glue::glue("## Workshop Experience ({length(text)}) {{data-icon=chalkboard-teacher}}"), text)
  }
}
