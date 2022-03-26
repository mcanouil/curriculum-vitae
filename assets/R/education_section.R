education_section <- function(xlsx = "data/cv.xlsx", sheet = "education", page_break_after = FALSE) {
  text <- read_excel_sheet(xlsx, sheet) |>
    dplyr::slice(dplyr::n():1) |>
    glue::glue_data(.sep = "\n\n",
      "### {degree}",
      "{university}",
      "{city}",
      "{start} - {end}",
      "{description}",
      "\n\n"
    )

  if (page_break_after) {
    c("## Education {data-icon=graduation-cap data-concise=true .break-after-me}", text)
  } else {
    c("## Education {data-icon=graduation-cap data-concise=true}", text)
  }
}
