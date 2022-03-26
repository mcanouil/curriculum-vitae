experience_section <- function(xlsx = "data/cv.xlsx", sheet = "experience", page_break_after = FALSE) {
  text <- read_excel_sheet(xlsx, sheet) %>%
    dplyr::slice(dplyr::n():1) %>%
    glue::glue_data(.sep = "\n\n",
      "### {position}",
      "{institute}",
      "{city}",
      "{start} - {end}",
      "Activities: *{activities}*",
      "\n\n"
    )

  if (page_break_after) {
    c("## Professional Experience {data-icon=laptop .break-after-me}", text)
  } else {
    c("## Professional Experience {data-icon=laptop}", text)
  }
}
