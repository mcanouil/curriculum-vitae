skills_section <- function(xlsx = "data/cv.xlsx", sheet = "skills") {
  read_excel_sheet(xlsx, sheet) |>
    dplyr::group_by(level) |>
    dplyr::summarise(
      what = as.character(glue::glue_collapse(what, sep = ", ", last = " and ")),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = level, values_from = what) |>
    glue::glue_data(
      "## Computer Skills {{#skills}}",
      "\n\n",
      '- <u style="color: var(--main-color);">*Advanced:*</u> {advanced}',
      "\n",
      '- <u style="color: var(--main-color);">*Intermediate:*</u> {intermediate}',
      "\n",
      '- <u style="color: var(--main-color);">*Basic:*</u> {basic}',
      "\n\n"
    )
}
