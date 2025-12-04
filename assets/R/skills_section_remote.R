skills_section_remote <- function(github_repo = NULL, branch = "main") {
  skills_data <- read_cv_data_remote(github_repo, "skills", branch)

  # Convert list to data.table-like structure
  skills_df <- data.frame(
    subject = sapply(skills_data, function(x) x$subject),
    level = sapply(skills_data, function(x) x$level),
    stringsAsFactors = FALSE
  )

  # Group by level and create text
  levels <- c("advanced", "intermediate", "basic")
  text_parts <- character(0)

  for (level in levels) {
    subjects <- skills_df[skills_df$level == level, "subject"]
    if (length(subjects) > 0) {
      if (length(subjects) > 1) {
        what_text <- paste(
          paste(subjects[-length(subjects)], collapse = ", "),
          tail(subjects, 1),
          sep = " and "
        )
      } else {
        what_text <- subjects[1]
      }
      text_parts <- c(
        text_parts,
        paste0(
          '- <u style="color: var(--main-color);">*',
          capitalise(level),
          ':*</u> ',
          what_text
        )
      )
    }
  }

  paste0(
    "## Technical Skills {#skills}\n\n",
    paste(text_parts, collapse = "\n"),
    "\n\n"
  )
}
