languages_section_remote <- function(github_repo = NULL, branch = "main") {
  languages_data <- read_cv_data_remote(github_repo, "languages", branch)

  # Create language entries - match original format exactly
  text <- sapply(
    languages_data,
    function(lang) {
      paste0(
        '- <u style="color: var(--main-color);">*',
        lang$subject,
        ':*</u> ',
        lang$level
      )
    },
    USE.NAMES = FALSE
  )

  paste0("## Languages {#skills}\n\n", paste(text, collapse = "\n"), "\n\n")
}
