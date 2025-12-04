profil_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  use_headings = TRUE
) {
  profile_data <- read_cv_data_remote(github_repo, "profile", branch)

  # The profile data is a list with one item containing text
  text <- profile_data[[1]]$text

  # Simply return the text with appropriate spacing
  return(paste0(text, "\n\n"))
}
