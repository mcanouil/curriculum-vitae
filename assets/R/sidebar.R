sidebar <- function(
  png = "pictures/cv.png",
  github_repo = NULL,
  branch = "main"
) {
  # Load configuration if no repo specified
  if (is.null(github_repo)) {
    config <- yaml::read_yaml("config/cv_config.yaml")
    github_repo <- config$cv_data_repo
    branch <- config$cv_data_branch
  }

  # Generate sections using functions
  contact <- contact_section(github_repo, branch)
  skills <- skills_section(github_repo, branch)
  languages <- languages_section(github_repo, branch)
  disclaimer <- disclaimer_section()

  paste0(
    "<div class='sidebar-container'>\n",
    "<div class='sidebar-section'>\n",
    "<img src='",
    png,
    "' alt='Profile Picture' class='profile-image'>\n",
    "</div>\n",
    "<div class='sidebar-section'>\n",
    contact,
    "\n",
    "</div>\n",
    "<div class='sidebar-section'>\n",
    skills,
    "\n",
    "</div>\n",
    "<div class='sidebar-section'>\n",
    languages,
    "\n",
    "</div>\n",
    "<div class='sidebar-section'>\n",
    disclaimer,
    "\n",
    "</div>\n",
    "</div>\n"
  )
}
