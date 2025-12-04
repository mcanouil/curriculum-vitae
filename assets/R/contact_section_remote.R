#' Contact Section (Remote)
#' #' @description
#' This function generates a contact section for a CV in R Markdown format using remote YAML data.
#' @param github_repo GitHub repository in format "owner/repo".
#' @param branch Branch name (default is "main").
#' @param colour The color of the icons (default is "#333333").
#' @return A character vector containing the formatted contact section.
contact_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  colour = "#333333"
) {
  contact_data <- read_cv_data_remote(github_repo, "contact", branch)

  paste0(
    "## Contact Info {#contact}\n",
    "- ",
    fontawesome::fa("user", fill = colour),
    " ",
    contact_data$position,
    "\n",
    "- ",
    fontawesome::fa("building-columns", fill = colour),
    " ",
    contact_data$institute,
    "\n",
    "- ",
    fontawesome::fa("map-location-dot", fill = colour),
    " ",
    contact_data$city,
    "\n",
    "- ",
    fontawesome::fa("envelope", fill = colour),
    " [",
    gsub("\\.", "[dot]", sub("@", "[at]", contact_data$email)),
    "](mailto:",
    contact_data$email,
    ")\n",
    "- ",
    fontawesome::fa("house", fill = colour),
    " [",
    sub("/$", "", sub("https*://", "", contact_data$website)),
    "](",
    contact_data$website,
    ")\n",
    "- ",
    fontawesome::fa("orcid", fill = colour),
    " [",
    contact_data$orcid,
    "](https://orcid.org/",
    contact_data$orcid,
    ")\n",
    "- ",
    fontawesome::fa("linkedin", fill = colour),
    " [",
    contact_data$linkedin,
    "](https://www.linkedin.com/in/",
    contact_data$linkedin,
    ")\n",
    "- ",
    fontawesome::fa("github", fill = colour),
    " [",
    contact_data$github,
    "](https://github.com/",
    contact_data$github,
    ")\n",
    "- ",
    fontawesome::fa("x-twitter", fill = colour),
    " [",
    contact_data$twitter,
    "](https://twitter.com/",
    contact_data$twitter,
    ")\n",
    "- ",
    fontawesome::fa("researchgate", fill = colour),
    " [",
    contact_data$researchgate,
    "](",
    contact_data$researchgate,
    ")\n",
    "\n"
  )
}
