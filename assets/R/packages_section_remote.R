packages_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  packages_data <- read_cv_data_remote(github_repo, "packages", branch)

  # Convert to data frame and reverse order
  packages_df <- do.call(rbind, lapply(packages_data, as.data.frame))
  packages_df <- packages_df[nrow(packages_df):1, ]

  # Create formatted entries
  text <- mapply(
    function(name, description, year, url, link_type) {
      paste0(
        "### ",
        name,
        "\n\n",
        "N/A\n\n", # For consistency with other sections
        "N/A\n\n", # For consistency with other sections
        year,
        "\n\n",
        if (!is.na(description) && description != "") {
          paste0("*", description, "*\n\n")
        } else {
          ""
        },
        "::: aside\n",
        if (!is.na(url) && url != "") {
          add_item_logo(url, type = link_type, colour)
        } else {
          ""
        },
        "\n:::\n\n\n\n"
      )
    },
    packages_df$name,
    packages_df$description,
    packages_df$year,
    packages_df$url,
    packages_df$link_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  if (page_break_after) {
    c(
      paste0(
        "## R Packages (",
        length(text),
        ") {data-icon=code .break-after-me}"
      ),
      text
    )
  } else {
    c(paste0("## R Packages (", length(text), ") {data-icon=code}"), text)
  }
}
