oral_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  oral_data <- read_cv_data_remote(github_repo, "oral_presentations", branch)

  # Reverse order (most recent first)
  oral_data <- oral_data[length(oral_data):1]

  # Create formatted entries - match actual YAML structure
  text <- sapply(
    oral_data,
    function(entry) {
      paste0(
        "### ",
        entry$title,
        "\n\n",
        entry$event,
        "\n\n",
        entry$location,
        "\n\n",
        entry$date,
        "\n\n",
        "::: aside\n",
        if (!is.na(entry$url) && entry$url != "") {
          add_item_logo(entry$url, colour, type = "presentation")
        } else {
          ""
        },
        "\n:::\n\n\n\n"
      )
    },
    USE.NAMES = FALSE
  )

  if (page_break_after) {
    c("## Presentations  {data-icon=comment-dots .break-after-me}", text)
  } else {
    c("## Presentations {data-icon=comment-dots}", text)
  }
}
