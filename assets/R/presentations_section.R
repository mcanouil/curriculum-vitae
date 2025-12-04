presentations_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  presentations_data <- read_cv_data_remote(
    github_repo,
    "presentations",
    branch
  )

  # Reverse order (most recent first)
  presentations_data <- presentations_data[length(presentations_data):1]

  # Create formatted entries - match actual YAML structure
  text <- sapply(
    presentations_data,
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
        if (!is.null(entry$url) && !is.na(entry$url) && entry$url != "") {
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
