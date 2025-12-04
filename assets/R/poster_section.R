poster_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  posters_data <- read_cv_data_remote(github_repo, "posters", branch)

  # Reverse order (most recent first)
  posters_data <- posters_data[length(posters_data):1]

  # Create formatted entries - match actual YAML structure and original format
  text <- sapply(
    posters_data,
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
          add_github_logo(entry$url, colour)
        } else {
          ""
        },
        "\n:::\n\n\n\n"
      )
    },
    USE.NAMES = FALSE
  )

  if (page_break_after) {
    c(
      paste0(
        "## Poster communications (",
        length(text),
        ") {data-icon=file .break-after-me}"
      ),
      text
    )
  } else {
    c(
      paste0("## Poster communications (", length(text), ") {data-icon=file}"),
      text
    )
  }
}
