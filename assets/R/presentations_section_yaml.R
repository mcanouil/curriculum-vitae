presentations_section_yaml <- function(
  yaml_file = "data/cv_data.yaml",
  page_break_after = FALSE,
  colour = "#333333"
) {
  # Read the YAML file
  cv_data <- yaml::read_yaml(yaml_file)

  # Extract presentations data
  presentations_data <- cv_data$presentations

  if (is.null(presentations_data)) {
    return(c("## Presentations {data-icon=comment-dots}", ""))
  }

  # Reverse order (most recent first)
  presentations_data <- presentations_data[length(presentations_data):1]

  # Create formatted entries
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
