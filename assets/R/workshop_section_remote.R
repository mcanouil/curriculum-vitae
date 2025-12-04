workshop_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  workshops_data <- read_cv_data_remote(github_repo, "workshops", branch)

  # Create formatted entries - match the original format exactly
  text <- sapply(
    workshops_data,
    function(entry) {
      # Construct date string based on availability
      date_str <- if (!is.na(entry$start) && entry$start != "") {
        if (!is.na(entry$end) && entry$end != "") {
          paste0(entry$start, " - ", entry$end, "\n\n")
        } else {
          paste0(entry$start, "\n\n")
        }
      } else {
        "" # No start date, don't include anything
      }

      paste0(
        "### ",
        entry$title,
        "\n\n",
        "*",
        entry$description,
        "*\n\n",
        entry$location,
        "\n\n",
        date_str,
        "::: aside\n",
        if (!is.na(entry$url) && entry$url != "") {
          add_item_logo(entry$url, type = "website", colour)
        } else {
          ""
        },
        "\n:::\n\n\n\n"
      )
    },
    USE.NAMES = FALSE
  )

  heading <- "## Teaching, Training & Workshop Delivery  {data-icon=chalkboard-teacher"
  if (page_break_after) {
    heading <- paste0(heading, " .break-after-me")
  }
  heading <- paste0(heading, "}")

  c(heading, text)
}
