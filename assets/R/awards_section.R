awards_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  awards_data <- read_cv_data_remote(github_repo, "awards", branch)

  # Convert to data frame and reverse order
  awards_df <- do.call(rbind, lapply(awards_data, as.data.frame))
  awards_df <- awards_df[nrow(awards_df):1, ]

  # Create formatted Markdown strings
  text <- mapply(
    function(name, institute, city, date, description, url, link_type) {
      paste0(
        "### ",
        name,
        "\n\n",
        institute,
        "\n\n",
        city,
        "\n\n",
        date,
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
    awards_df$name,
    awards_df$institute,
    awards_df$city,
    awards_df$date,
    awards_df$description,
    awards_df$url,
    awards_df$link_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  if (page_break_after) {
    c("## Grants & Funding {data-icon=trophy .break-after-me}", text)
  } else {
    c("## Grants & Funding  {data-icon=trophy}", text)
  }
}
