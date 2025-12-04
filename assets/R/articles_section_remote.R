articles_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  publications_data <- read_cv_data_remote(github_repo, "publications", branch)

  # Convert to data frame and reverse order
  publications_df <- do.call(rbind, lapply(publications_data, as.data.frame))
  publications_df <- publications_df[nrow(publications_df):1, ]

  # Create formatted entries
  text <- mapply(
    function(title, authors, journal, year, description, url, link_type) {
      paste0(
        "### ",
        title,
        "\n\n",
        authors,
        "\n\n",
        journal,
        "\n\n",
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
    publications_df$title,
    publications_df$authors,
    publications_df$journal,
    publications_df$year,
    publications_df$description,
    publications_df$url,
    publications_df$link_type,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  if (page_break_after) {
    c("## Publications  {data-icon=newspaper .break-after-me}", text)
  } else {
    c("## Publications  {data-icon=newspaper}", text)
  }
}
