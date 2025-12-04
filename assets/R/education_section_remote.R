education_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  include_sections = c("description", "thesis", "awards")
) {
  education_data <- read_cv_data_remote(github_repo, "education", branch)

  # Convert to data frame and reverse order (newest first)
  education_df <- do.call(rbind, lapply(education_data, as.data.frame))
  education_df <- education_df[nrow(education_df):1, ]

  # Build strings for each education entry - exactly like the original
  strings <- apply(education_df, 1, function(x) {
    # Build base string
    result_string <- paste0(
      "### ",
      x["degree"],
      "\n\n",
      x["university"],
      "\n\n",
      x["city"],
      "\n\n",
      x["start"],
      " - ",
      x["end"],
      "\n\n"
    )

    if ("description" %in% include_sections) {
      if (!is.na(x["description"]) && x["description"] != "") {
        result_string <- paste0(result_string, x["description"], "\n\n")
      }
    }

    if ("thesis" %in% include_sections) {
      if (!is.na(x["thesis"]) && x["thesis"] != "") {
        result_string <- paste0(
          result_string,
          "Thesis: *",
          x["thesis"],
          "*\n\n"
        )
      }
    }

    if ("awards" %in% include_sections) {
      if (!is.na(x["awards"]) && x["awards"] != "") {
        result_string <- paste0(
          result_string,
          "Awards: *",
          x["awards"],
          "*\n\n"
        )
      }
    }

    result_string <- paste0(result_string, "\n\n")

    return(result_string)
  })

  if (page_break_after) {
    c(
      "## Education {data-icon=graduation-cap data-concise=true .break-after-me}",
      strings
    )
  } else {
    c("## Education {data-icon=graduation-cap data-concise=true}", strings)
  }
}
