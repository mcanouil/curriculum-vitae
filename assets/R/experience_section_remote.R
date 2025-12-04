experience_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  use_bullets = TRUE,
  reverse_order = TRUE
) {
  experience_data <- read_cv_data_remote(github_repo, "experience", branch)

  # Convert to data frame
  experience_df <- do.call(rbind, lapply(experience_data, as.data.frame))

  if (use_bullets) {
    # Use bullet points for activities
    bullet_char <- "\u25CF\u00A0" # Large bullet + non-breaking space

    experience_df$activities <- sapply(experience_df$activities, function(x) {
      open_bullet <- paste0(bullet_char, x)
      # Replace instances of '.' with bullet points
      pattern_replacement <- paste0(". \n\n", bullet_char)
      open_bullet <- gsub("\\.", pattern_replacement, open_bullet)
      # Remove the last instance
      final_pattern <- paste0("\\. \n\n", gsub("(\\W)", "\\\\\\1", bullet_char))
      open_bullet <- stringi::stri_replace_last(
        open_bullet,
        replacement = ".",
        regex = final_pattern
      )
      return(open_bullet)
    })

    # Create formatted text
    if (reverse_order) {
      # Reverse order - most recent first
      formatted_text <- sapply(nrow(experience_df):1, function(i) {
        paste0(
          "### ",
          experience_df[i, "position"],
          "\n\n",
          experience_df[i, "institute"],
          "\n\n",
          experience_df[i, "city"],
          "\n\n",
          experience_df[i, "start"],
          " - ",
          experience_df[i, "end"],
          "\n\n",
          "\n\n",
          experience_df[i, "activities"],
          "\n\n\n\n"
        )
      })
    } else {
      # Original order
      formatted_text <- sapply(1:nrow(experience_df), function(i) {
        paste0(
          "### ",
          experience_df[i, "position"],
          "\n\n",
          experience_df[i, "institute"],
          "\n\n",
          experience_df[i, "city"],
          "\n\n",
          experience_df[i, "start"],
          " - ",
          experience_df[i, "end"],
          "\n\n",
          "\n\n",
          experience_df[i, "activities"],
          "\n\n\n\n"
        )
      })
    }
  } else {
    # Activities in italics without bullet points
    if (reverse_order) {
      formatted_text <- sapply(nrow(experience_df):1, function(i) {
        paste0(
          "### ",
          experience_df[i, "position"],
          "\n\n",
          experience_df[i, "institute"],
          "\n\n",
          experience_df[i, "city"],
          "\n\n",
          experience_df[i, "start"],
          " - ",
          experience_df[i, "end"],
          "\n\n",
          "\n\n*",
          experience_df[i, "activities"],
          "*\n\n\n\n"
        )
      })
    } else {
      formatted_text <- sapply(1:nrow(experience_df), function(i) {
        paste0(
          "### ",
          experience_df[i, "position"],
          "\n\n",
          experience_df[i, "institute"],
          "\n\n",
          experience_df[i, "city"],
          "\n\n",
          experience_df[i, "start"],
          " - ",
          experience_df[i, "end"],
          "\n\n",
          "\n\n*",
          experience_df[i, "activities"],
          "*\n\n\n\n"
        )
      })
    }
  }

  # Return the result
  if (page_break_after) {
    c(
      "## Professional Experience {data-icon=laptop .break-after-me}",
      formatted_text
    )
  } else {
    c("## Professional Experience {data-icon=laptop}", formatted_text)
  }
}
