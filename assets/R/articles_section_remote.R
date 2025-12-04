articles_section_remote <- function(
  github_repo = NULL,
  branch = "main",
  page_break_after = FALSE,
  colour = "#333333"
) {
  publications_data <- read_cv_data_remote(github_repo, "publications", branch)

  # Reverse order (most recent first)
  publications_data <- publications_data[length(publications_data):1]

  # Create formatted entries - match actual YAML structure
  text <- sapply(
    publications_data,
    function(pub) {
      # Build links section
      links <- character(0)

      # DOI link
      if (!is.na(pub$doi) && pub$doi != "") {
        links <- c(
          links,
          paste0(
            '[<svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:',
            colour,
            ';"><path d="M96 0C78.3 0 64 14.3 64 32v416c0 17.7 14.3 32 32 32h288c17.7 0 32-14.3 32-32V128l-128-128H96zM288 128H416L288 0v128z"/></svg> Read](',
            pub$doi,
            ')'
          )
        )
      }

      # Data link
      if (!is.na(pub$data_url) && pub$data_url != "") {
        links <- c(
          links,
          paste0(
            '[<svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:',
            colour,
            ';"><path d="M448 80v48c0 44.2-100.3 80-224 80S0 172.2 0 128V80C0 35.8 100.3 0 224 0S448 35.8 448 80zM393.2 214.7c20.8-7.4 39.9-16.9 54.8-28.6V288c0 44.2-100.3 80-224 80S0 332.2 0 288V186.1c14.9 11.8 34 21.2 54.8 28.6C99.7 230.7 159.5 240 224 240s124.3-9.3 169.2-25.3zM0 346.1c14.9 11.8 34 21.2 54.8 28.6C99.7 390.7 159.5 400 224 400s124.3-9.3 169.2-25.3c20.8-7.4 39.9-16.9 54.8-28.6V432c0 44.2-100.3 80-224 80S0 476.2 0 432V346.1z"/></svg> Data](',
            pub$data_url,
            ')'
          )
        )
      }

      # Code link
      if (!is.na(pub$code_url) && pub$code_url != "") {
        links <- c(
          links,
          paste0(
            '[<svg aria-hidden="true" role="img" viewBox="0 0 640 512" style="height:1em;width:1.25em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:',
            colour,
            ';"><path d="M392.8 1.2c-17-4.9-34.7 5-39.6 22l-128 448c-4.9 17 5 34.7 22 39.6s34.7-5 39.6-22l128-448c4.9-17-5-34.7-22-39.6zm80.6 120.1c-12.5 12.5-12.5 32.8 0 45.3L562.7 256l-89.4 89.4c-12.5 12.5-12.5 32.8 0 45.3s32.8 12.5 45.3 0l112-112c12.5-12.5 12.5-32.8 0-45.3l-112-112c-12.5-12.5-32.8-12.5-45.3 0zm-306.7 0c-12.5-12.5-32.8-12.5-45.3 0l-112 112c-12.5 12.5-12.5 32.8 0 45.3l112 112c12.5 12.5 32.8 12.5 45.3 0s12.5-32.8 0-45.3L77.3 256l89.4-89.4c12.5-12.5 12.5-32.8 0-45.3z"/></svg> Code](',
            pub$code_url,
            ')'
          )
        )
      }

      # Build the aside content
      aside_content <- if (length(links) > 0) {
        paste0(
          '<div class="contact-inline">',
          paste(
            paste0('<span class="contact-item">', links, '</span>'),
            collapse = "\n"
          ),
          '</div>'
        )
      } else {
        ""
      }

      paste0(
        "### ",
        pub$title,
        "\n\n",
        gsub("Black, B\\.", "<u>Black, B.</u>", pub$authors),
        "\n\n",
        "N/A\n\n",
        pub$year,
        "\n\n",
        "*",
        pub$journal,
        "*\n\n",
        "::: aside\n",
        aside_content,
        "\n:::\n\n\n\n"
      )
    },
    USE.NAMES = FALSE
  )

  if (page_break_after) {
    c("## Publications  {data-icon=newspaper .break-after-me}", text)
  } else {
    c("## Publications  {data-icon=newspaper}", text)
  }
}
