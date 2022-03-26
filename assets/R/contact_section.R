contact_section <- function(xlsx = "data/cv.xlsx", sheet = "contact", colour = "#333333") {
  read_excel_sheet(xlsx, sheet)[
    j = sprintf(
      fmt = paste(
        "## Contact Info {{#contact}}\n",
        "- %s %s",
        "- %s %s",
        "- %s %s",
        '- %s [{gsub("@", " [at] ", email)}](mailto:%s)',
        "- %s %s",
        '- %s [{sub("/$", "", sub("https*://", "", website))}](%s)',
        "- %s [%s](https://orcid.org/%s)",
        "- %s [%s](https://www.linkedin.com/in/%s)",
        "- %s [%s](https://github.com/%s)",
        "- %s [%s](https://twitter.com/%s)",
        "- %s %s",
        "\n",
        sep = "\n"
      ),
      fontawesome::fa("user", fill = colour), position,
      fontawesome::fa("university", fill = colour), institute,
      fontawesome::fa("map-marker", fill = colour), city,
      fontawesome::fa("envelope", fill = colour), email,
      fontawesome::fa("phone", fill = colour), phone,
      fontawesome::fa("home", fill = colour), website,
      fontawesome::fa("orcid", fill = colour), orcid, orcid,
      fontawesome::fa("linkedin", fill = colour), linkedin, linkedin,
      fontawesome::fa("github", fill = colour), github, github,
      fontawesome::fa("twitter", fill = colour), twitter, twitter,
      fontawesome::fa("r-project", fill = colour), rgroup
    )
  ]
}
