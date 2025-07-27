#' Contact Section
#' #' @description
#' This function generates a contact section for a CV in R Markdown format.
#' @param xlsx Path to the Excel file containing contact information.
#' @param sheet Name of the sheet in the Excel file to read contact data from.
#' @param colour The color of the icons (default is "#333333").
#' @return A character vector containing the formatted contact section.
contact_section <- function(
    xlsx = "data/cv.xlsx",
    sheet = "contact",
    colour = "#333333"
    ) {
  
  read_excel_sheet(xlsx, sheet)[
    j = paste0(
      "## Contact Info {#contact}\n",
      "- ", fontawesome::fa("user", fill = colour), " ", position, "\n",
      "- ", fontawesome::fa("building-columns", fill = colour), " ", institute, "\n",
      "- ", fontawesome::fa("map-location-dot", fill = colour), " ", city, "\n",
      "- ", fontawesome::fa("envelope", fill = colour), " [", gsub("\\.", "[dot]", sub("@", "[at]", email)), "](mailto:", email, ")\n",
      # "- ", fontawesome::fa("phone", fill = colour), " ", phone, "\n",
      "- ", fontawesome::fa("house", fill = colour), " [", sub("/$", "", sub("https*://", "", website)), "](", website, ")\n",
      "- ", fontawesome::fa("orcid", fill = colour), " [", orcid, "](https://orcid.org/", orcid, ")\n",
      "- ", fontawesome::fa("linkedin", fill = colour), " [", linkedin, "](https://www.linkedin.com/in/", linkedin, ")\n",
      "- ", fontawesome::fa("github", fill = colour), " [", github, "](https://github.com/", github, ")\n",
      "- ", fontawesome::fa("x-twitter", fill = colour), " [", twitter, "](https://twitter.com/", twitter, ")\n",
      # "- ", fontawesome::fa("mastodon", fill = colour), " ", mastodon, " ", paste(rev(strsplit(mastodon, "@")[[1]]), collapse = "/@"), "\n",
      # "- ", fontawesome::fa("r-project", fill = colour), " ", rgroup, "\n",
      "- ", fontawesome::fa("researchgate", fill = colour), " [", researchgate, "](", researchgate, ")\n",
      "\n"
    )
  ]
}