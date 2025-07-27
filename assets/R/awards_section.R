awards_section <- function(xlsx = "data/cv.xlsx", sheet = "awards", page_break_after = FALSE, colour = "#333333") {
  df <- read_excel_sheet(xlsx, sheet)
  df <- df[nrow(df):1, ]  # Reverse order
  
  # Create formatted Markdown strings row by row
  text <- mapply(function(name, institute, city, date, description, url, link_type) {
    paste0(
      "### ", name, "\n\n",
      institute, "\n\n",
      city, "\n\n",
      date, "\n\n",
      if (!is.na(description) && description != "") paste0("*", description, "*\n\n") else "",
      "::: aside\n",
      if (!is.na(url) && url != "") add_item_logo(url, type = link_type, colour) else "",
      "\n:::\n\n\n\n"
    )
  }, df$name, df$institute, df$city, df$date, df$description, df$url, df$link_type,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)
  
  if (page_break_after) {
    c("## Grants & Funding {data-icon=trophy .break-after-me}", text)
  } else {
    c("## Grants & Funding  {data-icon=trophy}", text)
  }
}


