workshop_section <- function(xlsx = "data/cv.xlsx", sheet = "workshop", page_break_after = FALSE, colour = "#333333") {
  df <- read_excel_sheet(xlsx, sheet)
  df <- df[nrow(df):1, ]  # Reverse order
  
  text <- mapply(function(title, type, city, start, end, url) {
    # Construct date string based on availability
    date_str <- if (!is.na(start) && start != "") {
      if (!is.na(end) && end != "") {
        paste0(start, " - ", end, "\n\n")
      } else {
        paste0(start, "\n\n")
      }
    } else {
      ""  # No start date, don't include anything
    }
    
    paste0(
      "### ", title, "\n\n", "*",
      type, "*\n\n",
      city, "\n\n",
      date_str,
      "::: aside\n",
      if (!is.na(url) && url != "") add_item_logo(url, type = "website", colour) else "",
      "\n:::\n\n\n\n"
    )
  }, df$title, df$type, df$city, df$start, df$end, df$url,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)
  
  heading <- "## Teaching, Training & Workshop Delivery  {data-icon=chalkboard-teacher"
  if (page_break_after) heading <- paste0(heading, " .break-after-me")
  heading <- paste0(heading, "}")
  
  c(heading, text)
}
