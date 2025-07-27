experience_section <- function(xlsx = "data/cv.xlsx", sheet = "experience", page_break_after = FALSE, use_bullets = TRUE, reverse_order = TRUE) {
  
  # Load required libraries
  library(data.table)
  
  # Read text from xlsx
  text <- read_excel_sheet(xlsx, sheet)
  
  # Convert to data.table
  text <- data.table::setDT(text)
  
  if (use_bullets) {
    # OPTION TO SPLIT ACTIVITIES INTO BULLET POINT LIST
    # Put the unicode character for bullet in front of each activities entry
    # Options for different bullet sizes:
    # "\u2022" - standard bullet •
    # "\u25CF" - large black circle ●
    # "\u2B24" - large black circle ⬤
    # "\u25A0" - black square ■
    
    # Solution 1: Use non-breaking space after bullet to prevent wrapping
    bullet_char <- "\u25CF\u00A0"  # Large bullet + non-breaking space
    
    # Alternative solutions (uncomment to use):
    # bullet_char <- "\u2022"  # Go back to standard bullet
    # bullet_char <- "\u25CF "  # Large bullet with regular space
    # bullet_char <- "\u25AA "  # Small black square ▪
    
    text$activities <- sapply(text$activities, function(x) {
      open_bullet <- paste0(bullet_char, x)  # Removed extra space since bullet_char includes it
      # Replace instances of '.' with the unicode character "\n\bullet" except the last instance
      pattern_replacement <- paste0(". \n\n", bullet_char)
      open_bullet <- gsub("\\.", pattern_replacement, open_bullet)
      # Remove the last instance in the string of "\n\bullet"
      final_pattern <- paste0("\\. \n\n", gsub("(\\W)", "\\\\\\1", bullet_char))
      open_bullet <- stringi::stri_replace_last(open_bullet,
                                                replacement = ".",
                                                regex = final_pattern)
      return(open_bullet)
    })
    
    # Create the formatted text with bullet points
    if (reverse_order) {
      # Reverse order (.N:1) - most recent first
      formatted_text <- text[.N:1, paste0(
        "### ", position, "\n\n",
        institute, "\n\n",
        city, "\n\n",
        start, " - ", end, "\n\n",
        "\n\n", activities, "\n\n\n\n"
      )]
    } else {
      # Original order (1:.N) - chronological order
      formatted_text <- text[1:.N, paste0(
        "### ", position, "\n\n",
        institute, "\n\n",
        city, "\n\n",
        start, " - ", end, "\n\n",
        "\n\n", activities, "\n\n\n\n"
      )]
    }
    
  } else {
    # OPTION WITHOUT BULLET POINTS - activities in italics
    # Create the formatted text without bullet points
    if (reverse_order) {
      # Reverse order (.N:1) - most recent first
      formatted_text <- text[.N:1, paste0(
        "### ", position, "\n\n",
        institute, "\n\n",
        city, "\n\n",
        start, " - ", end, "\n\n",
        "\n\n*", activities, "*\n\n\n\n"
      )]
    } else {    
      # Original order (1:.N) - chronological order
      formatted_text <- text[1:.N, paste0(
        "### ", position, "\n\n",
        institute, "\n\n",
        city, "\n\n",
        start, " - ", end, "\n\n",
        "\n\n*", activities, "*\n\n\n\n"
      )]
    }
  }
  
  # Return the result
  if (page_break_after) {
    c("## Professional Experience {data-icon=laptop .break-after-me}", formatted_text)
  } else {
    c("## Professional Experience {data-icon=laptop}", formatted_text)
  }
}
