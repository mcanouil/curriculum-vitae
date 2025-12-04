format_package_author <- function(authors, author, max = 57) {
  # Split authors by comma and clean whitespace
  author_list <- trimws(strsplit(authors, ",")[[1]])

  # Highlight the specified author by making it bold
  author_list <- sapply(author_list, function(a) {
    if (grepl(author, a, ignore.case = TRUE)) {
      paste0("**", a, "**")
    } else {
      a
    }
  })

  # Join authors back
  result <- paste(author_list, collapse = ", ")

  # Truncate if too long
  if (nchar(result) > max) {
    result <- paste0(substr(result, 1, max - 3), "...")
  }

  return(result)
}
