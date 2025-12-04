format_package_date <- function(date) {
  # Handle different date formats
  if (is.null(date) || is.na(date)) {
    return("")
  }

  # If it's already a character string, return as is
  if (is.character(date)) {
    return(date)
  }

  # If it's a Date object, format it
  if (inherits(date, "Date")) {
    return(format(date, "%Y-%m-%d"))
  }

  # Try to parse common date formats
  tryCatch(
    {
      # Try to parse as date
      parsed_date <- as.Date(date)
      return(format(parsed_date, "%Y-%m-%d"))
    },
    error = function(e) {
      # If parsing fails, return as character
      return(as.character(date))
    }
  )
}
