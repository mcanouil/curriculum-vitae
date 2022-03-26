add_github_logo <- function(url) {
  sub(
    "[GitHub]",
    paste0("[", fa("github", fill = main_colour), " GitHub]"),
    sub(
      pattern = "(.*)https://github.com/(.*)",
      replacement = "\\1[GitHub](https://github.com/\\2)",
      x = url
    ),
    fixed = TRUE
  )
}
