packages_section <- function(
  xlsx = "data/cv.xlsx",
  sheet = "packages",
  author = NULL,
  page_break_after = FALSE,
  colour = "#333333"
) {
  text <- read_excel_sheet(xlsx, sheet)[
    i = .N:1,
    j = sprintf(
      "### %s: %s\n\n(As *%s*)\n\n%s\n\n%s\n\n%s\n\n\n\n",
      name, title, tolower(type),
      lapply(
        X = where,
        FUN = function(.x) {
          switch(EXPR = .x, "GitHub" = "GitHub", "CRAN" = "CRAN", "BOTH" = "CRAN")
        }
      ),
      format_package_date(since),
      format_package_url(user, name, where, colour)
    )
  ]

  if (page_break_after) {
    c(
      sprintf("## R Packages (%s) {data-icon=code .break-after-me}", length(text)),
      text
    )
  } else {
    c(
      sprintf("## R Packages (%s) {data-icon=code}", length(text)),
      text
    )
  }
}

format_package_author <- function(authors, author, max = 57) {
  lapply(X = authors, FUN = function(iauthors) {
    split_authors <- unlist(strsplit(strsplit(iauthors, ", ")[[1]], " and "))
    split_authors <- gsub(
      pattern = author,
      replacement = paste0("<u>", author, "</u>"),
      x = split_authors
    )
    split_authors <- gsub(" ", "&nbsp;", split_authors)
    list_authors <- paste(
      paste(split_authors[-length(split_authors)], collapse = ", "),
      split_authors[length(split_authors)],
      sep = " and "
    )
    max <- max + length(gregexpr("&nbsp;", list_authors)[[1]]) * 5
    if (nchar(list_authors) > max) {
      regmatches(
        x = list_authors,
        m = structure(
          gregexpr(" ", list_authors)[[1]][max(which(gregexpr(" ", list_authors)[[1]] < max))],
          match.length = 1L
        )
      ) <- "  \n"
    }

    list_authors
  })
}

format_package_date <- function(date) {
  lapply(X = date, FUN = function(idate) {
    gsub(
      pattern = "May.",
      replacement = "May",
      x = format(
        as.Date(paste0(idate, "-01"), format = "%Y-%m-%d"),
        format = "%b. %Y"
      )
    )
  })
}

format_package_url <- function(repo_user, repo_name, where, colour) {
  mapply(
    repo_user = repo_user,
    repo_name = repo_name,
    where = where,
    FUN = function(repo_user, repo_name, where) {
      mc <- sub("#", "", colour)
      switch(
        EXPR = where,
        "GitHub" = {
          paste0(
            "[https://github.com/", repo_user,
            "/", repo_name, "/](https://github.com/",
            repo_user, "/", repo_name, "/)",
            "\n\n",
            "::: aside",
            "\n",
            "[![GitHub_tag](https://img.shields.io/github/tag/",
            repo_user, "/", repo_name,
            ".svg?label=Github&color=", mc,
            ")](https://github.com/", repo_user, "/", repo_name, "/)",
            "\n",
            ":::",
            "\n"
          )
        },
        "CRAN" = {
          paste0(
            "[https://cran.r-project.org/package=", repo_name,
            "](https://cran.r-project.org/package=", repo_name, ")",
            "\n\n",
            "::: aside",
            "\n",
            "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/",
            repo_name, "?color=", mc, ")](https://cran.r-project.org/package=",
            repo_name, ")",
            "\n",
            ":::",
            "\n"
          )
        },
        "BOTH" = {
          paste0(
            "[https://cran.r-project.org/package=", repo_name,
            "](https://cran.r-project.org/package=", repo_name, ")  ",
            "\n",
            "[https://github.com/", repo_user, "/",
            repo_name, "/](https://github.com/",
            repo_user, "/", repo_name, "/)",
            "\n\n",
            "::: aside",
            "\n",
            "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/",
            repo_name, "?color=", mc, ")](https://cran.r-project.org/package=",
            repo_name, ")  ",
            "\n",
            "[![GitHub_tag](https://img.shields.io/github/tag/",
            repo_user, "/", repo_name, ".svg?label=Github&color=",
            mc, ")](https://github.com/", repo_user, "/", repo_name, "/)",
            "\n",
            ":::",
            "\n"
          )
        }
      )
    }
  )
}
