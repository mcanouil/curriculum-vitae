articles_section <- function(bib = "data/cv.bib", author = NULL, page_break_after = FALSE, only_first = FALSE) {
  author <- gsub(" ", "&nbsp;", author)
  text <- glue::glue_data(.x = read_bib(bib), .sep = "\n\n",
    "### {title}",
    "{format_bib_author(authors, first, author)}",
    "N/A",
    "{month} {year}",
    "::: aside",
    '*[{journal}]({doi})*\n{ifelse(first, \'<p style="font-size: 75%;"><sup>&dagger;</sup> As first or co-first author.</p>\', \'\')}\n:::',
  )

  articles_count <- length(text)

  if (only_first) {
    text <- text[grepl("As first or co-first author", text)]
    articles_count <- sprintf("%s + %s", length(text), articles_count - length(text))
  }

  if (page_break_after) {
    c(
      sprintf("## Publications (%s) {{data-icon=newspaper .break-after-me}}", articles_count),
      text
    )
  } else {
    c(
      sprintf("## Publications (%s) {{data-icon=newspaper}}", articles_count),
      text
    )
  }
}

clean_field <- function(pattern, x) {
  gsub(
    pattern = sprintf("^%s = ", pattern),
    replacement = "",
    x = gsub(
      pattern = ",$",
      replacement = "",
      x = gsub(
        pattern = "[{}]",
        replacement = "",
        x = grep(sprintf("^%s", pattern), x, value = TRUE)
      )
    )
  )
}

read_article <- function(.x) {
  authors <- do.call("rbind", strsplit(unlist(strsplit(clean_field("author", .x), " and ")), ", "))
  authors <- apply(X = authors[, c(2, 1)], MARGIN = 1, FUN = function(irow) {
    gsub(" ", "&nbsp;", paste(unique(irow), collapse = " "))
  })
  authors <- paste(paste(authors[-length(authors)], collapse = ", "), authors[length(authors)], sep = " and ")
  data.frame(
    title = clean_field("title", .x),
    month = gsub("May.", "May", paste0(capitalise(clean_field("month", .x)), ".")),
    year = clean_field("year", .x),
    doi = clean_field("doi", .x),
    authors = authors,
    journal = clean_field("journal", .x),
    first = if (any(grepl("annote", .x))) {
      grepl("first", clean_field("annote", .x))
    } else {
      FALSE
    },
    stringsAsFactors = FALSE
  )
}

read_bib <- function(path) {
  big_file <- paste(readLines(path), collapse = "")
  big_file <- unlist(strsplit(x = big_file, split = "@", fixed = TRUE))
  big_file <- big_file[nchar(big_file) != 0]

  all_bib <- lapply(strsplit(x = big_file, split = "(,\t)|(,  )"), read_article)
  all_bib <- do.call("rbind.data.frame", all_bib)
  all_bib[["month"]] <- factor(
    x = all_bib[["month"]],
    levels = gsub("May.", "May", paste0(c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    ), "."))
  )
  all_bib[["doi"]] <- ifelse(
    test = grepl("^http", all_bib[["doi"]]),
    yes = all_bib[["doi"]],
    no = paste0("https://www.doi.org/", all_bib[["doi"]])
  )

  all_bib[order(all_bib[["year"]], all_bib[["month"]], decreasing = TRUE), ]
}

format_bib_author <- function(authors, first, author, max = 10) {
  mapply(
    iauthors = authors,
    ifirst = first,
    FUN = function(iauthors, ifirst) {
      split_authors <- unlist(strsplit(strsplit(iauthors, ", ")[[1]], " and "))
      split_authors <- gsub(
        pattern = author,
        replacement = paste0("<u>", author, "</u>", if (ifirst) "<sup>&dagger;</sup>" else ""),
        x = split_authors
      )
      pos_author <- grep(author, split_authors)
      if (length(split_authors) <= max) {
        paste(
          paste(split_authors[-length(split_authors)], collapse = ", "),
          split_authors[length(split_authors)],
          sep = " and "
        )
      } else {
        switch(
          EXPR = paste(
            abs(c(0, length(split_authors)) - pos_author) > ceiling(max / 2),
            collapse = "--"
          ),
          "TRUE--TRUE" = {
            if (pos_author > ceiling((max - 1) / 2)) {
              split_authors[pos_author] <- paste0(
                split_authors[pos_author],
                "<sup>", pos_author, "/", length(split_authors), "</sup>"
              )
            }
            paste0(
              paste(
                c(
                  split_authors[1:ceiling((max - 1) / 2)],
                  "*[...]*",
                  split_authors[pos_author],
                  "*[...]*",
                  split_authors[
                    (length(split_authors) - (max - 1 - ceiling((max - 1) / 2))):(length(split_authors) - 1)
                  ]
                ),
                collapse = ", "
              ),
              " and ",
              split_authors[length(split_authors)]
            )
          },
          "TRUE--FALSE" = {
            if (pos_author > ceiling(max / 2)) {
              split_authors[pos_author] <- paste0(
                split_authors[pos_author],
                "<sup>", pos_author, "/", length(split_authors), "</sup>"
              )
            }
            paste0(
              paste(
                c(
                  split_authors[1:ceiling(max / 2)],
                  "*[...]*",
                  split_authors[
                    (length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)
                  ]
                ),
                collapse = ", "
              ),
              " and ",
              split_authors[length(split_authors)]
            )
          },
          "FALSE--TRUE" = {
            if (pos_author > ceiling(max / 2)) {
              split_authors[pos_author] <- paste0(
                split_authors[pos_author],
                "<sup>", pos_author, "/", length(split_authors), "</sup>"
              )
            }
            paste0(
              paste(
                c(
                  split_authors[1:ceiling(max / 2)],
                  "*[...]*",
                  split_authors[
                    (length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)
                  ]
                ),
                collapse = ", "
              ),
              " and ",
              split_authors[length(split_authors)]
            )
          },
          "FALSE--FALSE" = {
            paste(
              paste(split_authors[-length(split_authors)], collapse = ", "),
              split_authors[length(split_authors)],
              sep = " and "
            )
          }
        )
      }
    }
  )
}
