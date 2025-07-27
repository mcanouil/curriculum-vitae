#devtools::install_github("ropensci/bib2df")
library(bib2df)

#' Create a section for articles in a CV
#' @description
#' This function creates a section for articles in a CV from a BibTeX file.
#' @param bib Path to the BibTeX file containing the articles.
#' @param author The author to highlight in the articles section.
#' @param page_break_after Logical value indicating whether to insert a page break after the section.
#' @return A character vector containing the formatted articles section.
articles_section <- function(
    bib = "data/cv.bib",
    author = "Black",
    page_break_after = FALSE
    ) {
  
  #read the bib tex file
  articles <- bib2df(bib)
  
  # seperate any rows for which year == "In Review"
  in_review <- articles[articles$YEAR == "In Review",]
  
  # remove the rows for which year == "In Review"
  articles <- articles[articles$YEAR != "In Review",]
  
  # convert YEAR column to numeric
  articles$YEAR <- as.numeric(articles$YEAR)
  
  #add a date column by pasting together year and month and adding arbitrary day
  # articles$DATE <- apply(articles,1, function(x){as.Date(paste0(x$YEAR, "-", x$MONTH, "-01"), "%Y-%b-%d")})
  
  #sort the rows according to descending YEAR
  articles <- articles[rev(order(articles$YEAR)),]
  
  # add the in review articles at the head of the df
  if(nrow(in_review) > 0){
    articles <- rbind(in_review, articles)
  }
  
  #loop over entries formatting as strings
  strings <- c(apply(articles,1, function(article){
    
    #vector required fields
    required_fields <- c("TITLE", "AUTHOR","YEAR","JOURNAL")
    
    #check which of the required fields are columns in the article
    reqs <- sapply(required_fields, function(field){field %in% names(article)})
    
    #subset to those present
    required_fields <- required_fields[reqs]
    
    #check for non-na entries
    Non_NA_reqs <- sapply(required_fields, function(field){ if(any(!is.na(article[[field]]))){TRUE}else{FALSE}})
    
    #subset to only non-NA entries
    required_fields <- required_fields[Non_NA_reqs]
    
    #extract the entries from article
    fields <- as.list(article[required_fields])
    
    #convert the authors to a single string
    fields$AUTHOR <- paste(fields$AUTHOR, collapse = ", ")
    
    #within the string replace 'Black, B.' with "<u>Black, B.</u>"
    fields$AUTHOR <- gsub("Black, B.", "<u>Black, B.</u>", fields$AUTHOR)
    
    #convert all entries to character
    fields <- lapply(fields, as.character)
    
    #check which of the custom fields DATA, CODE, PREPRINT or RESULTS are non-empty
    custom_fields <- c("DOI", "DATA", "CODE", "PREPRINT", "RESULTS")
    
    #check which of the custom fields are columns in the article
    customs <- sapply(custom_fields, function(field){field %in% names(article)})
    
    #subset to only those that are columns
    custom_fields <- custom_fields[customs]
    
    #subset to non-NA entries
    Non_NA_customs <- sapply(custom_fields, function(field){ if(any(!is.na(article[[field]]))){TRUE}else{FALSE}})
    
    #subset to only non-NA entries
    custom_fields <- custom_fields[Non_NA_customs]
    
    #get the entries from the article
    custom_entries <- as.list(article[custom_fields])
    
    #modify the data entry to include an icon and string and link both to the url 
    if("DATA" %in% names(custom_entries)){
      custom_entries$DATA <- paste0(" [ ", fontawesome::fa("database", fill = "#333333"), " Data]", "(", custom_entries$DATA, ")")
    }
    
    if("DOI" %in% names(custom_entries)){
      #add an icon to the DOI
      custom_entries$DOI <- paste0("[", fontawesome::fa("scroll", fill = "#333333"), " Read](", custom_entries$DOI, ")")
    }
    
    #modify the CODE entry
    if("CODE" %in% names(custom_entries)){
      custom_entries$CODE <- paste0(" [ ", fontawesome::fa("code", fill = "#333333"), " Code]", "(", custom_entries$CODE, ")")
    }
    
    # if length of custom fields is 0 then create the basic string
    if(length(custom_fields) == 0){
      
      # Build string using paste0 instead of sprintf
      result_string <- paste0(
        "### ", fields$TITLE, "\n\n",
        fields$AUTHOR, "\n\nN/A\n\n",
        fields$YEAR, "\n\n *", fields$JOURNAL, "*\n\n",
        "::: aside\n\n\n:::"
      )
    } else if(length(custom_fields) != 0){
      
      # Don't add \n between entries
      custom_entries <- lapply(custom_entries, function(x) {
        paste0('<span class="contact-item">', x, '</span>')
      })
      
      # Wrap all in a container div
      custom_block <- paste0(
        '<div class="contact-inline">',
        paste(custom_entries, collapse = "\n"),
        '</div>'
      )
      
      # Build the full result string
      result_string <- paste0(
        "### ", fields$TITLE, "\n\n",
        fields$AUTHOR, "\n\nN/A\n\n",
        fields$YEAR, "\n\n *", fields$JOURNAL, "*\n\n",
        "::: aside\n",
        custom_block,  # << your contact icons wrapped here
        "\n:::\n"
      )
      
    }
    
    return(result_string)
    
    
    
  }, simplify = TRUE))
  
  #count number of articles
  articles_count <- length(strings)
  
  #prepare section header
  if (page_break_after) {
    return(c("## Publications  {data-icon=newspaper .break-after-me}", strings))
  } else {
    return(c("## Publications  {data-icon=newspaper}", strings))
  }
  
}


