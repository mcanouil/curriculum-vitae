#' @title Create an education section for a CV
#' @description
#' This function creates an education section for a CV from an Excel sheet.
#' #' @param xlsx Path to the Excel file containing the education data.
#' #' @param sheet Name of the sheet in the Excel file to read data from.
#' #' @param page_break_after Logical value indicating whether to insert a page break after the section.
#' #' @param include_sections A character vector indicating which sections to include in the output.
#' #' @return A character vector containing the formatted education section.
education_section <- function(
    xlsx = "data/cv.xlsx",
    sheet = "education",
    page_break_after = FALSE,
    include_sections = c("description", "thesis", "awards")
    ) {

  
  #load sheet and convert to data.frame
  text <- as.data.frame(read_excel_sheet(xlsx,sheet))
  
  #sort the rows in reverse order
  text <- text[nrow(text):1,]
  
  #loop over rows and construct strings dependent on column entries
  
  #loop over rows pasting string
  strings <- apply(text,1, function(x){
    
    # Build base string using paste0 instead of sprintf
    result_string <- paste0(
      "### ", x["degree"], "\n\n",
      x["university"], "\n\n",
      x["city"], "\n\n",
      x["start"], " - ", x["end"], "\n\n"
    )
    
    if("description" %in% include_sections){
      #check for non-empty description column
      if(!is.na(x["description"]) && x["description"] != ""){
        result_string <- paste0(result_string, x["description"], "\n\n")
      }
    }
  
  
    if("thesis" %in% include_sections){
      #check for non-empty thesis column
      if(!is.na(x["thesis"]) && x["thesis"] != ""){
        result_string <- paste0(result_string, "Thesis: *", x["thesis"], "*\n\n")
      }
    }

    if("awards" %in% include_sections){
      #check for non-empty awards column
      if(!is.na(x["awards"]) && x["awards"] != ""){
        result_string <- paste0(result_string, "Awards: *", x["awards"], "*\n\n") 
      }
    }

    #add new line at the end
    result_string <- paste0(result_string, "\n\n")
    
    #return the string
    return(result_string)
  })
  
  if (page_break_after) {
    c("## Education {data-icon=graduation-cap data-concise=true .break-after-me}", strings)
  } else {
    c("## Education {data-icon=graduation-cap data-concise=true}", strings)
  }
}