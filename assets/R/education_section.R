education_section <- function(xlsx = "data/cv.xlsx", sheet = "education", page_break_after = FALSE) {
  # text <- read_excel_sheet(xlsx, sheet)[
  #   i = .N:1,
  #   j = sprintf(
  #     "### %s\n\n%s\n\n%s\n\n%s - %s\n\n*%s*\n\n\n\n",
  #     degree, university, city, start, end, description
  #   )
  # ]
  
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
    
    #check for non-empty description column
    if(!is.na(x["thesis"]) && x["thesis"] != ""){
      result_string <- paste0(result_string, "Thesis: *", x["thesis"], "*\n\n")
    }
    
    #check for non-empty awards column
    if(!is.na(x["awards"]) && x["awards"] != ""){
      result_string <- paste0(result_string, "Awards: *", x["awards"], "*\n\n") 
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