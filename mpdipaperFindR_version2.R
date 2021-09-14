
library(tidyverse)
RSpaperFindR <- function(keyword, author=NULL, file_path, year= NULL){
  #keyword = "Forest Biomass Estimation"
  #author = "Michael Thiel"
  #year <- 2020
  link <- "https://www.mdpi.com/journal/remotesensing"
  #the code below is constant for all remote sensing related papers on mpdi
  rs_code <- "2072-4292"
  
  #initiating browsing session, extracting the html form,  and submitting the filled form with the queries
  session <- rvest::html_session(link) 
  form <- rvest::html_form(session) 
  search_form <- form[[2]]
  if(is.null(author)){
    form_filled <- rvest::set_values(search_form, q = keyword)
  } else{
    form_filled <- rvest::set_values(search_form, q = keyword, authors = author)
  }
  search <- rvest::submit_form(session, form_filled)
  #extracting the contents of the search response and retrieving information based on the titles of the papers using
  #the ".title-link" tag
  content <- httr::content(search$response)
  document_href <- rvest::html_nodes(content, ".title-link")
  
  #removes all the html tags
  #gsub("<[^>]*>","",document_href)
  #filtering the search return by the first later of your keyword
  #maybe modify this to allow more keyword filters... maybe using more words.
  keyword_split <- str_split(keyword, " ")[[1]][1:3]
  
  #relook into this section. change it or leave it is... i.e use only the single keyword or all the words in the keyword.
  filtered_list_by_keyword <- lapply(keyword_split,  function(eachKeyword){
    grep(str_to_sentence(eachKeyword), document_href, value = T)
  })
  
  #y object  contains bibliographic information (volume, issue and paper code) as a list. 
  #These information are needed to construct download link for the paper
  y <- str_extract_all(filtered_list_by_keyword, "([0-9]{1,40})")
  
  #this loop extracts relevant bibliographic information(volume, issue, code) from list or more precise dictionary object (y),
  # it returns a dictionary with only the needed information stored in biblio_info.
  print(paste("downloading", length(y),"documents...", sep = " "))
  for(i in 1:length(y)) {
    list <- vector(mode="list", length = length(y))
    list[[i]] <- y[[i]][3:5]
    if(i==1){
      biblio_info <- list[i]
      
    } else {
      biblio_info <-c(biblio_info, list[i])
    }
    
    #converts the dictionary object to (biblio_info) to a dataframe and assigns the column names
    #the length of the list corresponds to the mount of papers to be downloaded.
    df <- data.frame(matrix(unlist(biblio_info), nrow=length(biblio_info), byrow=T))
    names(df) <- c("Volume", "Issue", "code")
    add_column(.data =df, Paper= 1:nrow(df), .before = "Volume")
    for(i in 1:nrow(df)){
      volume <- df$Volume[i]
      issue <- df$Issue [i]
      code <- df$code[i]
      download.link <- paste0(str_sub(link, start = 1, end = 20), "/", rs_code,"/", volume,"/", issue,"/", code,"/", "pdf")
      options(timeout=200)
      download.file(url = paste0(download.link),destfile = paste0(file_path,rs_code,"_", volume,"_", issue,"_", code, ".pdf"), method = "auto", mode = "wb")
    } 
  }
}




outdir <- "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/my_functions/test/"

RSpaperFindR(keyword = "Strengthening agricultural decisions in countries at risk of food insecurity: the GEOGLAM crop monitor for early warning", file_path = outdir)
