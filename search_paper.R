searchpapeR<- function(keyword, author=NULL, full_list = FALSE){
  #keyword = "forest fire"
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
  if (full_list==FALSE & document_href !=0) {
    #removes all the html tags and return the titles
    titles <- gsub("<[^>]*>","",document_href)
    return(titles)
  } else {
    if(full_list !=FALSE & document_href!=0) {
      #filtering the search return by the first later of your keyword
      #maybe modify this to allow more keyword filters... maybe using more words.
      keyword_split <- str_split(keyword, " ")[[1]][1]
      filtered_list_by_keyword <- grep(str_to_sentence(keyword_split), titles, value = T)
      return(filtered_list_by_keyword)
    } else {
      link <- "https://scholar.google.com/"
      #keyword <- "Mapping dune changes using InSAR sentinel 1"
      session <- rvest::html_session(link) 
      form <- rvest::html_form(session) 
      search_form <- form[[2]]
      form_filled <- rvest::set_values(search_form, q = keyword)
      search <- rvest::submit_form(session, form_filled)
      content <- httr::content(search$response)
      document_href <- rvest::html_nodes(content, "a") %>% 
        rvest::html_attr("href")
      #titles <- gsub("<[^>]*>","",document_href)
      x <- grep("mdpi", document_href, value = T)
      return(x)
    }
    
  }
}

searchpapeR("Mapping dune changes using InSAR sentinel 1")
