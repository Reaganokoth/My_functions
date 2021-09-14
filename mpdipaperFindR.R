
#widen functionality, include google scholar and other journals
#maybe im,plement abstract reading in r
library(tidyverse)
view_search_results <- function(keyword, author=NULL, year= NULL){
  #keyword = "Forest Biomass Estimation"
  #author = "Michael Thiel"
  #year <- 2016
  link <- "https://www.mdpi.com/journal/remotesensing"
  session <- rvest::html_session(link) 
  form <- rvest::html_form(session) 
  search_form <- form[[2]]
  form_filled <- rvest::set_values(search_form, q = keyword, authors = author)
  search <- rvest::submit_form(session, form_filled)
  content <- httr::content(search$response)
  document_href <- rvest::html_nodes(content, ".title-link")
  
  #find away to allow filtering by year. one option would be to use paper code and use it to filter the document_href
  list_of_matched_papers <- rvest::html_nodes(content, ".color-grey-dark") %>% 
      rvest::html_text()
  
  #removes all the html tags
  All_paper_titles <- gsub("<[^>]*>","",document_href)
  #split the keyword iunto a list
  keyword_split <- str_split(keyword, " ")[[1]]#[1:3]
  #filter the papers to remain with papers containing each word in the keyword
  filtered_list_by_keyword <- lapply(keyword_split,  function(eachKeyword){
   grep(str_to_sentence(eachKeyword), All_paper_titles, value = T)
  })
  titles <- unlist(filtered_list_by_keyword)
  return(titles)
  # #extract the information under the tag .color-grey-dark as text
  # list_of_matched_papers <- rvest::html_nodes(content, ".color-grey-dark") %>% 
  #   rvest::html_text()
  # filter_matched_papers_by_year <- paste0(list_of_matched_papers[grep(paste0(year),  list_of_matched_papers)])
  # 
  # get_bibliograpy_data <- str_extract_all(filter_matched_papers_by_year, "([0-9]{1,40})") [[1]] %>%
  #   str_split(" ")
  # rs_code <- "2072-4292"
  # volume <- get_bibliograpy_data[[2]]
  # issue <- get_bibliograpy_data[[3]]
  # paper_code <- get_bibliograpy_data[[4]]
  # #get the paper title 
  # #title_info <- rvest::html_nodes(content, ".title-link") 
  # #title <- grep(paper_code, title_info, value = T)
  # #reduce the list to year of interst
  # 
  # download.link <- paste0(str_sub(link, start = 1, end = 20), "/", rs_code,"/", volume,"/", issue,"/", paper_code,"/", "pdf")
  # 
  # download.file(url = paste0(download.link),destfile = paste0(file_path,rs_code,"_", volume,"_", issue,"_", paper_code, ".pdf"), method = "auto", mode = "wb")
  # 
}

view_search_results(keyword = "the GEOGLAM crop monitor for early warning",year = 2020)

outdir <- "/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/my_functions/test/"
 
RSpaperFindR(keyword = "the GEOGLAM crop monitor for early warning", file_path = outdir, year=2020)


get

