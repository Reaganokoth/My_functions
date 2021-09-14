
pubFindR <- function(url, keyword){
  # at the moment works for this particular link
  #link <- "https://www.geographie.hu-berlin.de/en/professorships/eol/publications-en"
  data <- rvest::html_session(url) %>% 
    rvest::html_nodes("p") %>% 
    rvest::html_text()
  paper_name <- data[grep(paste0(keyword), data)]
  return(paper_name)
}

pubFindR(url = "https://www.geographie.hu-berlin.de/en/professorships/eol/publications-en", keyword = "remote sensing")




