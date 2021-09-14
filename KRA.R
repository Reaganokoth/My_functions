link <- "https://itax.kra.go.ke/KRA-Portal/"
devtools::install_github("jeroen/jsonlite")
pin <- "A006899535S"
password <- "1C0nn3ct"
session <- rvest::html_session(link)
rvest::html_form(session)
KRA_itax_login <-httr::GET(link,httr::authenticate(user = paste0(pin) , password = paste0(password) ))

data <- rawToChar(KRA_itax_login$content)
data <- jsonlite::fromJSON(data, flatten = TRUE)
data <- data$d
data <- data$results
product_ID <- data$Id
product_name <- data$Name
d = jsonlite::fromJSON(readLines(data), flatten = TRUE)
