

library(rvest)

getWikipediaTable <- function(url,n = 1){
  tables <- url %>%
    read_html() %>%
    html_nodes('.wikitable') %>%
    html_table()
  table <- tables[[n]]
  table
}

removeNotes <- function(x){
  gsub( " *\\[.*?\\] *", "", x)
}




