library("stringr")


# Set working directory to script folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

folder <- "./"

base_url = "https://www.bundestag.de"

link_register_url  <- "/ajax/filterlist/de/services/opendata/543410-543410"


offset <- 0
limit <- 10


while(TRUE){
  print(offset)
  url <- paste(base_url, link_register_url, "?", "limit=", limit, "&", "offset=", offset, sep="")
  
  suppressWarnings({
    html <- paste(readLines(url), collapse="\n")
  })
  matched <- str_match_all(html, "href=\"(.*?)\"")
  links <- matched[[1]][, 2]
 
  if (length(links)==0){
    break
  }
  

  for (link in links){
    print(link)
    filename <- str_extract(link, regex("\\d{5}-data.xml$"))
    filepath <- paste(folder, filename, sep="")
    download_url <- paste(base_url, link, sep="")
    download.file(download_url, filepath) 
  }
  

  offset <- offset+10
}

print(links)

