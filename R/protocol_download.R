#'Wraps get_protocol_links and download_files to get all protocol files in a single call
#'
#'This function gets all download links and downloads the linked files into the specified folder
#'
#'@param base_url The URL of the bundestag.de-domain this should continue to be "https://www.bundestag.de
#'
#'@param registry_url This is the path to the service the "https://www.bundestag.de/protokolle"-frontend hits, to get the links to the protocol files.
#'The current default value is equivalent to the service path for the 19th period. This may change in the future.
#'It is necessary to use the devtools of the browser to get this information.
#'
#'@return No return values but the downloaded protocols are stored in the specified directory
#'
#'@examples
#'download_protocols()
#'
#'@export
download_protocols <- function(base_url = "https://www.bundestag.de", registry_url = "/ajax/filterlist/de/services/opendata/543410-543410", directory = "./protokolle"){
  links <- get_protocol_links(base_url = base_url, registry_url = registry_url)
  download_files(links, directory = directory, base_url = base_url)
}


#'Scrape the download-links for the plenary protocols from the bundestag website
#'
#'This function queries a file-registry-enpoint of the bundestag.de website repeadetly, to scrape the direct download links.
#'
#'@param base_url The URL of the bundestag.de-domain this should continue to be "https://www.bundestag.de
#'
#'@param registry_url This is the path to the service the "https://www.bundestag.de/protokolle"-frontend hits, to get the links to the protocol files.
#'The current default value is equivalent to the service path for the 19th period. This may change in the future.
#'It is necessary to use the devtools of the browser to get this information.
#'
#'@param directory Directory where the files should be stored
#'
#'@return A vector of complete links to the protocol files.
#'
#'@examples
#'get_protocol_links()
#'
#'@export
get_protocol_links <- function(base_url = "https://www.bundestag.de", registry_url = "/ajax/filterlist/de/services/opendata/543410-543410"){
  offset <- 0
  limit <- 10
  links <- vector()

  while(TRUE){
    print(offset)
    url <- stringr::paste(base_url, registry_url, "?", "limit=", limit, "&", "offset=", offset, sep="")

    suppressWarnings({
      html <- stringr::paste(readLines(url), collapse="\n")
    })
    matched <- stringr::str_match_all(html, "href=\"(.*?)\"")[[1]][, 2]

    if (length(matched)==0){
      break
    }
    links <- c(links, matched)

    offset <- offset+10
  }

  build_link <- function(base_url, link){
    stringr::str_c(base_url, link)
  }

  links <- sapply(links, build_link, base_url=base_url )

  return(links)
}


#'Download specified protocol files from the bundestag website
#'
#'Iterates over a vector of links to files and downloads them into the target directory
#'
#'@param links A vector of characters containing links to the files to download
#'
#'@param directory Directory where the files should be stored
#'
#'@return No return values but the downloaded protocols are stored in the specified directory
#'
#'@examples
#'parse_protocols(links = c("https://www.bundestag.de/resource/blob/704800/a2bb9229c2e19bbd633d30c747b41c62/19171-data.xml","https://www.bundestag.de/resource/blob/704394/b1d0626827462b96db4e2b7821b1c0cb/19170-data.xml"))
#'
#'@export
download_files <- function(links, directory = "./protokolle"){

  dir.create(file.path(directory), showWarnings = FALSE)

  for (link in links){
    print(stringr::str_c("Downloading: ", link))
    filename <- stringr::str_extract(link, regex("\\d{5}-data.xml$"))
    filepath <- stringr::paste(directory, "/" ,filename, sep="")
    download_url <- stringr::paste(base_url, link, sep="")
    download.file(download_url, filepath)
  }
}


