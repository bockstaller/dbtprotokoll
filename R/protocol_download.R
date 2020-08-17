#'Wraps get_protocol_links and download_files to get all protocol files in a single call
#'
#'This function gets all download links and downloads the linked files into the specified folder.
#'There exist no tests for it, because this function interacts directly with external systems and causing load and expenses for the operator of them.
#'Testing against them is considered bad practice. I could have mocked the server and it's responses, but this is not worth the effort given the fact that
#'this function uses unsupported interfaces anyway.
#'
#'@param base_url The URL of the bundestag.de-domain this should continue to be "https://www.bundestag.de
#'
#'@param registry_url This is the path to the service the "https://www.bundestag.de/protokolle"-frontend hits, to get the links to the protocol files.
#'The current default value is equivalent to the service path for the 19th period. This may change in the future.
#'It is necessary to use the devtools of the browser to get this information.
#'
#'@return A vector with all paths to the downloaded files
#'
#'@examples
#'download_protocols()
#'
#'@export
download_protocols <- function(base_url = "https://www.bundestag.de", registry_url = "/ajax/filterlist/de/services/opendata/543410-543410", directory = "./protokolle"){
  stopifnot("Please enter base_url as string" = is.character(base_url))
  stopifnot("Please enter registry_url as string" = is.character(registry_url))
  stopifnot("Please enter directory as string" = is.character(directory))
  links <- get_protocol_links(base_url = base_url, registry_url = registry_url)
  paths <- download_files(links, directory = directory)
  return(paths)
}


#'Scrape the download-links for the plenary protocols from the bundestag website
#'
#'This function queries a file-registry-enpoint of the bundestag.de website repeadetly, to scrape the direct download links.
#'There exist no tests for it, because this function interacts directly with external systems and causing load and expenses for the operator of them.
#'Testing against them is considered bad practice. I could have mocked the server and it's responses, but this is not worth the effort given the fact that
#'this function uses unsupported interfaces anyway.
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
  stopifnot("Please enter base_url as string" = is.character(base_url))
  stopifnot("Please enter registry_url as string" = is.character(registry_url))

  offset <- 0
  limit <- 10
  links <- vector()

  while(TRUE){
    print(offset)
    url <- paste(base_url, registry_url, "?", "limit=", limit, "&", "offset=", offset, sep="")

    suppressWarnings({
      html <- paste(readLines(url), collapse="\n")
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
#'There exist no tests for it, because this function interacts directly with external systems and causing load and expenses for the operator of them.
#'Testing against them is considered bad practice. I could have mocked the server and it's responses, but this is not worth the effort given the fact that
#'this function uses unsupported interfaces anyway.
#'
#'
#'@param links A vector of characters containing links to the files to download
#'
#'@param directory Directory where the files should be stored
#'
#'@return A vector with all paths to the downloaded files
#'
#'@examples
#'download_files(links = "https://www.bundestag.de/resource/blob/704800/a2bb9229c2e19bbd633d30c747b41c62/19171-data.xml")
#'
#'@export
download_files <- function(links, directory = "./protokolle"){
  stopifnot("Please enter links as strings" = is.character(links))
  stopifnot("Please enter links as vector" = is.vector(links))
  stopifnot("Please enter directory as string" = is.character(directory))

  dir.create(file.path(directory), showWarnings = FALSE)

  paths <- vector()

  for (link in links){
    print(stringr::str_c("Downloading: ", link))
    filename <- stringr::str_extract(link, stringr::regex("\\d{5}-data.xml$"))
    filepath <- paste(directory, "/" ,filename, sep="")
    paths <- c(paths, filepath)
    download.file(link, filepath)
  }
  return(paths)
}


