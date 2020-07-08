library(xml2)
library(tidyverse)


#'Parse a plenary protocol from xml format to tibbles
#'
#'Uses the xml structure of a plenary protocol to create three tibbles for further data analysis.
#'
#'@param path A string containing the path to the xml file you want to parse
#'
#'@return Three tibbles in a named list:
#'
#'"rednerliste": A tibble of all speaking politicians containing speaker id, name, party and similar information.
#'
#'"absatzliste": A tibble of all paragraphs in speeches, containing speaker id, speech id and content of the paragraph.
#'
#'"kommentarliste": A tibble of all comments given during speeches and about reactions to speeches, containing speech id and comment id as well as content of the comment.
#'
#'@examples
#'parse_protokoll("./protokolle/19007-data.xml")
#'
#'@export
parse_protokoll <- function(path){
  protokoll <- read_xml(path)
  print("Parsing...this might take some time.")
  rednertb <- rednerliste(protokoll)
  kommentartb <- kommliste(protokoll)
  absatztb <- paragraph_list(protokoll)
  return(list("rednerliste"=rednertb,"absatzliste"=absatztb, "kommentarliste"=kommentartb))
}


#'Parse all plenary protocols from xml format to tibbles
#'
#'Uses the xml structure of a plenary protocols to create three tibbles for further data analysis with the help of parse_protocol().
#'
#'@param path A string containing the path to the xml files you want to parse
#'
#'@param start A string of the protocol you want to start with (optional)
#'
#'@param end A string of the protocol you want to end with (optional). Must succed start in alphabetical order
#'
#'@return Three tibbles in a named list:
#'
#'"rednerliste": A tibble of all speaking politicians containing speaker id, name, party and similar information.
#'
#'"absatzliste": A tibble of all paragraphs in speeches, containing speaker id, speech id and content of the paragraph.
#'
#'"kommentarliste": A tibble of all comments given during speeches and about reactions to speeches, containing speech id and comment id as well as content of the comment.
#'
#'@examples
#'parse_protocols(start = "19001-data.xml", end = "19003-data.xml")
#'
#'@export
parse_protocols <- function(path = "protokolle", start = NULL, end = NULL){
  protocols <- dir(path)
  protocols <- protocols[endsWith(protocols, ".xml")]

  #remove everything before start
  if(!is.null(start)){
    while(length(protocols) > 1 && protocols[1] != start){
      protocols <- protocols[2:length(protocols)]
    }
    if(protocols[1] != start){
      stop(start, " was not found in ", path, ".")
    }
  }

  #remove everything after start
  if(!is.null(end)){
    while(length(protocols) >= 1 && protocols[length(protocols)] != end){
      protocols <- protocols[1:length(protocols)-1]
    }
    if(length(protocols) == 0){
      stop(end, " was not found in ", path, ". Does it preceed ", start, "?")
    }
  }

  #merge protocols
  protocolstb <- list("rednerliste"=tidyverse::tibble(),
                      "absatzliste"=tidyverse::tibble(),
                      "kommentarliste"=tidyverse::tibble())
  for(protocol in protocols){
    currenttb <- parse_protokoll(stringr::str_c(path, "/", protocol))
    for(j in 1:3){
      protocolstb[[j]] <- tidyverse::bind_rows(protocolstb[[j]], currenttb[[j]])
    }
  }

  #tidy speakers
  protocolstb[[1]] <- tidyverse::distinct(protocolstb[[1]])

  return(protocolstb)
}
