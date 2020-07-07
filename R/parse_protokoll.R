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


