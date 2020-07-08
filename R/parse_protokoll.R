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
#'"speakers": A tibble of all speaking politicians containing speaker id, name, party and similar information.
#'
#'"paragraphs": A tibble of all paragraphs in speeches, containing speaker id, speech id and content of the paragraph.
#'
#'"comments": A tibble of all comments given during speeches and about reactions to speeches, containing speech id and comment id as well as content of the comment.
#'
#'@examples
#'parse_protocol("./protokolle/19007-data.xml")
#'
#'@export
parse_protocol <- function(path){
  protocol <- read_xml(path)
  speakertb <- speakerlist(protocol)
  commenttb <- comment_list(protocol)
  paragraphtb <- paragraph_list(protocol)
  return(list("speakers"=speakertb,"paragraphs"=paragraphtb, "comments"=commenttb))
}


