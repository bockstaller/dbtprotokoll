library(xml2)
library(tidyverse)


#'Parse a plenary protocol from xml format to tibbles
#'
#'Uses the xml structure of a plenary protocol to create three tibbles for further data analysis.
#'
#'@param protokoll An xml tree as returned by e.g. \code{read_xml} or \code{xml_find_all} (from the package \code{xml2}).
#'
#'@return Three tibbles in a named list:
#'
#'"rednerliste": A tibble of all speaking politicians containing speaker id, name, party and similar information.
#'
#'"absatzliste": A tibble of all paragraphs in speeches, containing speaker id, speech id and content of the paragraph.
#'
#'"kommentarliste": A tibble of all comments given during speeches and about reactions to speeches, containing speech id and comment id.
#'
#'@examples
#'parse_protokoll(read_xml("./protokolle/19007-data.xml"))
#'
#'@export
parse_protokoll <- function(protokoll){
  print("Parsing...this might take some time.")
  rednertb <- rednerliste(protokoll)
  kommentartb <- kommliste(protokoll)
  absatztb <- paraliste(protokoll)

  return(list("rednerliste"=rednertb,"absatzliste"=absatztb, "kommentarliste"=kommentartb))
}


#returns a tibble of all the names of the speakers
rednerliste <- function(protokoll){
  rednerliste <- xml_find_all(protokoll, ".//rednerliste")
  redner <- xml_find_all(rednerliste, ".//redner")
  rednertbs <- sapply(redner, name_to_tibble)

  #combine single tibbles to one big tibble
  rednertb <- rednertbs[1]
  i <- 2
  while(i <= length(rednertbs)){
    rednertb <- bind_rows(rednertb, rednertbs[[i]])
    i <- i + 1
  }

  select(rednertb, id, titel, vorname, namenszusatz, nachname, everything())
}

#return a tibble with the whole name and id for a given name of a speaker
name_to_tibble <- function(redner){
  enframe(unlist(as_list(xml_find_all(redner, ".//name")))) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(id = xml_attr(redner, "id"))
}


#returns a tibble of all comments and the speech id they were taken in
#!Not saved information: which particular paragraph a comment has followed. For our analysis, this information is irrelevant. Therefore we agreed that knowing the speech id would be sufficient.
kommliste <- function(protokoll){
  redeliste <- xml_find_all(protokoll, ".//rede")
  kommentare <- xml_find_all(redeliste, ".//kommentar") #first, find all comments in speeches

  #data cleanup: remove all "a" nodes as they only contain structural info irrelevant to us
  anodes <- xml_find_all(kommentare, ".//a")
  xml_remove(anodes)

  #create data frame of fitting shape to collect comments
  kommentardf <- data.frame(id = integer(),
                            absatz_id = character(),
                            inhalt = character()
  )

  #filling the data frame with content from comment list
  j <- 1
  for (i in kommentare) { #for every comment
    akrede <- xml_parent(i) #find current speech
    redenid <- xml_attr(akrede, "id") #retrieve id of current speech
    kommentardf <- kommentardf %>% add_row(id=j, absatz_id=redenid, inhalt=xml_text(i))
    j <- j+1
  }
  #transform data frame to tibble for easier analysis later
  kommentartb <- as_tibble(kommentardf)
  return(kommentartb)

}


#returns a tibble of all paragraphs the speech id they were taken in, who said them and what kind of speech they are
#!Not saved information: paragraphs which aren't part of speech are ignored
#!Bug: moderation from the president/vicepresidents is included and attributed incorrectly. We have to check if they are the only ones who talk without getting a speech element as introduction
paraliste <- function(protokoll){
  speechlist <- xml_find_all(protokoll, ".//rede/p | .//rede/name")

  #create data frame of fitting shape to collect comments
  absatzdf <- data.frame(id = integer(),
                         redner_id = integer(),
                         klasse = character(),
                         inhalt = character()
  )


  #filling the data frame with content from comment list
  j <- 1
  current_speaker = 0L

  for (i in speechlist) { #for every comment
    current_speech <- xml_parent(i) #find current speech
    speech_id <- xml_attr(current_speech, "id") #retrieve id of current speech

    if (xml_has_attr(i, "klasse")){
      klasse <- xml_attr(i, "klasse")

      if (klasse == "redner"){
        speaker <- xml_child(i, "redner")
        current_speaker <- as.integer(xml_attr(speaker, "id"))
      }
    }else{
      klasse = "name"
    }

    absatzdf <- absatzdf %>% add_row(id=j, redner_id=current_speaker, klasse=klasse, inhalt=xml_text(i))
    j <- j+1
  }

  #transform data frame to tibble for easier analysis later
  absatztb <- as_tibble(absatzdf)
  return(absatztb)
}

