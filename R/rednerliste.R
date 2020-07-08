library(xml2)
library(tidyverse)

#usage:
#speakers(read_xml("protokolle/19003-data.xml"))

#returns a tibble of all the names of the speakers
#'@export
speakers <- function(protokoll){
  speakerlist <- xml_find_all(protokoll, ".//rednerliste")
  speaker <- xml_find_all(speakerlist, ".//redner")
  speakertbs <- sapply(speaker, name_to_tibble)

  #combine single tibbles to one big tibble
  speakertb <- speakertbs[1]
  i <- 2
  while(i <= length(speakertbs)){
    speakertb <- bind_rows(speakertb, speakertbs[[i]])
    i <- i + 1
  }

  select(speakertb, id, titel, vorname, namenszusatz, nachname, everything())
}

#return a tibble with the whole name and id for a given name of a speaker
name_to_tibble <- function(speaker){
  enframe(unlist(as_list(xml_find_all(speaker, ".//name")))) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(id = xml_attr(speaker, "id"))
}
