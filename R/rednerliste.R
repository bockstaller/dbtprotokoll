library(xml2)
library(tidyverse)

#usage:
#rednerliste(read_xml("protokolle/19003-data.xml"))

#returns a tibble of all the names of the speakers
#'@export
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
