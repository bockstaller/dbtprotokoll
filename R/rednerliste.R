library(xml2)
library(tidyverse)

#usage:
#rednerliste(read_xml("protokolle/19003-data.xml"))

#returns a tibble of all the names of the speakers
rednerliste <- function(protokoll){
  rednerliste <- xml_find_all(protokoll, ".//rednerliste")
  redner_namen <- xml_find_all(rednerliste, ".//name")
  rednertbs <- sapply(as_list(redner_namen), name_to_tibble)

  #combine single tibbles to one big tibble
  rednertb <- rednertbs[1]
  i <- 2
  while(i <= length(rednertbs)){
    rednertb <- bind_rows(rednertb, rednertbs[[i]])
    i <- i + 1
  }

  ids <- unlist((xml_attr(xml_find_all(rednerliste, ".//redner"), "id")))
  mutate(rednertb, id = ids) %>% select(id, titel, vorname, namenszusatz, nachname, everything())
}

#return a tibble with the hole name for a given name of a speaker
name_to_tibble <- function(redner){
  enframe(unlist(redner)) %>% pivot_wider(names_from = name, values_from = value)
}
