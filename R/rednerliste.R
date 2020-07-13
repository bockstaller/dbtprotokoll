#usage:
#speakers(read_xml("protokolle/19003-data.xml"))

#returns a tibble of all the names of the speakers
speakers <- function(protokoll){
  speakerlist <- xml2::xml_find_all(protokoll, ".//rednerliste")
  speaker <- xml2::xml_find_all(speakerlist, ".//redner")
  speakertbs <- sapply(speaker, name_to_tibble)
  speakertbs <- sapply(speakertbs, unify_fractions)

  #combine single tibbles to one big tibble
  speakertb <- speakertbs[1]
  i <- 2
  while(i <= length(speakertbs)){
    speakertb <- dplyr::bind_rows(speakertb, speakertbs[[i]])
    i <- i + 1
  }



  #bring tibble in neat order, but be careful because of possibility of missing "namenszusatz"-column
  if("namenszusatz" %in% colnames(speakertb)){
    return(dplyr::select(speakertb, id, titel, vorname, namenszusatz, nachname, everything()))
  }
  else{
    return(dplyr::select(speakertb, id, titel, vorname, nachname, everything()))
  }
}

#return a tibble with the whole name and id for a given name of a speaker
name_to_tibble <- function(speaker){
  dplyr::mutate(tidyr::pivot_wider(tibble::enframe(unlist(xml2::as_list(xml2::xml_find_all(speaker, ".//name")))),
                                   names_from = name, values_from = value), id = xml2::xml_attr(speaker, "id"))
}

#clean up fractions
unify_fractions <- function(speaker_tbl){
  if("fraktion" %in% colnames(speaker_tbl)){
    if (speaker_tbl$fraktion == "	BÜNDNIS 90/"){
      speaker_tbl$fraktion <- "BÜNDNIS 90/DIE GRÜNEN"
    }
    speaker_tbl$fraktion <- stringr::str_replace_all(speaker_tbl$fraktion, " ", "")
    speaker_tbl$fraktion <- stringr::str_to_lower(speaker_tbl$fraktion)
  }
  return(speaker_tbl)
}


