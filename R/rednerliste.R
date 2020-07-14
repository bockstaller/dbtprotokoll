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
    speaker_tbl$fraktion <- gsub("\\s", "", speaker_tbl$fraktion)
    speaker_tbl$fraktion <- stringr::str_to_lower(speaker_tbl$fraktion)
    if (speaker_tbl$fraktion == "bündnis90/"){
      speaker_tbl$fraktion <- "bündnis90/diegrünen"
    }

  }
  return(speaker_tbl)
}



#function to tidy up the list of speakers
clean_speakers <- function(speakers){
  #tidy multiple names for one id
  speakers[speakers$id == "10000", 1] <- as.character(10000+ (1:nrow(speakers[speakers$id == "10000",])))

  #merge rows of same person into one
  speakers <- group_by(speakers, id) %>% mutate("anzahl" = n()) %>% arrange(desc(anzahl), id)
  for(rownum in 1:nrow(speakers)){
    if(speakers$anzahl[rownum] > 1){
      for(i in 1:(speakers$anzahl[rownum] - 1)){
        speakers[rownum,1:(length(speakers)-1)] <- full_join(speakers[rownum,], speakers[rownum + i,], by = "id") %>%
          transmute(id,
                    "titel" = coalesce(.$titel.x, .$titel.y),
                    vorname = coalesce(.$vorname.x, .$vorname.y),
                    nachname = coalesce(.$nachname.x, .$nachname.y),
                    rolle.rolle_lang = coalesce(.$rolle.rolle_lang.x, .$rolle.rolle_lang.y),
                    rolle.rolle_kurz = coalesce(.$rolle.rolle_kurz.x, .$rolle.rolle_kurz.y),
                    ortszusatz = coalesce(.$ortszusatz.x, .$ortszusatz.y),
                    fraktion = coalesce(.$fraktion.x, .$fraktion.y),
                    namenszusatz = coalesce(.$namenszusatz.x, .$namenszusatz.y))
      }
      #remove next entries of group
      speakers <- speakers[-(rownum + (1:(speakers$anzahl[rownum]-1))),]
    }
    if(rownum == nrow(speakers)){
      break
    }
  }

  #ToDo: Tidy up entries of new entries of titel, namenszusatz and rolle

  return(speakers[1:(length(speakers)-1)])
}
#redner <- full_protocol[[1]]
#redner <- clean_speakers(redner)
#redner %>% arrange(desc(anzahl),id) -> redner
