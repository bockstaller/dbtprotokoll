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
    return(dplyr::select(speakertb, id, any_of("titel"), vorname, namenszusatz, nachname, everything()))
  }
  else{
    return(dplyr::select(speakertb, id, any_of("titel"), vorname, nachname, everything()))
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

#gives back tibbles for speakers without roles and for roles with id
split_roles <- function(speakers){
  if("rolle.rolle_lang" %in% colnames(speakers)){
    if(!("rolle.rolle_kurz" %in% colnames(speakers))){
      speakers <- dplyr::mutate(speakers, "rolle.rolle_kurz" = NA_character_)
    }
    roles <- dplyr::select(speakers, id, rolle.rolle_lang, rolle.rolle_kurz)
    roles <- tidyr::drop_na(roles)

    speakers <- dplyr::mutate(speakers, rolle = !is.na(rolle.rolle_lang), rolle.rolle_lang = NULL, rolle.rolle_kurz = NULL)
  }
  else{
    roles <- tibble::tibble("id" = character(), "rolle.rolle_lang" = character(), "rolle.rolle_kurz" = character())
    speakers <- dplyr::mutate(speakers, rolle = FALSE)
  }
  return(list(speakers, roles))
}


#function to tidy up the list of speakers
clean_speakers <- function(speakers){
  #remove 10000-ids
  wrongids <- which(speakers$id == "10000")
  if(!identical(wrongids, integer(0))){
    speakers <- speakers[-which(speakers$id == "10000"),]
  }

  #merge rows of same person into one
  speakers <- dplyr::arrange(dplyr::mutate(dplyr::group_by(speakers, id), "anzahl" = dplyr::n()), dplyr::desc(anzahl), id)
  for(rownum in 1:nrow(speakers)){
    if(speakers$anzahl[rownum] > 1){
      for(i in 1:(speakers$anzahl[rownum] - 1)){
        speakers[rownum,c(1:6,8)] <- dplyr::transmute(dplyr::full_join(speakers[rownum,], speakers[rownum + i,], by = "id"),
                    id,
                    titel = dplyr::coalesce(titel.x, titel.y),
                    vorname = dplyr::coalesce(vorname.x, vorname.y),
                    nachname = dplyr::coalesce(nachname.x, nachname.y),
                    ortszusatz = dplyr::coalesce(ortszusatz.x, ortszusatz.y),
                    fraktion = dplyr::coalesce(fraktion.x, fraktion.y),
                    namenszusatz = dplyr::coalesce(namenszusatz.x, namenszusatz.y))
        speakers[rownum,]$rolle <- (speakers[rownum,]$rolle | speakers[rownum + i,]$rolle)
      }
      #remove next entries of group
      speakers <- speakers[-(rownum + (1:(speakers$anzahl[rownum]-1))),]
    }
    if(rownum == nrow(speakers)){
      break
    }
  }

  #ToDo: Tidy up entries of new entries of titel, namenszusatz and rolle

  return(dplyr::ungroup(speakers[1:(length(speakers)-1)]))
}

clean_roles <- function(roles){
  #remove 10000-ids
  rubishids <- which(roles$id == "10000")
  if(!identical(rubishids, integer(0))){
    roles <- roles[-rubishids]
  }

  roles <- dplyr::mutate(roles, "rolle.rolle_lang" = stringr::str_squish(roles$rolle.rolle_lang), "rolle.rolle_kurz" = stringr::str_squish(roles$rolle.rolle_kurz))
  roles <- dplyr::distinct(roles)
  return(roles)
}
