library(xml2)
library(tidyverse)

#returns a tibble of all paragraphs the speech id they were taken in, who said them and what kind of speech they are
#!Not saved information: paragraphs which aren't part of speech are ignored
#!Bug: moderation from the president/vicepresidents is included and attributet incorrectly. We have to check if they are the only ones who talk without getting a speech element as introduction
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

  print(length(speechlist))

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
  return(absatzdf)
  #return(paragraphs)
}

#testing
x <- read_xml("./protokolle/19007-data.xml")
paragraphen <- paraliste(x)

paragraphen %>% filter(klasse == "name")
