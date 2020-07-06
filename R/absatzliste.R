library(xml2)
library(tidyverse)

#returns a tibble of all paragraphs the speech id they were taken in, who said them and what kind of speech they are
#!Not saved information: paragraphs which aren't part of speech are ignored
#!Bug: moderation from the president/vicepresidents is included and attributet incorrectly. We have to check if they are the only ones who talk without getting a speech element as introduction
paragraph_list <- function(protokoll){

  #create data frame of fitting shape to collect comments
  paragraph_df <- data.frame(id = integer(),
                         speech = character(),
                         speech_type = character(),
                         speaker_id = integer(),
                         moderator_id = integer(),
                         class = character(),
                         content = character(),
                         j_1_gate = logical()
                  )

  name_df <- data.frame(id = integer(),
                        name = character()
             )

  # we are only interestet in elements contained in speeches
  speechlist <- xml_find_all(protokoll, ".//rede/p | .//rede/name")

  # we assume that paragraphs in a speech section can be grouped into "speech" and "moderation"
  # a speech section always starts with a speaker
  # a moderation section always starts with a name tag
  # unknown is added for good meassure. But we still have to find an unknown situation
  speech_types <- c("unknown", "speech", "moderation")
  current_speech_type <- NA

  # we track the currently speaking speaker and a moderator who might interject.
  current_speaker = 0L
  current_moderator = NA

  # we try to prove that the first paragraph a persons says is highlighted as J_!
  j_1_gate = FALSE



  for (i in 1L:length(speechlist)){
    # we handle a moderation section by appending the name of the moderator to a list
    # which gets deduplicated later and setting some modes
    # we do not reset the current speaker, this way we have a chance to know who gets moderated
    # we do not append to the paragraph list

    if (xml_name(speechlist[i]) == "name"){
      name_df <- name_df %>% add_row(id=i, name=xml_text(speechlist[i]))
      current_moderator <- i
      current_speech_type <- 3
      j_1_gate <- FALSE
      next()
    }

    if (xml_name(speechlist[i]) == "p"){
      class <- xml_attr(speechlist[i], "klasse")

      # in case of a new speaker
      # we reset the moderator, save the speaker id and change some modes
      # we do not append to the paragraph list
      if (class == "redner"){
        current_moderator <- NA
        current_speaker <- as.integer(xml_attr(xml_child(speechlist[i], "redner"), "id"))
        current_speech_type <- 2
        j_1_gate <- FALSE

        next()
      }

      # we set J_1 gate after each encounter to check if we have any paragraphs which aren't
      # led on by a J_1 tag
      if (class == "J_1"){
        j_1_gate <- TRUE
      }


      paragraph_df <- paragraph_df %>% add_row(id = i,
                                       speaker_id = current_speaker,
                                       moderator_id = current_moderator,
                                       speech_type = speech_types[current_speech_type],
                                       speech = xml_attr(xml_parent(speechlist[i]), "id"),
                                       class = class,
                                       content = xml_text(speechlist[i]),
                                       j_1_gate = j_1_gate)

      next()
    }


  }

  #transform data frame to tibble for easier analysis later
  paragraph_tb <- as_tibble(paragraph_df)

  name_tb <- as_tibble(name_tb)

  return(paragraph_tb)

}







#testing
#x <- read_xml("./protokolle/19007-data.xml")

#paragraphen <- paragraph_list(x)

