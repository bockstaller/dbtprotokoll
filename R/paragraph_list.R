#returns a tibble of all all parts of a speech with speach-id and speaker-id
paragraph_list <- function(protocol){

  #create data frame of fitting shape to collect comments
  paragraph_df <- data.frame(id = integer(),
                         speech = character(),
                         speech_type = character(),
                         speaker_id = character(),
                         moderator = character(),
                         class = character(),
                         content = character(),
                         j_1_gate = logical()
                  )

  # we are only interestet in elements contained in speeches
  speech_list <- xml2::xml_find_all(protocol, ".//rede/p | .//rede/name")

  # we assume that paragraphs in a speech section can be grouped into "speech" and "moderation"
  # a speech section always starts with a speaker
  # a moderation section always starts with a name tag
  # unknown is added for good meassure. But we still have to find an unknown situation
  speech_types <- c("unknown", "speech", "moderation")
  current_speech_type <- NA

  # we track the currently speaking speaker and a moderator who might interject.
  current_speaker = 0L
  current_moderator = NA_character_

  # we try to prove that the first paragraph a persons says is highlighted as J_!
  j_1_gate = FALSE

  for (i in 1L:length(speech_list)){
    # we handle a moderation section by appending the name of the moderator to a list
    # which gets deduplicated later and setting some modes
    # we do not reset the current speaker, this way we have a chance to know who gets moderated
    # we do not append to the paragraph list
    if (xml2::xml_name(speech_list[i]) == "name"){
      current_moderator <- xml2::xml_text(speech_list[i])
      current_speech_type <- 3
      j_1_gate <- FALSE
      next()
    }

    if (xml2::xml_name(speech_list[i]) == "p"){
      class <- xml2::xml_attr(speech_list[i], "klasse")

      if (!is.na(class)){
        # in case of a new speaker
        # we reset the moderator, save the speaker id and change some modes
        # we do not append to the paragraph list
        if ("redner" == class){
          current_moderator <- NA_character_
          current_speaker <- xml2::xml_attr(xml2::xml_child(speech_list[i], "redner"), "id")
          current_speech_type <- 2
          j_1_gate <- FALSE

          next()
        }

        # we set J_1 gate after each encounter to check if we have any paragraphs which aren't
        # led on by a J_1 tag
        if (class == "J_1"){
          j_1_gate <- TRUE
        }
      }

      paragraph_df <- tibble::add_row(paragraph_df,
                                      id = i,
                                      speaker_id = current_speaker,
                                      moderator = current_moderator,
                                      speech_type = speech_types[current_speech_type],
                                      speech = xml2::xml_attr(xml2::xml_parent(speech_list[i]), "id"),
                                      class = class,
                                      content = xml2::xml_text(speech_list[i]),
                                      j_1_gate = j_1_gate)

      next()
    }

  }

  #transform data frame to tibble for easier analysis later
  paragraph_tb <- tibble::as_tibble(paragraph_df)
  paragraph_tb <- clean_paragraphs(paragraph_tb)

  return(paragraph_tb)

}

clean_paragraphs <- function(paragraph_tb){
  paragraph_tb <- dplyr::filter(paragraph_tb, !is.na(speaker_id))
  paragraph_tb <- dplyr::filter(paragraph_tb, speaker_id != as.character(10000L))
  paragraph_tb <- dplyr::filter(paragraph_tb, speaker_id != "")
  return(paragraph_tb)
}

#testing
#x <- xml2::read_xml("./protokolle/19115-data.xml")

#paragraphen <- paragraph_list(x)

