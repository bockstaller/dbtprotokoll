
#returns a tibble of all comments and the speech id they were taken in
#!Not saved information: which particular paragraph a comment has followed. For our analysis, this information is irrelevant. Therefore we agreed that knowing the speech id would be sufficient.
comment_list <- function(protocol){
  speechlist <- xml2::xml_find_all(protocol, ".//rede")
  comments <- xml2::xml_find_all(speechlist, ".//kommentar") #first, find all comments in speeches

  #data cleanup: remove all "a" nodes as they only contain structural info irrelevant to us
  anodes <- xml2::xml_find_all(comments, ".//a")
  xml2::xml_remove(anodes)

  #create data frame of fitting shape to collect comments
  commentdf <- data.frame(id = integer(),
                            paragraph_id = character(),
                            content = character(),
                            speaker_id= character()
  )

  #filling the data frame with content from comment list
  j <- 1
  for (i in comments) { #for every comment
    curspeech <- xml2::xml_parent(i) #find current speech

    speaker_id <- NA
    speaker_id <- xml2::xml_attr(xml2::xml_find_all(curspeech, ".//p/redner")[1], "id")


    speechid <- xml2::xml_attr(curspeech, "id") #retrieve id of current speech
    commentdf <- tibble::add_row(commentdf,id=j, paragraph_id=speechid, content=xml2::xml_text(i), speaker_id=speaker_id)
    j <- j+1
  }
  #transform data frame to tibble for easier analysis later
  commenttb <- tibble::as_tibble(commentdf)

  commenttb <- clean_comments(commenttb)

  return(commenttb)

}

clean_comments <- function(commenttb){
  commenttb <- dplyr::filter(commenttb, !is.na(speaker_id))
  commenttb <- dplyr::filter(commenttb, speaker_id != as.character(10000L))
  commenttb <- dplyr::filter(commenttb, speaker_id != "")
  return(dplyr::select(commenttb, -speaker_id))
}

#testing
#x <- xml2::read_xml("./protokolle/19007-data.xml")
#kommentar <- comment_list(x)
