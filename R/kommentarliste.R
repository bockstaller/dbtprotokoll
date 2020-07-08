library(xml2)
library(tidyverse)


#returns a tibble of all comments and the speech id they were taken in
#!Not saved information: which particular paragraph a comment has followed. For our analysis, this information is irrelevant. Therefore we agreed that knowing the speech id would be sufficient.
comment_list <- function(protocol){
  speechlist <- xml_find_all(protocol, ".//rede")
  comments <- xml_find_all(speechlist, ".//kommentar") #first, find all comments in speeches

  #data cleanup: remove all "a" nodes as they only contain structural info irrelevant to us
  anodes <- xml_find_all(comments, ".//a")
  xml_remove(anodes)

  #create data frame of fitting shape to collect comments
  commentdf <- data.frame(id = integer(),
                            paragraph_id = character(),
                            content = character()
  )

  #filling the data frame with content from comment list
  j <- 1
  for (i in comments) { #for every comment
    curspeech <- xml_parent(i) #find current speech
    speechid <- xml_attr(curspeech, "id") #retrieve id of current speech
    commentdf <- commentdf %>% add_row(id=j, paragraph_id=speechid, content=xml_text(i))
    j <- j+1
  }
  #transform data frame to tibble for easier analysis later
  commenttb <- as_tibble(commentdf)
  return(commenttb)

}

#testing
#x <- read_xml("./protokolle/19007-data.xml")
#kommentar <- comment_list(x)
