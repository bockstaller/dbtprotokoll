library(xml2)
library(tidyverse)

#returns a tibble of all comments and the speech id they were taken in
kommliste <- function(protokoll){
  redeliste <- xml_find_all(protokoll, ".//rede")
  kommentare <- xml_find_all(redeliste, ".//kommentar") #first, find all comments in speeches

  #data cleanup: remove all "a" nodes as they only contain structural info irrelevant to us
  anodes <- xml_find_all(kommentare, ".//a")
  xml_remove(anodes)

  #create data frame of fitting shape to collect comments
  kommentardf <- data.frame(id = integer(),
                            absatz_id = character(),
                            inhalt = character()
  )

  #filling the data frame with content from comment list
  j <- 1
  for (i in kommentare) { #for every comment
    akrede <- xml_parent(i) #find current speech
    redenid <- xml_attr(akrede, "id") #retrieve id of current speech
    kommentardf <- kommentardf %>% add_row(id=j, absatz_id=redenid, inhalt=xml_text(i))
    j <- j+1
  }
  #transform data frame to tibble for easier analysis later
  kommentartb <- as.tibble(kommentardf)
  return(kommentartb)

}

#testing
#x <- read_xml("./protokolle/19007-data.xml")
#kommentar <- kommliste(x)
