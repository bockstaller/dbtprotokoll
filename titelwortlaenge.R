library(tidyverse)
library(xml2)
library(dbtprotokoll)

#function that outputs mean word-length in a sentence
word_length <- function(str){
  words <-  str_split(str, "[:space:]")
  len <- sapply(words, str_count, "[:alpha:]{1}")
  #print(len)
  if(is.numeric(len)) return(mean(len, na.rm = TRUE))
  return(NULL)
}


#read protocols and generate list of academic and nonacademic speakers
baseprotocol <- parse_protocols(start = "19001-data.xml", end = "19020-data.xml")
academics <- filter(baseprotocol[[1]], !is.na(titel))
nonacademics <- filter(baseprotocol[[1]], is.na(titel))

#analysis for academics
academic_speaches <- left_join(academics, mutate(baseprotocol[[2]],speaker_id=as.character(speaker_id)),
                               by = c("id" = "speaker_id"))
academic_speaches <- filter(academic_speaches, speech_type == "speech")
academic_mean_word_length <- mean(sapply(academic_speaches$content, word_length))


#analysis for nonacademics:
nonacademic_speaches <- left_join(nonacademics, mutate(baseprotocol[[2]],speaker_id=as.character(speaker_id)),
                                  by = c("id" = "speaker_id"))
nonacademic_speaches <- filter(nonacademic_speaches, speech_type == "speech")
nonacademic_mean_word_length <- mean(sapply(nonacademic_speaches$content, word_length))

cat("academics: ", academic_mean_word_length, "\n", "nonacademics: ", nonacademic_mean_word_length)
