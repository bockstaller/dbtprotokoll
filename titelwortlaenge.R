library(tidyverse)
library(xml2)
library(dbtprotokoll)
library(ggplot2)

#function that outputs mean word-length in a sentence
word_length <- function(str){
  words <-  str_split(str, "[:space:]")
  len <- sapply(words, str_count, "[:alpha:]{1}")
  return(len)
}

percentage <- function(tbl){
  group_by(tbl, lgt) %>%
    mutate("count" = n()) %>%
    distinct() %>%
    arrange(lgt) %>%
    ungroup() %>%
    mutate(pct = count / sum(count))
}

#read protocols and generate list of academic and nonacademic speakers
baseprotocol <- parse_protocols(start = "19001-data.xml", end = "19020-data.xml")
academics <- filter(baseprotocol[[1]], !is.na(titel))
nonacademics <- filter(baseprotocol[[1]], is.na(titel))
speaches <- filter(baseprotocol[[2]], is.na(moderator))

#analysis for academics
academic_speaches <- left_join(academics, mutate(speaches,speaker_id=speaker_id),
                               by = c("id" = "speaker_id"))
academic_speaches <- filter(academic_speaches, speech_type == "speech")
academic_word_lengths <- tibble("lgt" = unname(unlist(sapply(academic_speaches$content, word_length)))) %>%
  filter(lgt > 0)
academic_mean_word_length <- summarise(academic_word_lengths, "mean" = mean(lgt))


#analysis for nonacademics:
nonacademic_speaches <- left_join(nonacademics, mutate(speaches,speaker_id=speaker_id),
                                  by = c("id" = "speaker_id"))
nonacademic_speaches <- filter(nonacademic_speaches, speech_type == "speech")

nonacademic_word_lengths <- tibble("lgt" = unname(unlist(sapply(nonacademic_speaches$content, word_length)))) %>%
  filter(lgt > 0)
nonacademic_mean_word_length <- summarise(nonacademic_word_lengths, "mean" = mean(lgt))

#summary
titel_mean_word_length <- tibble("titel" = c("academics", "nonacademics"), "count" = c(academic_mean_word_length$mean, nonacademic_mean_word_length$mean))

titel_word_lengths <- bind_rows(mutate(percentage(academic_word_lengths), "titel" = "academics"),
                                mutate(percentage(nonacademic_word_lengths), "titel" = "nonacademics"))


#plots
ggplot(titel_mean_word_length) + geom_col(aes(x = titel, y = count))
ggplot(titel_word_lengths) + geom_col(aes(x = lgt, y = pct, fill = titel), position = "dodge")



#for different parties
party_word_length <- function(speaches, speakers, party){
  party_members <- filter(speakers, fraktion == party)
  party_speaches <- left_join(party_members, speaches,
                                 by = c("id" = "speaker_id"))
  party_speaches <- filter(party_speaches, speech_type == "speech")
  word_lengths <- tibble("lgt" = unname(unlist(sapply(party_speaches$content, word_length)))) %>%
    filter(lgt > 0)
  return(word_lengths)
}

parties <- c('cdu/csu', 'spd', 'fdp', 'bündnis90/diegrünen', 'afd', 'dielinke')

party_word_lengths <- lapply(parties, party_word_length, speakers = baseprotocol[[1]], speaches = speaches)
party_mean_word_length <- lapply(party_word_lengths, summarise, "mean" = mean(lgt))

party_mean_word_length <- tibble("party" = parties,
                                 "count" = sapply(party_mean_word_length, function(tbl){return(tbl$mean)}))
party_word_lengths_pct <- bind_rows(lapply(1:6, function(i){
  return(mutate(percentage(party_word_lengths[[i]]), "party" = parties[i]))}))

#plots for parties
ggplot(party_mean_word_length) + geom_col(aes(x = party, y = count)) + scale_fill_manual(values=party_colors)
ggplot(party_word_lengths_pct) + geom_col(aes(x = lgt, y = pct, fill = party), position = "dodge") + scale_fill_manual(values=party_colors)
