library(tidyverse)
library(xml2)
library(dbtprotokoll)



bspprotokoll <- parse_protokoll("./protokolle/19005-data.xml")
kommentare <- bspprotokoll$kommentarliste
rednerInnen <- bspprotokoll$rednerliste
absaetze <- bspprotokoll$absatzliste

kommentare %>%
  filter(str_detect(inhalt, "Beifall ")) -> beifaelle


absaetze %>%
  #group_by(speech) %>%
  mutate_if(is.numeric, as.character) %>%
  select(speech, 'speaker_id') %>%
  distinct(speech, .keep_all = TRUE) -> matchrede

matchrede$speaker_id <- as.character(matchrede$speaker_id)

kombitabelle <- left_join(beifaelle, matchrede, by=c('absatz_id'= 'speech'))

#find fraction in whose speech applause was given
rednerInnen %>%
  select(id, vorname, nachname, fraktion) -> matchredner

beifallanzahl <- left_join(kombitabelle, matchredner, by = c('speaker_id'='id'))

#now we have a tibble with all relevant information for analysis.
#we can now count how much applause each faction gets, how much applause philipp amthor gets compared to the other cdu people etc

beifallanzahl %>%
  group_by(fraktion) %>%
  summarize('beifaelle' =n()) %>%
  arrange(desc(beifaelle)) -> bflfraktion

#now lets find out how many speeches each fraction actually gets
rede_redner <- left_join(matchrede, matchredner, by = c('speaker_id'='id'))

rede_redner %>%
  group_by(fraktion) %>%
  summarize('reden' = n()) %>%
  arrange(desc(reden)) -> rdnfraktion

#now lets combine our information!
redenbeifall <- left_join(rdnfraktion, bflfraktion)

redenbeifall %>%
  mutate('bfl_pro_rede'=beifaelle/reden) %>%
  arrange(desc(bfl_pro_rede))


####################
kommentare %>%
  filter(str_detect(inhalt, "Heiterkeit ")) -> heiterkeiten

kombitabelle <- left_join(heiterkeiten, matchrede, by=c('absatz_id'= 'speech'))

heiteranzahl <- left_join(kombitabelle, matchredner, by = c('speaker_id'='id'))

heiteranzahl %>%
  group_by(fraktion) %>%
  summarize('heiterkeiten' =n()) %>%
  arrange(desc(heiterkeiten)) -> htkfraktion

redenheiterkeit <- left_join(rdnfraktion, htkfraktion)

redenheiterkeit %>%
  mutate('htk_pro_rede'=heiterkeiten/reden) %>%
  arrange(desc(htk_pro_rede))

###########################

kommentare %>%
  filter(str_detect(inhalt, "Lachen ")) -> lachen

kombitabelle <- left_join(lachen, matchrede, by=c('absatz_id'= 'speech'))

lachanzahl <- left_join(kombitabelle, matchredner, by = c('speaker_id'='id'))

lachanzahl %>%
  group_by(fraktion) %>%
  summarize('lachen' =n()) %>%
  arrange(desc(lachen)) -> lchfraktion

redenlachen <- left_join(rdnfraktion, lchfraktion)

redenlachen %>%
  mutate('lch_pro_rede'=lachen/reden) %>%
  arrange(desc(lch_pro_rede))
######################################

kommentare %>%
  filter(str_detect(inhalt, "Zuruf")) -> zurufe

kombitabelle <- left_join(zurufe, matchrede, by=c('absatz_id'= 'speech'))

zurufanzahl <- left_join(kombitabelle, matchredner, by = c('speaker_id'='id'))

zurufanzahl %>%
  group_by(fraktion) %>%
  summarize('zurufe' =n()) %>%
  arrange(desc(zurufe)) -> zrffraktion

redenzurufe <- left_join(rdnfraktion, zrffraktion)

redenzurufe %>%
  mutate('zrf_pro_rede'=zurufe/reden) %>%
  arrange(desc(zrf_pro_rede))

##############################

unterbrechungen <- left_join(redenbeifall,redenheiterkeit)
unterbrechungen <- left_join(unterbrechungen, redenlachen)
unterbrechungen <- left_join(unterbrechungen, redenzurufe)

unterbrechungen
