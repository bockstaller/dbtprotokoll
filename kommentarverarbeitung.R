library(tidyverse)
library(xml2)
library(dbtprotokoll)



bspprotokoll <- parse_protocol("./protokolle/19005-data.xml")
kommentare <- bspprotokoll$comments
rednerInnen <- bspprotokoll$speakers
absaetze <- bspprotokoll$paragraphs

kommentare %>%
  filter(str_detect(content, "Beifall ")) -> beifaelle


absaetze %>%
  #group_by(speech) %>%
  mutate_if(is.numeric, as.character) %>%
  select(speech, 'speaker_id') %>%
  distinct(speech, .keep_all = TRUE) -> matchrede

matchrede$speaker_id <- as.character(matchrede$speaker_id)

kombitabelle <- left_join(beifaelle, matchrede, by=c('paragraph_id'= 'speech'))

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


#nächster Teil: wer gibt Beifall? Idee: mutate in kombination mit einer regex funktion,
#suche das, was hinter "Beifall bei der (/beim)" und "Beifall bei Abgeordneten der (/des)" steht
 beifaelle %>%
  # mutate('beifallgebend' = str_extract(content, '(?<=\\(Beifall bei der) .*|(?<=\\(Beifall beim) .*|(?<=\\(Beifall bei Abgeordneten der|s) .*')) %>%
   mutate('union' = as.integer(str_detect(content, 'CDU')), 'spd' = as.integer(str_detect(content, 'SPD')), 'fdp' = as.integer((str_detect(content, 'FDP'))), 'grüne' = as.integer(str_detect(content, 'DIE GR')), 'afd' = as.integer(str_detect(content, 'AfD')), 'linke' = as.integer(str_detect(content, 'LINKE'))) %>%
   group_by(paragraph_id) %>%
   summarize(id, 'CDU/CSU' = sum(union), 'SPD' = sum(spd), 'FDP' = sum(fdp), 'BÜNDNIS 90/DIE GRÜNEN' = sum(grüne), 'AfD' = sum(afd), 'DIE LINKE' = sum(linke)) %>%
   distinct(paragraph_id, .keep_all = TRUE) -> bflwerwann

 #jetzt wollen wir herausfinden, wer die jeweilige Rede gehalten hat
kombitabelle <- left_join(bflwerwann, matchrede, by = c('paragraph_id' = 'speech'))

rednerInnen %>%
  select(id, fraktion) -> matchredner

bflwerwem <- left_join(kombitabelle, matchredner, by= c('speaker_id' = 'id'))

bflwerwem %>%
  ungroup() %>%
  select(!c(speaker_id,id, paragraph_id)) %>%
  select('redende fraktion' = fraktion, everything()) %>%
  group_by(`redende fraktion`) %>%
  summarize('CDU/CSU' = sum(`CDU/CSU`), 'SPD' = sum(SPD), 'FDP' = sum(FDP), 'BÜNDNIS 90/DIE GRÜNEN' = sum(`BÜNDNIS 90/DIE GRÜNEN`), 'AfD' = sum(AfD), 'DIE LINKE' = sum(`DIE LINKE`)) %>%
  ungroup() %>%
  summarize(`redende fraktion`, `CDU/CSU`, 'anteilig_CDU' = (`CDU/CSU`/sum(`CDU/CSU`)), SPD, 'anteilig_SPD' = (SPD/sum(SPD)), FDP, 'anteilig_FDP' = (FDP/sum(FDP)), `BÜNDNIS 90/DIE GRÜNEN`, 'anteilig_GRÜNE' = (`BÜNDNIS 90/DIE GRÜNEN`/sum(`BÜNDNIS 90/DIE GRÜNEN`)), AfD, 'anteilig_AfD' = (AfD/sum(AfD)), `DIE LINKE`,'anteilig_LINKE' = (`DIE LINKE`/sum(`DIE LINKE`))) -> bflwerwem_anteile
#diese Tabelle heißt bflwerwem_anteile

bflwerwem_anteile %>%
  select(`redende fraktion`,starts_with('anteilig'))

####################
kommentare %>%
  filter(str_detect(content, "Heiterkeit ")) -> heiterkeiten

kombitabelle <- left_join(heiterkeiten, matchrede, by=c('paragraph_id'= 'speech'))

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
  filter(str_detect(content, "Lachen ")) -> lachen

kombitabelle <- left_join(lachen, matchrede, by=c('paragraph_id'= 'speech'))

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
  filter(str_detect(content, "Zuruf")) -> zurufe

kombitabelle <- left_join(zurufe, matchrede, by=c('paragraph_id'= 'speech'))

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

