library(tidyverse)
library(xml2)
library(dbtprotokoll)
library(ggplot2)

#all_prots <- parse_protocols()
#save(all_prots, file = "dataset.RData")

protocols <- parse_protocols(start="19002-data.xml", end = "19035-data.xml")

bspprotokoll <- parse_protocols(start = "19001-data.xml", end = "19050-data.xml")
kommentare <- bspprotokoll$comments
rednerInnen <- bspprotokoll$speakers
absaetze <- bspprotokoll$paragraphs

#rednerInnen$fraktion <- str_trim(rednerInnen$fraktion)
#rednerInnen$fraktion <- str_replace(rednerInnen$fraktion, "BÜNDNIS 90/.*", "bündnis90/diegrünen")
rednerInnen$fraktion <- replace_na(rednerInnen$fraktion, "andere")


kommentare %>%
  filter(str_detect(content, "Beifall ")) -> beifaelle


absaetze %>%
  #group_by(speech) %>%
  #mutate_if(is.numeric, as.character) %>%
  select(speech, 'speaker_id') %>%
  distinct(speech, .keep_all = TRUE) -> matchrede

#matchrede$speaker_id <- matchrede$speaker_id

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
  arrange(desc(bfl_pro_rede)) -> bflprorede

ggplot(bflprorede, mapping= aes(x=fraktion, y=bfl_pro_rede) ) + geom_bar(stat="identity")


#nächster Teil: wer gibt Beifall? Idee: mutate in kombination mit einer regex funktion,
#suche das, was hinter "Beifall bei der (/beim)" und "Beifall bei Abgeordneten der (/des)" steht
 beifaelle %>%
  # mutate('beifallgebend' = str_extract(content, '(?<=\\(Beifall bei der) .*|(?<=\\(Beifall beim) .*|(?<=\\(Beifall bei Abgeordneten der|s) .*')) %>%
   mutate('union' = as.integer(str_detect(content, 'CDU')), 'spd' = as.integer(str_detect(content, 'SPD')), 'fdp' = as.integer((str_detect(content, 'FDP'))), 'grüne' = as.integer(str_detect(content, 'DIE GR')), 'afd' = as.integer(str_detect(content, 'AfD')), 'linke' = as.integer(str_detect(content, 'LINKE'))) %>%
   group_by(paragraph_id) %>%
   summarize(id, 'cdu/csu' = sum(union), 'spd' = sum(spd), 'fdp' = sum(fdp), 'bündnis90/diegrünen' = sum(grüne), 'afd' = sum(afd), 'dielinke' = sum(linke)) %>%
   distinct(paragraph_id, .keep_all = TRUE) -> bflwerwann

 #jetzt wollen wir herausfinden, wer die jeweilige Rede gehalten hat
kombitabelle <- left_join(bflwerwann, matchrede, by = c('paragraph_id' = 'speech'))

rednerInnen %>%
  select(id, fraktion) -> matchredner
#matchredner$fraktion <- str_trim(matchredner$fraktion)
#matchredner$fraktion <- str_replace(matchredner$fraktion, "BÜNDNIS 90/.*", "bündnis90/diegrünen")

bflwerwem <- left_join(kombitabelle, matchredner, by= c('speaker_id' = 'id'))

bflwerwem %>%
  ungroup() %>%
  select(!c(speaker_id,id, paragraph_id)) %>%
  select('redende fraktion' = fraktion, everything()) %>%
  group_by(`redende fraktion`) %>%
  summarize('cdu/csu' = sum(`cdu/csu`), 'spd' = sum(spd), 'fdp' = sum(fdp), 'bündnis90/diegrünen' = sum(`bündnis90/diegrünen`), 'afd' = sum(afd), 'dielinke' = sum(`dielinke`)) %>%
  ungroup() %>%
  summarize(`redende fraktion`, `cdu/csu`, 'anteilig_cdu' = (`cdu/csu`/sum(`cdu/csu`)), spd, 'anteilig_spd' = (spd/sum(spd)), fdp, 'anteilig_fdp' = (fdp/sum(fdp)), `bündnis90/diegrünen`, 'anteilig_grüne' = (`bündnis90/diegrünen`/sum(`bündnis90/diegrünen`)), afd, 'anteilig_afd' = (afd/sum(afd)), `dielinke`,'anteilig_linke' = (`dielinke`/sum(`dielinke`))) -> bflwerwem_anteile
#diese Tabelle heißt bflwerwem_anteile

bflwerwem_anteile %>%
  select(`redende fraktion`,starts_with('anteilig')) -> bfl_antl


#ggplot(bflwerwem, mapping= aes(x= fraktion) ) + geom_bar()
#ggplot(bfl_antl, mapping= aes(x= `redende fraktion`)) + geom_bar(stat="identity")

bfl_antl <- select(bfl_antl, `redende fraktion`, 'cdu/csu' = anteilig_cdu, 'spd' = anteilig_spd, 'fdp' = anteilig_fdp, 'grüne' = anteilig_grüne, 'afd' = anteilig_afd, 'linke' = anteilig_linke )
bfl_antl <- pivot_longer(bfl_antl, `cdu/csu`:linke, names_to = 'beifallgebend', values_to = 'anteile')

#let's fix those colors!
party_colors <-c(
  spd = "#DF0B25",
  "cdu/csu" = "#000000",
  afd = "#1A9FDD",
  "dielinke" = "#BC3475",
  "bündnis90/diegrünen" = "#4A932B",
  fdp = "#FEEB34",
  "fraktionslos" = "#61420e",
  "andere" = "#ed9e1f",
  "zur Geschäftsordnung" = "#999999")

ggplot(bfl_antl, mapping= aes(x=beifallgebend, y=anteile, fill= `redende fraktion`)) + geom_bar(stat="identity") + scale_fill_manual(values=party_colors)

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
  arrange(desc(htk_pro_rede)) -> htkprorede

ggplot(htkprorede, mapping= aes(x=fraktion, y=htk_pro_rede) ) + geom_bar(stat="identity", aes(fill = fraktion)) + scale_fill_manual(values = party_colors)


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
  arrange(desc(lch_pro_rede)) -> lchprorede

ggplot(lchprorede, mapping= aes(x=fraktion, y=lch_pro_rede) ) + geom_bar(stat="identity")

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
  arrange(desc(zrf_pro_rede)) -> zrfprorede

ggplot(zrfprorede, mapping= aes(x=fraktion, y=zrf_pro_rede) ) + geom_bar(stat="identity")

##############################

unterbrechungen <- left_join(redenbeifall,redenheiterkeit)
unterbrechungen <- left_join(unterbrechungen, redenlachen)
unterbrechungen <- left_join(unterbrechungen, redenzurufe)

unterbrechungen

unterbrechungen %>%
  mutate('utb_pro_rede' = (beifaelle+heiterkeiten+lachen+zurufe)/reden) %>%
  arrange(desc(utb_pro_rede)) -> utbprorede

ggplot(utbprorede, mapping= aes(x=fraktion, y=utb_pro_rede) ) + geom_bar(stat="identity") + scale_fill_manual(values=party_colors)


