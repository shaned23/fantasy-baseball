
# 
# Create an RP output sheet for DC and OC
rp.output <- rp.oopsy %>%
  filter(sv >= 2 | hld >= 2) %>%
  select(name, so, sv, era, whip, k.perc, bb.perc, playerid) %>%
  rename(k = so) %>%
  left_join(select(rp.stuffplus, playerid, stuff_plus, location_plus), by = 'playerid') %>%
  left_join(select(rp.winprob, playerid, gmli), by = 'playerid') %>%
  left_join(best.by.class, by = 'playerid') %>%
  left_join(hhi.rp, by = 'playerid') %>%
  left_join(rank.rp, by = 'playerid')  %>%
  left_join(distinct(playerid.map,fg.id,fantrax.id) %>% filter(fantrax.id != '') %>% rename(playerid = fg.id), by = 'playerid') %>%
  filter(fantrax.id != '*06als*' | is.na(fantrax.id)) %>%
  mutate(fantrax.id = case_when(
    playerid == '13770' ~ '*02mzf*'
    , playerid == '17170' ~ '*031fj*'
    , playerid == '26203' ~ '*0514i*'
    , playerid == '17871' ~ '*03qpg*'
    , playerid == '13346' ~ '*02n0v*'
    , TRUE ~ fantrax.id
  )) %>%
  select(fantrax.id, name, team, k, sv, era, whip, k.perc, bb.perc, gmli, hhi
         , era.rank, stuffplus.rank, stuff_plus, location_plus
         , ends_with('Fastball'), ends_with('Secondary')) %>%
  arrange(-stuff_plus)


  link = 'https://docs.google.com/spreadsheets/d/1GFZTsccSIEr0yqVdE6md8hrUxj9FpalBlePv0NkBuqE/'
  googlesheets4::write_sheet(rp.output,link,'r.RP sheet')
