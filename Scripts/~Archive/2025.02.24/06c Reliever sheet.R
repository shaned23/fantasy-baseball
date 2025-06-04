
# Create an RP output sheet for DC and OC
rp.output <- rp.oopsy %>%
  filter(sv >= 2 | hld >= 2) %>%
  select(name, so, sv, era, whip, k.perc, bb.perc, playerid) %>%
  rename(k = so) %>%
  left_join(select(rp.stuffplus, playerid, stuff_plus, location_plus), by = 'playerid') %>%
  left_join(select(rp.winprob, playerid, gmli), by = 'playerid') %>%
  left_join(best.by.class, by = 'playerid') %>%
  left_join(hhi.rp, by = 'playerid') %>%
  left_join(rank.rp, by = 'playerid') %>%
  select(playerid, name, team, k, sv, era, whip, k.perc, bb.perc, gmli, hhi
         , era.rank, stuffplus.rank, stuff_plus, location_plus
         , ends_with('Fastball'), ends_with('Secondary')) %>%
  arrange(-stuff_plus)

map(leagues, function(l){
  link = get(paste0(tolower(l),'.book'))
  googlesheets4::write_sheet(rp.output,link,'RP sheet')
})
