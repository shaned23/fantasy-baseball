

leagues = c('DC','OC')
map(leagues, function(l){
  
  
  if(league == 'DC') {
    lrfilter = 3
    teams = 15
  } else if(league == 'OC') {
    lrfilter = 2
    teams = 12
  }
  
  link = get(paste0(tolower(l),'.book'))
  WAR = nfbc.values(l)
  
  googlesheets4::write_sheet(WAR,link,'WAR')
  googlesheets4::write_sheet(hit.proj %>%
                               select(fg.id, weeks, pa, ab, h, hr, r, rbi, bb, sb, avg, obp, slg, ops, `1b`, `2b`, `3b`, sf, hbp) %>%
                               rename(playerid = fg.id)
                             ,link,'hitter stats')
  googlesheets4::write_sheet(pitch.proj %>%
                               select(fg.id, weeks, w, era, g, gs, sv, holds, ip, tbf, h, er, bb, so, whip) %>%
                               rename(playerid = fg.id
                                      , k = so)
                             ,link,'pitcher stats')
  googlesheets4::write_sheet(nfbc2023 %>%
    filter(league.type == !!l & league.rank <= lrfilter) %>%
    summarise(across(c(ab,ip),mean,.names = '{.col}.est'))
    , link, 'est')
  
})
 