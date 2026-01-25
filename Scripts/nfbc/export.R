
# l = 'DC'

# leagues = c('DC', 'OC')
# leagues = c('DC')
map(leagues, function(l){
  
  
  if(l == 'DC') {
    lrfilter = 3
    teams = 15
  } else if(l == 'OC') {
    lrfilter = 2
    teams = 12
  }
  
  link = get(paste0(tolower(l),'.book'))
  WAR = nfbc.values(l)
  experimentWAR = experimental.values(l)
  
  googlesheets4::write_sheet(WAR,link,'WAR')
  googlesheets4::write_sheet(experimentWAR,link,'experimentWAR')
  googlesheets4::write_sheet(hit.proj %>%
                               select(fg.id, weeks, pa, ab, h, hr, r, rbi, bb, sb, avg, obp, slg, ops, `1b`, `2b`, `3b`, sf, hbp) %>%
                               rename(playerid = fg.id)
                             ,link,'hitter stats')
  googlesheets4::write_sheet(pitch.proj %>%
                               select(fg.id, weeks, w, era, g, gs, sv, holds, ip, tbf, h, er, bb, so, whip) %>%
                               rename(playerid = fg.id
                                      , k = so)
                             ,link,'pitcher stats')
  
  googlesheets4::write_sheet(hit.450.proj %>%
                               select(fg.id, weeks, pa, ab, h, hr, r, rbi, bb, sb, avg, obp, slg, ops, `1b`, `2b`, `3b`, sf, hbp) %>%
                               rename(playerid = fg.id)
                             ,link,'hitter 450 stats')
  googlesheets4::write_sheet(pitch.120.proj %>%
                               select(fg.id, weeks, w, era, g, gs, sv, holds, ip, tbf, h, er, bb, so, whip) %>%
                               rename(playerid = fg.id
                                      , k = so)
                             ,link,'pitcher 120 stats')
  
  googlesheets4::write_sheet(nfbc.standings %>%
    filter(league.type == !!l & league.rank <= lrfilter) %>%
    summarise(across(c(ab,ip),mean,.names = '{.col}.est'))
    , link, 'est')
  
  googlesheets4::write_sheet(replacement.stats(WAR,l),link, 'replacement')
  
})
 