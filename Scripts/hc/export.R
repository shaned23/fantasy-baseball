
  
link = hc.book
WAR = hc.values('HC')

googlesheets4::write_sheet(coef.mat,link,'coefficients')
googlesheets4::write_sheet(WAR,link,'WAR')
googlesheets4::write_sheet(hit.proj %>%
                             select(fg.id, weeks, pa, ab, h, hr, r, rbi, bb, sb, `1b`, `2b`, `3b`, sf, hbp, avg, obp, slg, ops) %>%
                             rename(playerid = fg.id) %>%
                             mutate(tb = `1b` + 2*`2b` + 3*`3b` + 4* hr
                                    , .after = hbp) %>%
                             mutate(across(pa:tb, ~./weeks, .names = 'week.{.col}'))
                           ,link,'hitter stats')
googlesheets4::write_sheet(pitch.proj %>%
                             select(fg.id, weeks, g, gs, sv, holds, ip, tbf, h, er, bb, so, qs, era, bb.9) %>%
                             mutate(svhld = sv*1.5 + holds
                                    , .after = qs) %>%
                             mutate(hip = h / ip
                                    , .before = era) %>%
                             rename(playerid = fg.id
                                    , k = so
                                    , bb9 = bb.9) %>%
                             select(-c(sv,holds)) %>%
                             mutate(across(ip:svhld, ~./weeks, .names = 'week.{.col}'))
                           ,link,'pitcher stats')
googlesheets4::write_sheet(est
  , link, 'est')

googlesheets4::write_sheet(replacement.stats(WAR,'HC'),link, 'replacement')

 