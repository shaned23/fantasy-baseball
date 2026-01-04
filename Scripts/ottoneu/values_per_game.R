teams = 20
games = 162
rp.nerf = 0.67
ip.limit = 1500
rp.ip = 350
budget = 400
bat.split = NA
min.bid = 1
slots = 40


ottoneu.values <- function(teams
                           ,games = 162
                           ,rp.nerf = 0.67
                           ,ip.limit = 1500
                           ,rp.ip = 350
                           ,budget = 400
                           ,bat.split = NA
                           ,min.bid = 1
                           ,slots = 40){

  # Hitters
  hit.cuts <- tibble(
     pos = c('C','1B','2B','3B','SS','OF')
    ,set = c(NA,NA,'MI',NA,'MI',NA)
    ,games = games
    ,slots = c(1,1,1,1,1,5)
    ,total.games = games * slots * teams
  )
  
  hit.df = hit.on
  max.test <- 0
  while (max.test != 1) {
    
    # Find replacement values for single positions
    hit.rv.players <- hit.df %>%
      left_join(select(hit.cuts,-set), by = 'pos') %>%
      arrange(pos,-pts.per.g) %>%
      group_by(pos) %>%
      mutate(sum.g = cumsum(g)
             ,diff = sum.g - total.games) %>%
      filter(diff >= 0)
    
    hit.rv1 <- hit.rv.players %>%
      mutate(rank = row_number()) %>%
      filter(between(rank,2,ceiling(teams/2) + 1)) %>%
      summarise(rv.ppg = mean(pts.per.g)) %>%
      ungroup()
      
    # Find replacement values for MI 
    hit.rv.playersMI <- hit.rv.players %>%
      ungroup() %>%
      filter(set == 'MI') %>%
      select(!(games:diff)) %>%
      left_join(filter(hit.cuts,set == 'MI'), by = join_by(pos,set)) %>%
      select(-pos) %>%
      distinct() %>%
      arrange(set,-pts.per.g) %>%
      group_by(set) %>%
      mutate(sum.g = cumsum(g)
             ,diff = sum.g - total.games) %>%
      filter(diff >= 0)
    
    hit.rvMI <- hit.rv.playersMI %>%
      mutate(rank = row_number()) %>%
      filter(between(rank,2,ceiling(teams/2) + 1)) %>%
      summarise(rv.ppg.mi = mean(pts.per.g)) %>%
      ungroup()
    
    # Find replacement values for UTIL
    hit.rv.playersUTIL <- hit.rv.players %>%
      ungroup() %>%
      filter(set != 'MI' | is.na(set)) %>%
      bind_rows(hit.rv.playersMI) %>%
      select(!(pos:set)) %>%
      select(!(games:diff)) %>%
      distinct() %>%
      mutate(total.games = games * teams) %>%
      arrange(-pts.per.g) %>%
      mutate(sum.g = cumsum(g)
             , diff = sum.g - total.games) %>%
      filter(diff >= 0)
    
    hit.rvUTIL <- hit.rv.playersUTIL %>%
      mutate(rank = row_number()) %>%
      filter(between(rank,2,ceiling(teams/2) + 1)) %>%
      summarise(rv.ppg.util = mean(pts.per.g)) %>%
      ungroup()
    
    # Combine the RVs to get PAR
    hitter.chart <- hit.df %>%
      left_join(hit.rv1, by = 'pos') %>%
      left_join(hit.rvMI, by = 'set') %>%
      cross_join(hit.rvUTIL) %>%
      rowwise() %>%
      mutate(rv = min(across(starts_with('rv.ppg')), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(par = (pts.per.g - rv) * g) %>%
      group_by(fg.id) %>%
      arrange(-par,rv.ppg) %>%
      mutate(rank = row_number()
             , player.pos.count = n()) %>%
      filter(rank == 1 | rank < player.pos.count) %>%
      mutate(player.count = n()) %>%
      ungroup()
    
    max.test <- max(hitter.chart$player.count)
    print(max.test)
    
    if (max.test != 1) {
      hit.df <- select(hitter.chart,fg.id,pos,set,weeks,g,total.pts,pts.per.g,pts.per.week)
    } else {
      hit.df <- hitter.chart %>%
        select(fg.id,pos,weeks,g,total.pts,pts.per.g,par)
    }
  
    
  }
  
  ## Pitchers
  pitch.cuts <- tibble(
    pos = c('SP','RP')
    , ip = c(ip.limit - rp.ip,rp.ip)
    , total.ip = ip*teams
  ) %>%
    select(-ip)
  
  # Find replacement values for single positions
  pitch.rv.players <- pitch.on %>%
    left_join(pitch.cuts, by = 'pos') %>%
    group_by(pos) %>%
    arrange(-pts.per.ip) %>%
    mutate(sum.ip = cumsum(ip)
           ,diff = sum.ip - total.ip) %>%
    filter(diff >= 0)
  
  pitch.rv1 <- pitch.rv.players %>%
    mutate(rank = row_number()) %>%
    filter(between(rank,2,teams + 1)) %>%
    summarise(rv.ip = mean(pts.per.ip)) %>%
    ungroup()
  
  # Combine the RVs to get PAR
  pitcher.chart <- pitch.on %>%
    left_join(pitch.rv1, by = 'pos') %>%
    mutate(par = (pts.per.ip - rv.ip) * ip
           , par = if_else(pos == 'RP', par * rp.nerf, par)) %>%
    group_by(fg.id) %>%
    arrange(-par) %>%
    mutate(rank = row_number()
           , player.pos.count = n()) %>%
    filter(rank == 1) %>%
    ungroup() %>%
    select(-c(rank,player.pos.count))
  
  ###########
  # Dollar values
  total.budget = budget * teams
  bid.budget = total.budget - slots * teams * min.bid
  
  # Nerf RP values here
  hit.par = sum(filter(hit.df,par > 0)$par)
  pitch.par = sum(filter(pitcher.chart,par > 0)$par)
  if (is.na(bat.split)) {
    bat.split = hit.par / (hit.par + pitch.par)
  }
  print(bat.split)
  hit.bid.budget = bid.budget * bat.split
  pitch.bid.budget = bid.budget * (1 - bat.split)
  
  hit.values <- hit.df %>%
    mutate(value = case_when(
      par == 0 ~ min.bid
      ,par >= 0 ~ (par / hit.par) * hit.bid.budget + min.bid
      ,par < 0 ~ (par / hit.par) * hit.bid.budget)
      ) %>%
    rename(best.pos = pos) %>%
    left_join(select(hit.proj,name,fg.id), by = 'fg.id') %>%
    select(fg.id,name,best.pos,g,total.pts,pts.per.g,value)
  
  pitch.values <- pitcher.chart %>%
    mutate(value = case_when(
      par == 0 ~ min.bid
      ,par >= 0 ~ (par / pitch.par) * pitch.bid.budget + min.bid
      ,par < 0 ~ (par / pitch.par) * pitch.bid.budget)
    ) %>%
    rename(best.pos = pos) %>%
    left_join(select(pitch.proj,name,fg.id), by = 'fg.id') %>%
    select(fg.id,name,best.pos,ip,total.pts,pts.per.ip,value)
  
  output.values <- bind_rows(
    hit.values
    , pitch.values
  ) %>%
  mutate(`g/ip` = coalesce(g,ip)
         , `ppg/ip` = coalesce(pts.per.g,pts.per.ip)) %>%
    select(fg.id,name,value,best.pos, total.pts, `g/ip`, `ppg/ip`) %>%
    left_join(on.widepos, by = 'fg.id') %>%
    left_join(distinct(other.raw,pos2,fg.id), by = 'fg.id') %>%
    mutate(pos = if_else(is.na(pos),pos2,pos)) %>%
    select(-pos2) %>%
    left_join(pitch.other.positions,by = 'fg.id') %>%
    mutate(pos = if_else(is.na(pos),pos2,pos)) %>%
    select(-pos2) %>%
    mutate(pos = if_else(is.na(pos),best.pos,pos)
           , pos = if_else(fg.id == '19755', 'Util', pos)
           ) %>%
    filter(!is.na(value)) %>%
    verify(!is.na(pos)) %>%
    left_join(select(fg.on.xwalk,fg.id,ottoneu.id), by = 'fg.id') %>%
    arrange(-value) %>%
    select(fg.id,name,pos,value, ottoneu.id, total.pts, `g/ip`, `ppg/ip`)
  
  return(output.values)
    
}

ottoneu12 <- ottoneu.values(teams = 12, bat.split = 0.67)
ottoneu20 <- ottoneu.values(teams = 20, bat.split = 0.67)

sheetname = 'Custom value inputs - FG'
leagues = c('FnL','20SD')
map(leagues, function(l){

  if(l == 'FnL'){
    df = ottoneu12
    link = 'https://docs.google.com/spreadsheets/d/1lJAGdHExdjuUx1Fu2ZDXn2uyRmPmd11Hj4gN4rh41KU/'
  }  else if(l == '20SD'){
    df = ottoneu20
    link = 'https://docs.google.com/spreadsheets/d/1Sp_XjplprhI-kyd5qwX3LRyLYCWOLr2Kv-1TRLuYDZo/'
  }

  out <-  df %>%
    mutate(Prospect = +(str_sub(fg.id,1,2) == 'sa' & value < 1)) %>%
    select(name, pos, value, fg.id, ottoneu.id, Prospect, total.pts, `g/ip`, `ppg/ip`) %>%
    rename(Name = name
           , Position = pos
           , Dollars = value
           , PlayerID = fg.id
           , 'ottoneu ID' = ottoneu.id)

  googlesheets4::write_sheet(out,link,sheetname)

})
