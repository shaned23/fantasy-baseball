teams = 30
rp.nerf = 0.67
budget = 125000000
bat.split = NA
min.bid = 500000
spots = 40

mendoza.values <- function(teams = 30
                           ,rp.nerf = 0.67
                           ,budget = 125000000
                           ,bat.split = NA
                           ,min.bid = 500000
                           ,spots = 40){
  
  # Hitters
  cuts <- tibble(
    pos = c('C','1B','2B','3B','SS','OF','SP','RP')
    ,slots = c(1,1,1,1,1,3,7,7)
    ,total.slots = slots * teams
  )
  
  hit.df = hit.mend
  max.test <- 0
  while (max.test != 1) {
    
    # Find replacement values for single positions
    hit.rv.players <- hit.df %>%
      left_join(cuts, by = 'pos') %>%
      arrange(pos,-total.pts) %>%
      group_by(pos) %>%
      mutate(g = 1
            , sum.g = cumsum(g)
             ,diff = sum.g - total.slots) %>%
      filter(diff >= 0)
    
    hit.rv1 <- hit.rv.players %>%
      mutate(rank = row_number()) %>%
      filter(between(rank,2,teams + 1)) %>%
      summarise(rv.pts = mean(total.pts)) %>%
      ungroup()
    
    # Find replacement values for UTIL
    hit.rv.playersUTIL <- hit.rv.players %>%
      ungroup() %>%
      select(fg.id,total.pts) %>%
      distinct() %>%
      arrange(-total.pts) %>%
      mutate(g = 1
              , sum.g = cumsum(g)
             , diff = sum.g - teams) %>%
      filter(diff >= 0)
    
    hit.rvUTIL <- hit.rv.playersUTIL %>%
      mutate(rank = row_number()) %>%
      filter(between(rank,2,teams + 1)) %>%
      summarise(rv.pts.util = mean(total.pts)) %>%
      ungroup()
    
    # Combine the RVs to get PAR
    hitter.chart <- hit.df %>%
      left_join(hit.rv1, by = 'pos') %>%
      cross_join(hit.rvUTIL) %>%
      rowwise() %>%
      mutate(rv = min(across(starts_with('rv.pts')), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(par = total.pts - rv) %>%
      group_by(fg.id) %>%
      arrange(-par,total.pts) %>%
      mutate(rank = row_number()
             , player.pos.count = n()) %>%
      filter(rank == 1 | rank < player.pos.count) %>%
      mutate(player.count = n()) %>%
      ungroup()
    
    max.test <- max(hitter.chart$player.count)
    
    if (max.test != 1) {
      hit.df <- select(hitter.chart,fg.id,pos,weeks,g,total.pts)
    } else {
      hit.df <- hitter.chart %>%
        select(fg.id,pos,weeks,g,total.pts,par)
    }
    
    print(max.test)
    
  }
  
  # Find replacement values for single positions
  pitch.rv.players <- pitch.mend %>%
    left_join(cuts, by = 'pos') %>%
    group_by(pos) %>%
    arrange(-total.pts) %>%
    mutate(g = 1
          , sum.g = cumsum(g)
           ,diff = sum.g - total.slots) %>%
    filter(diff >= 0)
  
  pitch.rv1 <- pitch.rv.players %>%
    mutate(rank = row_number()) %>%
    filter(rank == 2) %>%
    summarise(rv.pts = mean(total.pts)) %>%
    ungroup()
  
  # Combine the RVs to get PAR
  pitcher.chart <- pitch.mend %>%
    left_join(pitch.rv1, by = 'pos') %>%
    # Nerf RP values here
    mutate(par = total.pts - rv.pts
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
  bid.budget = total.budget - spots * teams * min.bid
  
  # What if we squared par for values here? Would that make the top players more valuable?
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
    select(fg.id,name,best.pos,g,total.pts,value)
  
  pitch.values <- pitcher.chart %>%
    mutate(value = case_when(
      par == 0 ~ min.bid
      ,par >= 0 ~ (par / pitch.par) * pitch.bid.budget + min.bid
      ,par < 0 ~ (par / pitch.par) * pitch.bid.budget)
    ) %>%
    rename(best.pos = pos) %>%
    left_join(select(pitch.proj,name,fg.id), by = 'fg.id') %>%
    select(fg.id,name,best.pos,ip,total.pts,value)
  
  output.values <- bind_rows(
    hit.values
    , pitch.values
  ) %>%
    select(fg.id,name,value,best.pos) %>%
    left_join(mend.widepos, by = 'fg.id') %>%
    left_join(distinct(other.raw,pos2,fg.id), by = 'fg.id') %>%
    mutate(pos = if_else(is.na(pos),pos2,pos)) %>%
    select(-pos2) %>%
    left_join(pitch.other.positions,by = 'fg.id') %>%
    mutate(pos = if_else(is.na(pos),pos2,pos)) %>%
    select(-pos2) %>%
    mutate(pos = if_else(is.na(pos),best.pos,pos)) %>%
    filter(!is.na(value)) %>%
    verify(!is.na(pos)) %>%
    select(fg.id,name,pos,value) %>%
    left_join(select(fg.on.xwalk,fg.id,ottoneu.id), by = 'fg.id') %>%
    arrange(-value)
  
  return(output.values)
  
}

    link = 'https://docs.google.com/spreadsheets/d/1GFZTsccSIEr0yqVdE6md8hrUxj9FpalBlePv0NkBuqE/'
  
    # Split should be close to 0.54
  out <-  mendoza.values(teams = 30, rp.nerf = 0.65, bat.split = .54) %>%
    select(fg.id, name, pos, value) %>%
    left_join(distinct(playerid.map,fg.id,fantrax.id) %>% filter(fantrax.id != ''), by = 'fg.id') %>%
    rename(Name = name
           , Position = pos
           , Dollars = value
           , fantrax.id = fantrax.id) %>%
      select(fantrax.id,Name,Position,Dollars,fg.id) %>%
      filter(fantrax.id != '*06als*' | is.na(fantrax.id)) %>%
    mutate(fantrax.id = case_when(
      fg.id == '13770' ~ '*02mzf*'
      , fg.id == '17170' ~ '*031fj*'
      , fg.id == '26203' ~ '*0514i*'
      , fg.id == '17871' ~ '*03qpg*'
      , fg.id == '13346' ~ '*02n0v*'
      , TRUE ~ fantrax.id
    ))
  
  googlesheets4::write_sheet(out,link,'Shane Projections')
  #googlesheets4::write_sheet(read_csv(paste0(.data,'Fantrax-Players-The Mendoza League (1).csv')),link,'2024 Current Rosters')

