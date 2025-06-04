
# Set parameters
# league = 'DC'

nfbc.values <- function(league) {
  
  if(league == 'DC') {
    lrfilter = 3
    teams = 15
  } else if(league == 'OC') {
    lrfilter = 2
    teams = 12
  }
  
  slots = 23
  total = 30
  seasonweeks = 25
  c = 2
  hits = 14
  pits = 9
  scal.hit = (hits - 1)/hits
  scal.pit = (pits - 1)/pits
  ll = tolower(league)
  base = get(paste0('base.',ll))
  adp.data <- get(paste0('adp.',ll))
  
  # Create positions slots
  # Hitters
  if(league == 'OC') {
    cuts <- tibble(
      pos = c('C','1B','2B','3B','SS','OF','P')
      ,set = c(NA,'CI','MI','CI','MI',NA,NA)
      ,slots = c(2,1,1,1,1,5,9)
      ,total.weeks = seasonweeks * slots * teams
    )
  } else if (league == 'DC') {
    cuts <- tibble(
      pos = c('C','1B','2B','3B','SS','OF','P')
      ,set = c(NA,'CI','MI','CI','MI',NA,NA)
      ,slots = c(2,1,1,1,1,5,9)
      ,total.weeks = seasonweeks * slots * teams
    )    
  }
  
  # Find the prior above replacement players
  prior <- base
  
  # Calculate AB and IP estimates
  est <- nfbc.standings %>%
    filter(league.type == !!league & league.rank <= lrfilter) %>%
    summarise(across(c(ab,ip),mean,.names = '{.col}.est'))
  ab.est = est$ab.est[1]
  ip.est = est$ip.est[1]
  
  test = 0
  
  while(test == 0){
    
    # Find average of available players
    hit.mid <- hit.nfbc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement == 1)) %>%
      filter(above.replacement) %>%
      summarise(across(ab:sb,~mean(., na.rm = TRUE),.names = 'mid.{.col}')
                , across(c(ab,h),~sum(., na.rm = TRUE))) %>%
      mutate(across(starts_with('mid.'),~.*hits,.names = 'sum{.col}')) %>%
      cross_join(get(paste0('coef.hit.',league))) %>%
      mutate(mid.avg = h/ab
             , .after = mid.sb) %>%
      mutate(summid.h = `0.5.avg` * ab.est
             , summid.ab = ab.est
             , summid.avg = `0.5.avg`) %>%
      select(contains('mid.'))
    
    hit.repl.stats1 <-  hit.nfbc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement)) %>%
      filter(!above.replacement & !is.na(avg)) %>%
      group_by(pos) %>%
      arrange(rank) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= teams) %>%
      summarise(across(ab:sb,~mean(., na.rm = TRUE),.names = 'repl.{.col}')) %>%
      filter(pos != 'UT')
    
    hit.repl.stats <- hit.repl.stats1 %>%
      summarise(across(where(is.numeric),max)) %>%
      mutate(pos = 'UT') %>%
      bind_rows(hit.repl.stats1)
    
    pitch.mid <- pitch.nfbc %>%
      left_join(base, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,TRUE)) %>%
      filter(above.replacement) %>%
      summarise(across(c(ip,er,h,bb,w,sv,k),~mean(., na.rm = TRUE), .names = 'mid.{.col}')
                , across(c(ip,er,h,bb),~sum(., na.rm = TRUE))) %>%
      mutate(across(starts_with('mid.'),~.*pits,.names = 'sum{.col}')) %>%
      cross_join(get(paste0('coef.pitch.',league))) %>%
      mutate(mid.era = er*9/ip,
             mid.whip = (h+bb)/ip
             , .after = mid.k) %>%
      mutate(summid.er = (`0.5.era` * ip.est)/9
             , summid.h = (`0.5.whip` * ip.est) * (h/(h+bb))
             , summid.bb = (`0.5.whip` * ip.est) * (bb/(h+bb))
             , summid.ip = ip.est
             , summid.era = `0.5.era`
             , summid.whip = `0.5.whip`) %>%
      select(contains('mid.'))
    
    pitch.repl.stats <-  pitch.nfbc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement)) %>%
      filter(!above.replacement & !is.na(era)) %>%
      arrange(rank) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= teams*2) %>%
      summarise(across(c(ip,er,h,bb,w,sv,k),mean, .names = 'repl.{.col}'))
    
    # Create sgp for hitters
    # Add on the coef data
    hit.df <- hit.nfbc %>%
      cross_join(hit.mid) %>%
      cross_join(get(paste0('coef.hit.',league))) %>%
      left_join(hit.repl.stats, by = 'pos')

    
    if(league == 'OC') {
      
      map(hitlist.count,function(catg) {
        
        hit.df <<- hit.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)
                                     + get(paste0('0.5.',catg))
                                     - get(paste0('mid.',catg))
                                     + get(paste0('repl.',catg)) * (seasonweeks - weeks) / seasonweeks
        )
        
      })      
      
      ## AVG
      hit.df <- hit.df %>% mutate(v1.avg = (h + summid.h - mid.h + repl.h * (seasonweeks - weeks)/seasonweeks)
                                  / (ab + summid.ab - mid.ab + repl.ab * (seasonweeks - weeks)/seasonweeks)
      )
      
    } else if(league == 'DC') {
      
      map(hitlist.count,function(catg) {
        
        hit.df <<- hit.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)
                                     + get(paste0('0.5.',catg))
                                     - get(paste0('mid.',catg))
        )
        
      })      
      
      ## AVG
      hit.df <- hit.df %>% mutate(v1.avg = (h + summid.h - mid.h)
                                  / (ab + summid.ab - mid.ab)
      )
    }

    
    # Run probs
    hit.prob <- function(catg) {
      
      hit.df <<- hit.df %>% mutate(!!as.name(paste0('spg.',catg)) :=
                                     (# Calculate e^(b0 + b1*x1)
                                       (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))
                                        # Divide by 1 + e^(b0 + b1*x1)
                                        / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))))
                                       # Caclulate the probablities of the average team
                                       - (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg)))
                                          / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg))))))
                                   # Multiply by teams
                                   * (teams - 1) + 1
      )
      
    }
    map(hit.cats,hit.prob)
    
    # Sum up the spgs
    hit.spg <- hit.df %>%
      rowwise() %>%
      mutate(spg = sum(c_across(starts_with('spg')))) %>%
      ungroup() %>%
      select(fg.id, pos, weeks, spg) %>%
      left_join(select(cuts,pos,set), by = 'pos')
    
    # Find replacement values
    max.test = 0
    while(max.test != 1) {
      
      # Find replacement values for single positions
      hit.rv.players <- hit.spg %>%
        left_join(select(cuts,-set), by = 'pos') %>%
        arrange(pos,-spg) %>%
        group_by(pos) %>%
        mutate(sum.weeks = cumsum(weeks)
               ,diff = sum.weeks - total.weeks) %>%
        filter(diff >= 0)
      
      hit.rv1 <- hit.rv.players %>%
        mutate(rank = row_number()) %>%
        filter(between(rank,2,ceiling(teams/2) + 1)) %>%
        summarise(rv.spg = mean(spg, na.rm = TRUE)) %>%
        ungroup()
      
      # Find replacement values for CI/MI
      hit.rv.players.sub <- hit.rv.players %>%
        ungroup() %>%
        filter(set %in% c('CI','MI')) %>%
        select(!(slots:diff)) %>%
        left_join(filter(cuts,set %in% c('CI','MI')), by = join_by(pos,set)) %>%
        select(-pos) %>%
        distinct() %>%
        arrange(set,-spg) %>%
        group_by(set) %>%
        mutate(sum.weeks = cumsum(weeks)
               ,diff = sum.weeks - total.weeks) %>%
        filter(diff >= 0)
      
      hit.rv.sub <- hit.rv.players.sub %>%
        mutate(rank = row_number()) %>%
        filter(between(rank,2,ceiling(teams/2) + 1)) %>%
        summarise(rv.spg.sub = mean(spg, na.rm = TRUE)) %>%
        ungroup()
      
      # Find replacement values for UTIL
      hit.rv.playersUTIL <- hit.rv.players %>%
        ungroup() %>%
        filter((set != 'MI' & set != 'CI') | is.na(set)) %>%
        bind_rows(hit.rv.players.sub) %>%
        select(!(slots:diff)) %>%
        distinct() %>%
        mutate(total.weeks = weeks * teams) %>%
        arrange(-spg) %>%
        mutate(sum.weeks = cumsum(weeks)
               , diff = sum.weeks - total.weeks) %>%
        filter(diff >= 0)
      
      hit.rvUTIL <- hit.rv.playersUTIL %>%
        mutate(rank = row_number()) %>%
        filter(between(rank,2,ceiling(teams/2) + 1)) %>%
        summarise(rv.spg.util = mean(spg, na.rm = TRUE)) %>%
        ungroup()
      
      # Combine the RVs to get PAR
      hitter.chart <- hit.spg %>%
        left_join(hit.rv1, by = 'pos') %>%
        left_join(hit.rv.sub, by = 'set') %>%
        cross_join(hit.rvUTIL) %>%
        rowwise() %>%
        mutate(rv = min(across(starts_with('rv.spg')), na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(par = spg - rv) %>%
        group_by(fg.id) %>%
        arrange(-par,rv.spg) %>%
        mutate(rank = row_number()
               , player.pos.count = n()) %>%
        filter(rank == 1 | rank < player.pos.count) %>%
        mutate(player.count = n()) %>%
        ungroup()
      
      max.test <- max(hitter.chart$player.count)
      print(max.test)
      if (max.test != 1) {
        print('repeat')
        hit.spg <- select(hitter.chart,fg.id,set,pos,weeks,spg)
      } else {
        print('end')
        hitter.chart <<- hitter.chart %>%
          select(fg.id,pos,weeks,spg,par,rv)
        # %>%
        #   rename(best.pos = pos)
      }
      
    }
    
    
    # PITCHING
    pitch.df <- pitch.nfbc %>%
      cross_join(pitch.mid) %>%
      cross_join(get(paste0('coef.pitch.',league))) %>%
      cross_join(pitch.repl.stats)
    
    if(league == 'OC') {
      
      map(pitlist.count,function(catg) {
        
        pitch.df <<- pitch.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)
                                         + get(paste0('0.5.',catg))
                                         - get(paste0('mid.',catg))
                                         + get(paste0('repl.',catg)) * (seasonweeks - weeks)/seasonweeks
        )
        
      })      
      
      ## ERA
      pitch.df <- pitch.df %>% mutate(v1.era = (er + summid.er - mid.er + repl.er * (seasonweeks - weeks)/seasonweeks)*9
                                      / (ip + summid.ip - mid.ip + repl.ip * (seasonweeks - weeks)/seasonweeks)
      )
      
      ## WHIP
      pitch.df <- pitch.df %>% mutate(v1.whip = (h + bb + summid.h + summid.bb - (mid.h + mid.bb) + (repl.h + repl.bb)*((seasonweeks - weeks)/seasonweeks))
                                      / (ip + summid.ip - mid.ip + repl.ip * (seasonweeks - weeks)/seasonweeks)
      )
      
    } else if(league == 'DC') {
      
      map(pitlist.count,function(catg) {
        
        pitch.df <<- pitch.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)
                                         + get(paste0('0.5.',catg))
                                         - get(paste0('mid.',catg))
        )
        
      })      
      
      ## ERA
      pitch.df <- pitch.df %>% mutate(v1.era = (er + summid.er - mid.er)*9
                                      / (ip + summid.ip - mid.ip)
      )
      
      ## WHIP
      pitch.df <- pitch.df %>% mutate(v1.whip = (h + bb + summid.h + summid.bb - (mid.h + mid.bb))
                                      / (ip + summid.ip - mid.ip)
      )
      
    }
    

    # Run probs
    pitch.prob <- function(catg) {
      
      pitch.df <<- pitch.df %>% mutate(!!as.name(paste0('spg.',catg)) :=
                                     (# Calculate e^(b0 + b1*x1)
                                       (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))
                                        # Divide by 1 + e^(b0 + b1*x1)
                                        / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))))
                                       # Caclulate the probablities of the average team
                                       - (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg)))
                                          / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg))))))
                                   # Multiply by teams
                                   * (teams - 1) + 1
      )
      
    }
    map(pitch.cats,pitch.prob)
    
    pitch.spg <- pitch.df %>%
      rowwise() %>%
      mutate(spg = sum(c_across(starts_with('spg')))) %>%
      ungroup() %>%
      select(fg.id, weeks, spg) %>%
      mutate(pos = 'P') %>%
      left_join(select(cuts,pos,set), by = 'pos')
    
    # Find replacement values for single positions
    pitch.rv.players <- pitch.spg %>%
      left_join(select(cuts,-set), by = 'pos') %>%
      arrange(pos,-spg) %>%
      group_by(pos) %>%
      mutate(sum.weeks = cumsum(weeks)
             ,diff = sum.weeks - total.weeks) %>%
      filter(diff >= 0)
    
    pitch.rv <- pitch.rv.players %>%
      mutate(rank = row_number()) %>%
      filter(between(rank,2,ceiling(teams/2) + 1)) %>%
      summarise(rv.spg = mean(spg)) %>%
      ungroup()
    
    # Combine the RVs to get PAR
    pitcher.chart <- pitch.spg %>%
      left_join(pitch.rv, by = 'pos') %>%
      rowwise() %>%
      mutate(rv = min(across(starts_with('rv.spg')), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(par = spg - rv)  %>%
      select(fg.id,pos,weeks,spg,par,rv)
    
    # Find the list of players abovereplacement
    prior.new <- bind_rows(
      hitter.chart
      , pitcher.chart
    ) %>%
      arrange(desc(par)) %>%
      mutate(rank = row_number()
             , above.replacement = par > 0) %>%
      distinct(fg.id, rank, above.replacement) %>%
      filter(!is.na(above.replacement))
    
    if(isTRUE(all.equal(prior %>% distinct(fg.id) %>% arrange(fg.id), 
                        prior.new %>% distinct(fg.id) %>% arrange(fg.id), convert = TRUE))) {
      test = 1
      print(hitter.chart)
      print(pitcher.chart)
      
      # Combine
      combine <- bind_rows(
        select(hitter.chart, fg.id, pos, weeks, spg, par, rv) %>% mutate(pos = if_else(is.na(pos),'UT',pos))
        , pitcher.chart %>% mutate(pos = if_else(is.na(pos),'P',pos))
      ) 
      
      unique.players <- combine %>%
        group_by(fg.id) %>%
        filter(par == max(par)) %>%
        distinct(fg.id, pos)
      
      output.values <- combine %>%
        inner_join(unique.players, by = c('fg.id', 'pos')) %>%
        rename(best.pos = pos) %>%
        left_join(nfbc.pos.wide, by = 'fg.id') %>%
        mutate(pos = if_else(best.pos == 'P','P',pos)) %>%
        left_join(select(adp.data,-c(player,pos)), by = 'fg.id') %>%
        left_join(bind_rows(hit.proj, pitch.proj) %>% distinct(fg.id,name), by = 'fg.id') %>%
        select(fg.id, name, team, pos, best.pos, weeks, spg, rv, par, adp, min.pick, max.pick) %>%
        rename(PlayerID = fg.id
               , Name = name
               , Team = team
               , Position = pos
               , Best = best.pos
               , winsadded = spg
               , war = par
               , ADP = adp
               , `Min Pick` = min.pick
               , `Max Pick` = max.pick
               ) %>%
        arrange(desc(war))
      
    } else {
      prior <- prior.new %>%
        group_by(fg.id) %>%
        summarise(rank = min(rank)
                  , above.replacement = max(above.replacement)
                  , .groups = 'drop')
    }
  
  }
  
  return(output.values)
}

replacement.stats <- function(o, league) {
  
  if(league == 'DC') {
    lrfilter = 3
    teams = 15
  } else if(league == 'OC') {
    lrfilter = 2
    teams = 12
  }
  
  slots = 23
  total = 30
  seasonweeks = 25
  c = 2
  hits = 14
  pits = 9
  scal.hit = (hits - 1)/hits
  scal.pit = (pits - 1)/pits
  ll = tolower(league)
  
  # Create positions slots
  # Hitters
  cuts <- tibble(
    pos = c('C','1B','2B','3B','SS','OF','P')
    ,set = c(NA,'CI','MI','CI','MI',NA,NA)
    ,slots = c(2,1,1,1,1,5,9)
    ,total.weeks = seasonweeks * slots * teams
  )
  
  # Limit to replacement level players
  rep <- o %>%
    filter(war < 0) %>%
    mutate(hitter.flag = Best != 'P') %>%
    left_join(hit.nfbc, by = join_by(PlayerID == fg.id)) %>%
    left_join(pitch.nfbc, by = join_by(PlayerID == fg.id)) %>%
    mutate(h = coalesce(h.x,h.y)) %>%
    group_by(Best) %>%
    arrange(desc(war)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= teams * 2) %>%
    summarise(across(c(ab,h,hr,r,rbi,sb,avg,ip,w,era,k,sv,er,bb,whip),~mean(., na.rm = TRUE))) %>%
    ungroup()
    
  return(rep)
}

experimental.values <- function(league) {
  
  if(league == 'DC') {
    lrfilter = 3
    teams = 15
  } else if(league == 'OC') {
    lrfilter = 2
    teams = 12
  }
  
  slots = 23
  total = 30
  seasonweeks = 25
  c = 2
  hits = 14
  pits = 9
  scal.hit = (hits - 1)/hits
  scal.pit = (pits - 1)/pits
  ll = tolower(league)
  base = get(paste0('base.',ll))
  adp.data <- get(paste0('adp.',ll))
  
  # Create positions slots
  # Hitters
  if(league == 'OC') {
    cuts <- tibble(
      pos = c('C','1B','2B','3B','SS','OF','P')
      ,set = c(NA,'CI','MI','CI','MI',NA,NA)
      ,slots = c(2,1,1,1,1,5,9)
      ,total.weeks = seasonweeks * slots * teams
    )
  } else if (league == 'DC') {
    cuts <- tibble(
      pos = c('C','1B','2B','3B','SS','OF','P')
      ,set = c(NA,'CI','MI','CI','MI',NA,NA)
      ,slots = c(2,1,1,1,1,5,9)
      ,total.weeks = seasonweeks * slots * teams
    )    
  }
  
  # Find the prior above replacement players
  prior <- base
  
  # Calculate AB and IP estimates
  est <- nfbc.standings %>%
    filter(league.type == !!league & league.rank <= lrfilter) %>%
    summarise(across(c(ab,ip),mean,.names = '{.col}.est'))
  ab.est = est$ab.est[1]
  ip.est = est$ip.est[1]
  
  test = 0
  
  while(test == 0){
    
    # Find average of available players
    hit.mid <- hit.nfbc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement == 1)) %>%
      filter(above.replacement) %>%
      summarise(across(ab:sb,~mean(., na.rm = TRUE),.names = 'mid.{.col}')
                , across(c(ab,h),~sum(., na.rm = TRUE))) %>%
      mutate(across(starts_with('mid.'),~.*hits,.names = 'sum{.col}')) %>%
      cross_join(get(paste0('coef.hit.',league))) %>%
      mutate(mid.avg = h/ab
             , .after = mid.sb) %>%
      mutate(summid.h = `0.5.avg` * ab.est
             , summid.ab = ab.est
             , summid.avg = `0.5.avg`) %>%
      select(contains('mid.'))
    
    hit.repl.stats1 <-  hit.nfbc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement)) %>%
      filter(!above.replacement & !is.na(avg)) %>%
      group_by(pos) %>%
      arrange(rank) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= teams) %>%
      summarise(across(ab:sb,~mean(., na.rm = TRUE),.names = 'repl.{.col}')) %>%
      filter(pos != 'UT')
    
    hit.repl.stats <- hit.repl.stats1 %>%
      summarise(across(where(is.numeric),max)) %>%
      mutate(pos = 'UT') %>%
      bind_rows(hit.repl.stats1)
    
    pitch.mid <- pitch.120.nfbc %>%
      left_join(base, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,TRUE)) %>%
      filter(above.replacement) %>%
      summarise(across(c(ip,er,h,bb,w,sv,k),~mean(., na.rm = TRUE), .names = 'mid.{.col}')
                , across(c(ip,er,h,bb),~sum(., na.rm = TRUE))) %>%
      mutate(across(starts_with('mid.'),~.*pits,.names = 'sum{.col}')) %>%
      cross_join(get(paste0('coef.pitch.',league))) %>%
      mutate(mid.era = er*9/ip,
             mid.whip = (h+bb)/ip
             , .after = mid.k) %>%
      mutate(summid.er = (`0.5.era` * ip.est)/9
             , summid.h = (`0.5.whip` * ip.est) * (h/(h+bb))
             , summid.bb = (`0.5.whip` * ip.est) * (bb/(h+bb))
             , summid.ip = ip.est
             , summid.era = `0.5.era`
             , summid.whip = `0.5.whip`) %>%
      select(contains('mid.'))
    
    pitch.repl.stats <-  pitch.nfbc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement)) %>%
      filter(!above.replacement & !is.na(era)) %>%
      arrange(rank) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= teams*2) %>%
      summarise(across(c(ip,er,h,bb,w,sv,k),mean, .names = 'repl.{.col}'))
    
    # Create sgp for hitters
    # Add on the coef data
    hit.df <- hit.450.nfbc %>%
      cross_join(hit.mid) %>%
      cross_join(get(paste0('coef.hit.',league))) %>%
      left_join(hit.repl.stats, by = 'pos')
    
    if(league == 'OC') {
      
      map(hitlist.count,function(catg) {
        
        hit.df <<- hit.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)
                                     + get(paste0('0.5.',catg))
                                     - get(paste0('mid.',catg))
                                     + get(paste0('repl.',catg)) * (seasonweeks - weeks) / seasonweeks
        )
        
      })      
      
      ## AVG
      hit.df <- hit.df %>% mutate(v1.avg = (h + summid.h - mid.h + repl.h * (seasonweeks - weeks)/seasonweeks)
                                  / (ab + summid.ab - mid.ab + repl.ab * (seasonweeks - weeks)/seasonweeks)
      )
      
    } else if(league == 'DC') {
      
      map(hitlist.count,function(catg) {
        
        hit.df <<- hit.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)
                                     + get(paste0('0.5.',catg))
                                     - get(paste0('mid.',catg))
        )
        
      })      
      
      ## AVG
      hit.df <- hit.df %>% mutate(v1.avg = (h + summid.h - mid.h)
                                  / (ab + summid.ab - mid.ab)
      )
    }
    
    
    # Run probs
    hit.prob <- function(catg) {
      
      hit.df <<- hit.df %>% mutate(!!as.name(paste0('spg.',catg)) :=
                                     (# Calculate e^(b0 + b1*x1)
                                       (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))
                                        # Divide by 1 + e^(b0 + b1*x1)
                                        / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))))
                                       # Caclulate the probablities of the average team
                                       - (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg)))
                                          / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg))))))
                                   # Multiply by teams
                                   * (teams - 1) + 1
      )
      
    }
    map(hit.cats,hit.prob)
    
    # Sum up the spgs
    hit.spg <- hit.df %>%
      rowwise() %>%
      mutate(spg = sum(c_across(starts_with('spg')))) %>%
      ungroup() %>%
      select(fg.id, pos, weeks, spg) %>%
      left_join(select(cuts,pos,set), by = 'pos')
    
    # Find replacement values
    max.test = 0
    while(max.test != 1) {
      
      # Find replacement values for single positions
      hit.rv.players <- hit.spg %>%
        left_join(select(cuts,-set), by = 'pos') %>%
        arrange(pos,-spg) %>%
        group_by(pos) %>%
        mutate(sum.weeks = cumsum(weeks)
               ,diff = sum.weeks - total.weeks) %>%
        filter(diff >= 0)
      
      hit.rv1 <- hit.rv.players %>%
        mutate(rank = row_number()) %>%
        filter(between(rank,2,ceiling(teams/2) + 1)) %>%
        summarise(rv.spg = mean(spg, na.rm = TRUE)) %>%
        ungroup()
      
      # Find replacement values for CI/MI
      hit.rv.players.sub <- hit.rv.players %>%
        ungroup() %>%
        filter(set %in% c('CI','MI')) %>%
        select(!(slots:diff)) %>%
        left_join(filter(cuts,set %in% c('CI','MI')), by = join_by(pos,set)) %>%
        select(-pos) %>%
        distinct() %>%
        arrange(set,-spg) %>%
        group_by(set) %>%
        mutate(sum.weeks = cumsum(weeks)
               ,diff = sum.weeks - total.weeks) %>%
        filter(diff >= 0)
      
      hit.rv.sub <- hit.rv.players.sub %>%
        mutate(rank = row_number()) %>%
        filter(between(rank,2,ceiling(teams/2) + 1)) %>%
        summarise(rv.spg.sub = mean(spg, na.rm = TRUE)) %>%
        ungroup()
      
      # Find replacement values for UTIL
      hit.rv.playersUTIL <- hit.rv.players %>%
        ungroup() %>%
        filter((set != 'MI' & set != 'CI') | is.na(set)) %>%
        bind_rows(hit.rv.players.sub) %>%
        select(!(slots:diff)) %>%
        distinct() %>%
        mutate(total.weeks = weeks * teams) %>%
        arrange(-spg) %>%
        mutate(sum.weeks = cumsum(weeks)
               , diff = sum.weeks - total.weeks) %>%
        filter(diff >= 0)
      
      hit.rvUTIL <- hit.rv.playersUTIL %>%
        mutate(rank = row_number()) %>%
        filter(between(rank,2,ceiling(teams/2) + 1)) %>%
        summarise(rv.spg.util = mean(spg, na.rm = TRUE)) %>%
        ungroup()
      
      # Combine the RVs to get PAR
      hitter.chart <- hit.spg %>%
        left_join(hit.rv1, by = 'pos') %>%
        left_join(hit.rv.sub, by = 'set') %>%
        cross_join(hit.rvUTIL) %>%
        rowwise() %>%
        mutate(rv = min(across(starts_with('rv.spg')), na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(par = spg - rv) %>%
        group_by(fg.id) %>%
        arrange(-par,rv.spg) %>%
        mutate(rank = row_number()
               , player.pos.count = n()) %>%
        filter(rank == 1 | rank < player.pos.count) %>%
        mutate(player.count = n()) %>%
        ungroup()
      
      max.test <- max(hitter.chart$player.count)
      print(max.test)
      if (max.test != 1) {
        print('repeat')
        hit.spg <- select(hitter.chart,fg.id,set,pos,weeks,spg)
      } else {
        print('end')
        hitter.chart <<- hitter.chart %>%
          select(fg.id,pos,weeks,spg,par,rv)
        # %>%
        #   rename(best.pos = pos)
      }
      
    }
    
    # PITCHING
    pitch.df <- pitch.120.nfbc %>%
      cross_join(pitch.mid) %>%
      cross_join(get(paste0('coef.pitch.',league))) %>%
      cross_join(pitch.repl.stats)
    
    div.pit <- function(catg) {
      
      pitch.df <<- pitch.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)
                                       + get(paste0('0.5.',catg))
                                       - get(paste0('mid.',catg))
                                       + get(paste0('repl.',catg)) * (seasonweeks - weeks)/seasonweeks
      )
      
    }
    
    map(pitlist.count,div.pit)
    
    ## ERA
    pitch.df <- pitch.df %>% mutate(v1.era = (er + summid.er - mid.er + repl.er * (seasonweeks - weeks)/seasonweeks)*9
                                    / (ip + summid.ip - mid.ip + repl.ip * (seasonweeks - weeks)/seasonweeks)
    )
    
    ## WHIP
    pitch.df <- pitch.df %>% mutate(v1.whip = (h + bb + summid.h + summid.bb - (mid.h + mid.bb) + (repl.h + repl.bb)*((seasonweeks - weeks)/seasonweeks))
                                    / (ip + summid.ip - mid.ip + repl.ip * (seasonweeks - weeks)/seasonweeks)
    )
    
    # Run probs
    pitch.prob <- function(catg) {
      
      pitch.df <<- pitch.df %>% mutate(!!as.name(paste0('spg.',catg)) :=
                                         (# Calculate e^(b0 + b1*x1)
                                           (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))
                                            # Divide by 1 + e^(b0 + b1*x1)
                                            / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))))
                                           # Caclulate the probablities of the average team
                                           - (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg)))
                                              / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg))))))
                                       # Multiply by teams
                                       * (teams - 1) + 1
      )
      
    }
    map(pitch.cats,pitch.prob)
    
    pitch.spg <- pitch.df %>%
      rowwise() %>%
      mutate(spg = sum(c_across(starts_with('spg')))) %>%
      ungroup() %>%
      select(fg.id, weeks, spg) %>%
      mutate(pos = 'P') %>%
      left_join(select(cuts,pos,set), by = 'pos')
    
    # Find replacement values for single positions
    pitch.rv.players <- pitch.spg %>%
      left_join(select(cuts,-set), by = 'pos') %>%
      arrange(pos,-spg) %>%
      group_by(pos) %>%
      mutate(sum.weeks = cumsum(weeks)
             ,diff = sum.weeks - total.weeks) %>%
      filter(diff >= 0)
    
    pitch.rv <- pitch.rv.players %>%
      mutate(rank = row_number()) %>%
      filter(between(rank,2,ceiling(teams/2) + 1)) %>%
      summarise(rv.spg = mean(spg)) %>%
      ungroup()
    
    # Combine the RVs to get PAR
    pitcher.chart <- pitch.spg %>%
      left_join(pitch.rv, by = 'pos') %>%
      rowwise() %>%
      mutate(rv = min(across(starts_with('rv.spg')), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(par = spg - rv)  %>%
      select(fg.id,pos,weeks,spg,par,rv)
    
    # Find the list of players abovereplacement
    prior.new <- bind_rows(
      hitter.chart
      , pitcher.chart
    ) %>%
      arrange(desc(par)) %>%
      mutate(rank = row_number()
             , above.replacement = par > 0) %>%
      distinct(fg.id, rank, above.replacement) %>%
      filter(!is.na(above.replacement))
    
    if(isTRUE(all.equal(prior %>% distinct(fg.id) %>% arrange(fg.id), 
                        prior.new %>% distinct(fg.id) %>% arrange(fg.id), convert = TRUE))) {
      test = 1
      print(hitter.chart)
      print(pitcher.chart)
      
      # Combine
      combine <- bind_rows(
        select(hitter.chart, fg.id, pos, weeks, spg, par, rv) %>% mutate(pos = if_else(is.na(pos),'UT',pos))
        , pitcher.chart %>% mutate(pos = if_else(is.na(pos),'P',pos))
      ) 
      
      unique.players <- combine %>%
        group_by(fg.id) %>%
        filter(par == max(par)) %>%
        distinct(fg.id, pos)
      
      output.values <- combine %>%
        inner_join(unique.players, by = c('fg.id', 'pos')) %>%
        rename(best.pos = pos) %>%
        left_join(nfbc.pos.wide, by = 'fg.id') %>%
        mutate(pos = if_else(best.pos == 'P','P',pos)) %>%
        left_join(select(adp.data,-c(player,pos)), by = 'fg.id') %>%
        left_join(bind_rows(hit.proj, pitch.proj) %>% distinct(fg.id,name), by = 'fg.id') %>%
        select(fg.id, name, team, pos, best.pos, weeks, spg, rv, par, adp, min.pick, max.pick) %>%
        rename(PlayerID = fg.id
               , Name = name
               , Team = team
               , Position = pos
               , Best = best.pos
               , winsadded = spg
               , war = par
               , ADP = adp
               , `Min Pick` = min.pick
               , `Max Pick` = max.pick
        ) %>%
        arrange(desc(war))
      
    } else {
      prior <- prior.new %>%
        group_by(fg.id) %>%
        summarise(rank = min(rank)
                  , above.replacement = max(above.replacement)
                  , .groups = 'drop')
    }
    
  }
  
  return(output.values)
}