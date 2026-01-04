
# Set parameters
league = 'HC'

hc.values <- function(league) {
  
  teams = 12
  
  slots = 21
  total = 24
  seasonweeks = 25
  c = 1
  hits = 11
  pits = 12
  scal.hit = (hits - 1)/hits
  scal.pit = (pits - 1)/pits
  ll = tolower(league)
  adp.data <- get(paste0('adp.',ll))
  
  # Create positions slots
  # Hitters
  cuts <- tibble(
    pos = c('C','1B','2B','3B','SS','OF','SP','RP')
    ,set = c(NA,'INF','INF','INF','INF',NA,NA,NA)
    ,slots = c(1,1,1,1,1,4,9,3)
    ,total.weeks = seasonweeks * slots * teams
  )
  
  # Find the prior above replacement players
  prior <- base
  
  test = 0
  
  while(test == 0){
    
    # Find average of available players
    hit.mid <- hit.hc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement)) %>%
      filter(above.replacement) %>%
      summarise(across(starts_with('week.'),~mean(., na.rm = TRUE),.names = 'mid.{.col}')
                , across(c(week.ab,week.h,week.x1b,week.x2b,week.x3b,week.hr,week.sf,week.hbp,week.bb),~sum(., na.rm = TRUE))) %>%
      rename_with(~str_replace(.,'mid.week.','mid.')) %>%
      mutate(across(starts_with('mid.'),~.*hits,.names = 'sum{.col}')) %>%
      cross_join(get(paste0('coef.hit.',ll))) %>%
      mutate(mid.avg = week.h/week.ab
             , mid.ops =  
               # OBP
               (week.h + week.bb + week.hbp)/(week.ab + week.bb + week.hbp + week.sf) +
               # SLG
               (week.x1b + 2*week.x2b + 3*week.x3b + 4*week.hr) / week.ab
             , .after = mid.sb) %>%
      mutate(summid.h = `0.5.avg` * ab.est
             , summid.ab = ab.est
             , summid.avg = `0.5.avg`
             , summid.ops = `0.5.ops`) %>%
      mutate(avg.scale = summid.avg / mid.avg
             , mid.h = mid.h * avg.scale
             , ops.scale = summid.ops / mid.ops
            , across(c(mid.x1b,mid.x2b,mid.x3b,mid.hr,mid.hbp,mid.bb),~. * ops.scale)) %>%
      select(contains('mid.'))
    
    hit.repl.stats1 <-  hit.hc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement)) %>%
      filter(!above.replacement & !is.na(avg)) %>%
      group_by(pos) %>%
      arrange(rank) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= teams) %>%
      summarise(across(ab:sb,~mean(./weeks, na.rm = TRUE),.names = 'repl.{.col}')) %>%
      filter(pos != 'UT')
    
    hit.repl.stats <- hit.repl.stats1 %>%
      summarise(across(where(is.numeric),max)) %>%
      mutate(pos = 'UT') %>%
      bind_rows(hit.repl.stats1)
    
    pitch.mid <- pitch.hc %>%
      left_join(base, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,TRUE)) %>%
      filter(above.replacement) %>%
      summarise(across(starts_with('week.'),mean, .names = 'mid.{.col}')
                , across(c(week.ip,week.er,week.h,week.bb),~sum(., na.rm = TRUE))) %>%
      mutate(across(starts_with('mid.'),~.*pits,.names = 'sum{.col}')) %>%
      rename_with(~str_replace(.,'mid.week.','mid.')) %>%
      cross_join(get(paste0('coef.pitch.',ll))) %>%
      mutate(mid.era = mid.er*9/mid.ip
             , mid.hip = mid.h/mid.ip
             , mid.bb9 = mid.bb*9/mid.ip
             , mid.k = `0.5.k` * (1-scal.pit)
             , mid.qs = `0.5.qs` * (1-scal.pit)
             , mid.ip = ip.est * (1-scal.pit)
             , mid.er = (`0.5.era` * mid.ip)/9
             , mid.h = `0.5.hip` * mid.ip
             , mid.bb = (`0.5.bb9` * mid.ip)/9
             , .before = summid.ip) %>%
      mutate(summid.er = (`0.5.era` * ip.est)/9
             , summid.h = (`0.5.hip` * ip.est) 
             , summid.bb = (`0.5.bb9` * ip.est)/9
             , summid.ip = ip.est
             , summid.era = `0.5.era`
             , summid.hip = `0.5.hip`
             , summid.bb9 = `0.5.bb9`) %>%
      select(contains('mid.'))
    
    era.scale = pitch.mid$summid.era / pitch.mid$mid.era
    hip.scale = pitch.mid$summid.hip / pitch.mid$mid.hip
    bb9.scale = pitch.mid$summid.bb9 / pitch.mid$mid.bb9
    
    pitch.repl.stats <-  pitch.hc %>%
      left_join(prior, by = 'fg.id') %>%
      mutate(above.replacement = if_else(is.na(above.replacement),FALSE,above.replacement)) %>%
      filter(!above.replacement & !is.na(era)) %>%
      group_by(pos) %>%
      arrange(rank) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= teams*2) %>%
      summarise(across(c(ip,er,h,bb,qs,svhld,k),~mean(./weeks, na.rm = TRUE), .names = 'repl.{.col}'))
    
    # Create sgp for hitters
    # Add on the coef data
    hit.df <- hit.hc %>%
      cross_join(hit.mid) %>%
      cross_join(get(paste0('coef.hit.',ll))) %>%
      left_join(hit.repl.stats, by = 'pos')
    
    div.bat <- function(catg) {
      
      hit.df <<- hit.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(paste0('week.',catg))
                                   + get(paste0('0.5.',catg))
                                   - get(paste0('mid.',catg))
      )
      
    }
    
    map(hitlist.count,div.bat)
    
    ## AVG
    hit.df <- hit.df %>% mutate(v1.avg = (week.h + summid.h - mid.h)
                                / (week.ab + summid.ab - mid.ab)
                                # OPS
                                , v1.ops =
                                  # OBP
                                  (((week.h + summid.h - mid.h) + 
                                  (week.bb + summid.bb - mid.bb) +
                                  (week.hbp + summid.hbp - mid.hbp))
                                / ((week.ab + summid.ab - mid.ab) +
                                   (week.bb + summid.bb - mid.bb) +
                                   (week.hbp + summid.hbp - mid.hbp) +
                                   (week.sf + summid.sf - mid.sf))
                                +
                                  # SLG
                                  ((x1b + summid.x1b - mid.x1b) + 
                                     2*(x2b + summid.x2b - mid.x2b) +
                                     3*(x3b + summid.x3b - mid.x3b) +
                                     4*(hr + summid.hr - mid.hr))
                                  / (ab + summid.ab - mid.ab))
                                )
    
    # Run probs
    hit.prob <- function(catg) {
      
      hit.df <<- hit.df %>% mutate(!!as.name(paste0('spg.',catg)) :=
                                     (# Calculate e^(b0 + b1*x1)
                                       (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))
                                        # Divide by 1 + e^(b0 + b1*x1)
                                        / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('v1.',catg)))))
                                       # Calculate the probabilities of the average team
                                       - (exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg)))
                                          / (1 + exp(get(paste0('b0.',catg)) + get(paste0('b1.',catg)) * get(paste0('0.5.',catg))))))
                                   # Multiply by weeks
                                   * weeks
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
        filter(set == 'INF') %>%
        select(!(slots:diff)) %>%
        left_join(filter(cuts,set == 'INF'), by = join_by(pos,set)) %>%
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
        filter((set != 'INF') | is.na(set)) %>%
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
        mutate(par = spg - rv + rv * (seasonweeks - weeks)/seasonweeks) %>%
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
          select(fg.id,pos,weeks,spg,par,rv) %>%
          rename(best.pos = pos)
      }
      
    }
    
    
    # PITCHING
    pitch.df <- pitch.hc %>%
      left_join(cuts, by = 'pos') %>%
      cross_join(pitch.mid) %>%
      cross_join(get(paste0('coef.pitch.',ll))) %>%
      left_join(pitch.repl.stats, by = 'pos')
    
    div.pit <- function(catg) {
    
      pitch.df <<- pitch.df %>% mutate(!!as.name(paste0('v1.',catg)) := get(paste0('week.',catg))
                                       + get(paste0('0.5.',catg))
                                       - get(paste0('mid.',catg))
      )
    
    }
    
    map(pitlist.count,div.pit)
    
    ## ERA
    pitch.df <- pitch.df %>% mutate(v1.era = (week.er + summid.er - mid.er)*9*era.scale
                                    / (week.ip + summid.ip - mid.ip)
                                    
                                    , v1.hip = (week.h + summid.h - mid.h)*hip.scale
                                    / (week.ip + summid.ip - mid.ip)
                                    
                                    , v1.bb9 = (week.bb + summid.bb - mid.bb)*9*bb9.scale
                                    / (week.ip + summid.ip - mid.ip)
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
                                   * weeks
      )
      
    }
    map(pitch.cats,pitch.prob)
    
    pitch.spg <- pitch.df %>%
      rowwise() %>%
      mutate(spg = sum(c_across(starts_with('spg')))) %>%
      ungroup() %>%
      select(fg.id, pos, weeks, spg) %>%
      left_join(select(cuts,pos), by = 'pos')
    
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
      mutate(par = spg - rv + rv * (seasonweeks - weeks)/seasonweeks) %>%
      group_by(fg.id) %>%
      arrange(-par,rv.spg) %>%
      mutate(rank = row_number()
             , player.pos.count = n()) %>%
      filter(rank == 1 | rank < player.pos.count) %>%
      mutate(player.count = n()) %>%
      ungroup() %>%
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
      
      output.values <- bind_rows(
        select(hitter.chart, fg.id, pos, weeks, spg, par, rv) %>% mutate(pos = if_else(is.na(pos),'UT',pos))
        , pitcher.chart %>% mutate(pos = if_else(is.na(pos),'P',pos))
      ) %>%
        rename(best.pos = pos) %>%
        left_join(adp.hc, by = 'fg.id') %>%
        mutate(pos = if_else(best.pos == 'P','P',pos)) %>%
        left_join(fangraphs, by = join_by('fg.id' == 'fangraphs.id')) %>%
        select(fg.id, name, team, pos, best.pos, weeks, spg, rv, par, adp) %>%
        rename(PlayerID = fg.id
               , Name = name
               , Team = team
               , Position = pos
               , Best = best.pos
               , winsadded = spg
               , war = par
               , ADP = adp
               ) %>%
        arrange(desc(war))
      
      test.shout <- output.values %>%
        filter(war > 0) %>%
        mutate(pitcher = str_detect(Best,'P')) %>%
        group_by(pitcher) %>%
        summarise(war = sum(war)) %>%
        ungroup()
      
      print(test.shout)
      
    } else {
      prior <- prior.new
    }
  
  }
  
  return(output.values)
}

replacement.stats <- function(o, league) {
  
  teams = 12
  
  slots = 21
  total = 24
  seasonweeks = 25
  c = 1
  hits = 11
  pits = 12
  scal.hit = (hits - 1)/hits
  scal.pit = (pits - 1)/pits
  ll = tolower(league)
  adp.data <- get(paste0('adp.',ll))
  
  # Create positions slots
  # Hitters
  cuts <- tibble(
    pos = c('C','1B','2B','3B','SS','OF','SP','RP')
    ,set = c(NA,'INF','INF','INF','INF',NA,NA,NA)
    ,slots = c(1,1,1,1,1,4,7,3)
    ,total.weeks = seasonweeks * slots * teams
  )
  
  # Limit to replacement level players
  rep <- o %>%
    filter(war < 0) %>%
    mutate(hitter.flag = str_detect(Best,'P')) %>%
    left_join(hit.hc, by = join_by(PlayerID == fg.id)) %>%
    left_join(pitch.hc, by = join_by(PlayerID == fg.id)) %>%
    mutate(h = coalesce(h.x,h.y)
           , bb = coalesce(bb.x,bb.y)) %>%
    mutate(tb = `x1b` + 2*`x2b` + 3*`x3b` + 4* hr
           , .after = hbp) %>%
    group_by(Best) %>%
    arrange(desc(war)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= teams * 2) %>%
    summarise(across(c(ab,h,hr,r,rbi,sb,avg,ops,tb,hbp,sf,ip,qs,era,k,svhld,er,bb,hip,bb9),~mean(., na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(Best = if_else(Best == 'UT','Hit',Best))
    
  return(rep)
}
