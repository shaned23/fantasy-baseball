#### Ways to improve next year
# Iterate over it where each successive loop uses the top 240 from the last loop
# Positional replacement value
# Adjust all the categories before logit model to be proportional to the top 240 averages,
# then adjust the spreadsheet to use average remaining rather than the overall average


# Set parameters
teams = 12
weeks = 25
slots = 24
scal = 10/11
hits = 12
sp.slots = 7
rp.slots = 3



# Calculate shane volumes of ABs and IPs
shanevol <- left_join(shanevol,select(df,team,scoringperiod,daysinperiod),
                             by = c('scoringperiod' = 'scoringperiod','Team' = 'team'))

volavg <- as.list(shanevol %>% filter(daysinperiod == 7) %>%
  summarise(meanab = mean(ab),meanip = mean(ip)))

ab.est <- volavg$meanab[1]
ip.est <- volavg$meanip[1]

# Create hitters df
hitdf <- batxhit %>% select(PlayerId,PA,AB,H,HR,R,RBI,BB,SB,AVG,OBP,SLG,OPS,
                            ISO,X1B,X2B,X3B,HR,BB.,ADP, SF, HBP)
hitdf <- hitdf %>%
  left_join(.,
            select(batxvalues,PlayerId,Dollars)) %>%
    rename_with(tolower) 


alpha <- hitdf %>% filter(dollars > 0)

alpha.summ <- alpha %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  select(pa:hbp)


divide.weeks <- function(x){
  x <<- x / weeks
}
alpha.mean <- alpha %>% 
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  select(pa:hbp) %>%
  mutate(across(where(is.numeric) , divide.weeks))

# Create input values for counting stats
hitlist.count <- c('r','hr','rbi','sb')

r.a <- alpha.mean$r[1]
hr.a <- alpha.mean$hr[1]
rbi.a <- alpha.mean$rbi[1]
sb.a <- alpha.mean$sb[1]

div <- function(catg) {
  
  hitdf <<- hitdf %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)/weeks
                             + get(paste0(catg,'.5'))
                             - get(paste0(catg,'.a')))
  
}

lapply(hitlist.count,div)

# Create input values for rate stats
temphit <- hitdf %>% select(playerid, pa, ab, h,x1b,x2b,x3b,hr,bb,hbp,sf)

avglist <- list('ab','h','pa','bb','hbp','sf','x1b','x2b','x3b','hr')
avgdiv <- function(catg) {
  
  temphit <<- temphit %>% mutate(!!as.name(paste0('week.',catg)) := get(catg)/weeks)
  
}
map(avglist, avgdiv)

##AVG
ab.est <- as.numeric(volavg[1])
h.5 <- avg.5 * ab.est
temphit <- temphit %>% mutate(v1.avg = (h.5 * scal + week.h)
                              / (ab.est * scal + week.ab))

##OPS
obplist <- c('bb','hbp','sf')
slglist <- c('x1b','x2b','x3b','hr')

alpha.summ <- alpha.summ %>% mutate(star.pa = ((pa / ab) * ab.est))
obpcreate <- function(catg) {
  alpha.summ <<- alpha.summ %>% mutate(!!as.name(paste0('star.',catg)) := ((get(catg) / pa) * star.pa))
}
map(obplist,obpcreate)


slgcreate <- function(catg) {
  alpha.summ <<- alpha.summ %>% mutate(!!as.name(paste0('star.',catg)) := ((get(catg) / ab) * ab.est))
}
map(slglist,slgcreate)

alpha.summ <- alpha.summ %>% mutate(star.tb = star.x1b + 2 * star.x2b + 3 * star.x3b + 4 * star.hr)

alpha.merge <- alpha.summ %>% select(starts_with('star.'))

temphit <- bind_cols(temphit,alpha.merge)

temphit <- temphit %>% mutate(week.tb = week.x1b + 2 * week.x2b + 3 * week.x3b + 4 * week.hr)
temphit <- temphit %>% mutate(v1.ops = 
                                ((((h.5 + star.bb + star.hbp) * scal) + (week.h + week.bb + week.hbp)) /
                                (((ab.est + star.bb + star.hbp + star.sf) * scal) + (week.ab + week.bb + week.hbp + week.sf))) +
                                (((star.tb * scal) + (week.tb)) /
                                   ((ab.est * scal) + week.ab)))

alpha.merge <- alpha.merge %>% mutate(ops.multiplier = ((h.5 + star.bb + star.hbp) / (ab.est + star.bb + star.hbp + star.sf)) +
                                            (star.tb / ab.est))

# Merge back on
hit.merge <- temphit %>% select(playerid, v1.avg, v1.ops)
hitdf <- left_join(hitdf,hit.merge)

######## Dataset done #######

# Create player id with v1 dataset
hitregdf <- hitdf %>% select(playerid,starts_with('v1.'))
hitregdf <- hitregdf %>% rename_with(~gsub('v1.','',.),starts_with('v1.'))

regsave <- function(catg){
  
  assign(paste0('reg.',catg), 
         glm(get(paste0('w.',catg)) ~ get(catg), data = filter(df7,get(catg)!=99999),
             family = "quasibinomial"),
          envir = .GlobalEnv)
  
  hitregdf <<- hitregdf %>% 
    mutate(!!as.name(paste0('prob.',catg)) := 
             (predict(get(paste0('reg.',catg)), newdata = hitregdf , 'response') - 0.5) * weeks)
  
}
hitcats <-  c('r','hr','rbi','sb','avg','ops')
lapply(hitcats,regsave)

hitregdf <- hitregdf %>%  mutate(winsadded = rowSums(across(starts_with('prob.'))))

hitout <- left_join(select(batxhit,Name,Team,G,PA,PlayerId,R,HR,RBI,SB,AVG,OPS),
                    select(hitregdf,starts_with('prob.'),winsadded,playerid),
                      by = c('PlayerId' = 'playerid')) %>%
            arrange(desc(winsadded))
################

## Pitchers
pitlist.count <- c('k','qs','svhld')
ip.est <- as.numeric(volavg[2])

# Create pitchers df
pitdf <- atcpit %>% select(PlayerId,QS,ERA,G,GS,SV,HLD,IP,TBF,H,ER,BB,SO,BB.9,AVG,ADP) %>%
  filter(!(PlayerId %in% c('14710','27473')))

pitdf <- pitdf %>% mutate(svhld = SV*1.5 + HLD)

pitdf <- pitdf %>%
  left_join(.,
            select(atcvalues,PlayerId,Dollars)) %>%
  rename_with(tolower) %>%
  filter(!(playerid %in% c('14710','27473')))

pitdf <- pitdf %>% rename('bb9' = 'bb.9',
                          'k' = 'so')

alpha.pit <- pitdf %>%
  filter(dollars > 0)

alpha.pit <- alpha.pit %>% mutate(hip = h / ip)

alpha.pit.summ <- alpha.pit %>% 
  summarize_if(is.numeric, sum, na.rm = TRUE) %>%
  select(qs:svhld)

alpha.pit.mean <- alpha.pit %>% 
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  select(qs:svhld) %>%
  mutate(across(where(is.numeric) , divide.weeks))


# Create input values for counting stats
k.a <- alpha.pit.mean$k[1]
qs.a <- alpha.pit.mean$qs[1]
svhld.a <- alpha.pit.mean$svhld[1]

div.pit <- function(catg) {
  
  pitdf <<- pitdf %>% mutate(!!as.name(paste0('v1.',catg)) := get(catg)/weeks
                             + get(paste0(catg,'.5'))
                             - get(paste0(catg,'.a')))
  
}

map(pitlist.count,div.pit)

# Create input values for rate stats
temppit <- pitdf %>% select(playerid, er, bb, h, ip)

ratelist <- list('er','bb','h','ip')
ratediv <- function(catg) {
  
  temppit <<- temppit %>% mutate(!!as.name(paste0('week.',catg)) := get(catg)/weeks)
  
}
lapply(ratelist, ratediv)

hp.5 <- hip.5 * ip.est
bb.5 <- (bb9.5 * ip.est) / 9
er.5 <- (era.5 * ip.est) / 9

#HIP
temppit <- temppit %>% mutate(v1.hip = (hp.5 * scal + week.h)
                              / (ip.est * scal + week.ip))

#BB9
temppit <- temppit %>% mutate(v1.bb9 = ((bb.5 * scal + week.bb)
                              / (ip.est * scal + week.ip))*9)

#ERA
temppit <- temppit %>% mutate(v1.era = ((er.5 * scal + week.er)
                              / (ip.est * scal + week.ip))*9)

# Merge back on
pit.merge <- temppit %>% select(playerid, v1.hip, v1.bb9, v1.era)
pitdf <- left_join(pitdf,pit.merge)

##################
# Create player id with v1 dataset
pitregdf <- pitdf %>% select(playerid,starts_with('v1.'))
pitregdf <- pitregdf %>% rename_with(~gsub('v1.','',.),starts_with('v1.'))

regsave.pit <- function(catg){
  
  assign(paste0('reg.',catg), 
         glm(get(paste0('w.',catg)) ~ get(catg), data = filter(df7,get(catg)!=99999),
             family = "quasibinomial"),
         envir = .GlobalEnv)
  
  pitregdf <<- pitregdf %>% 
    mutate(!!as.name(paste0('prob.',catg)) := 
             (predict(get(paste0('reg.',catg)), newdata = pitregdf , 'response') - 0.5) * weeks)
  
}
pitcats <-  c('k','qs','svhld','hip','bb9','era')
lapply(pitcats,regsave.pit)

pitregdf <- pitregdf %>%  mutate(winsadded = rowSums(across(starts_with('prob.'))))

pitout <- left_join(select(atcpit,Name,Team,IP,G,GS,PlayerId,QS,SO,SV,HLD,H,BB.9,ERA),
                    select(pitregdf,starts_with('prob.'),winsadded,playerid),
                    by = c('PlayerId' = 'playerid')) %>%
              arrange(desc(winsadded))
pitout <- pitout %>% rename('K' = 'SO')



all <- union(select(hitout,PlayerId,Name,Team,winsadded),select(pitout,PlayerId,Name,Team,winsadded)) %>%
  group_by(PlayerId,Name,Team) %>%
    summarise(winsadded = sum(winsadded)) %>%
  arrange(desc(winsadded))

############
## Add in replacement values

# Create smaller tables
hitval.skinny <- batxvalues %>% select(PlayerId,POS) %>%
                rename('hitter.pos' = 'POS')
pitval.skinny <- atcvalues %>% select(PlayerId,POS) %>%
  rename('pitcher.pos' = 'POS')

# Join them
positions <- full_join(hitval.skinny,pitval.skinny)

# Recreate position
positions <- positions %>% mutate(position =
                                    if_else(is.na(hitter.pos)==TRUE,pitcher.pos,hitter.pos))

pos.list <- c('C','1B','2B','3B','SS','OF','SP','RP')
pos.fun <- function(pos) {
  
  if (str_detect({{pos}},'P')) {
    positions <<- positions %>% mutate(!!paste0('pos.',pos) := replace_na(str_detect(pitcher.pos,{{pos}}), FALSE))
  } else {
    positions <<- positions %>% mutate(!!paste0('pos.',pos) := replace_na(str_detect(hitter.pos,{{pos}}), FALSE))
  }

}
map(pos.list,pos.fun)

# Create main position
positions <- positions %>% mutate(hitpos.first = case_when(pos.C == TRUE ~ 'C'
                                                          ,pos.3B == TRUE ~ '3B'
                                                          ,pos.OF == TRUE ~ 'OF'      
                                                          ,pos.2B == TRUE ~ '2B'
                                                          ,pos.SS == TRUE ~ 'SS'
                                                          ,pos.1B == TRUE ~ '1B'
                                                          , TRUE ~ 'UTIL'))
positions <- positions %>% mutate(pitpos.first = case_when(pos.RP == TRUE ~ 'RP'
                                                           ,pos.SP == TRUE ~ 'SP'
                                                           , TRUE ~ 'RP'))

# Join on positions into hitout
hit.repl <- left_join(hitout,
                      select(filter(positions,is.na(hitter.pos) == FALSE),
                             PlayerId,hitpos.first)
                      ) %>% rename('pos' = 'hitpos.first')

# Find catcher replacement value
c.repl <- hit.repl %>% filter(pos == 'C')
rv.c <- c.repl %>% mutate(rank.pos = row_number()) %>%
            filter(rank.pos == teams + 1)
rv.c <- rv.c$winsadded[1]

# Find all other replacement value
h.repl <- hit.repl %>% filter(pos != 'C')
rv.h <- h.repl %>% mutate(rank.pos = row_number()) %>%
  filter(rank.pos == teams*hits + 1)
rv.h <- rv.h$winsadded[1]

# Add to hit out
hit.repl <- hit.repl %>% mutate(rv = -1 * case_when(pos == 'C' ~ rv.c
                                                , TRUE ~ rv.h))
hit.repl <- hit.repl %>% mutate(war = winsadded + rv) %>%
  arrange(desc(war))

# Join on positions into pitout
pit.repl <- left_join(pitout,
                      select(filter(positions,is.na(pitcher.pos) == FALSE),
                             PlayerId,pitpos.first)
) %>% rename('pos' = 'pitpos.first') %>%
  mutate(pos = case_when(
    PlayerId == '15440' ~ 'SP',
    PlayerId == '25880' ~ 'SP',
    TRUE ~ pos
  ))

# Find SP replacement value
sp.repl <- pit.repl %>% filter(pos == 'SP')
rv.sp <- sp.repl %>% mutate(rank.pos = row_number()) %>%
  filter(rank.pos == teams*sp.slots  + 1)
rv.sp <- rv.sp$winsadded[1]

# Find all other replacement value
rp.repl <- pit.repl %>% filter(pos == 'RP')
rv.rp <- rp.repl %>% mutate(rank.pos = row_number()) %>%
  filter(rank.pos == teams*rp.slots + 1)
rv.rp <- rv.rp$winsadded[1]

# Add to pit out
pit.repl <- pit.repl %>% mutate(rv = -1 * case_when(pos == 'SP' ~ rv.sp
                                                    , TRUE ~ rv.rp))
pit.repl <- pit.repl %>% mutate(war = winsadded + rv) %>%
  arrange(desc(war))

# Test how many wins there are
test.hit <- hit.repl %>% 
  filter(war > 0) %>%
    summarize(total.war = sum(war))

test.pit <- pit.repl %>% 
  filter(war > 0) %>%
  summarize(total.war = sum(war))

test.hit$total.war[1] + test.pit$total.war[1]
test.hit$total.war[1] / (test.hit$total.war[1] + test.pit$total.war[1])

# Union it all together for export
ftrx <- read.csv('Data/Fantrax-Players-HC Baseballers.csv')

playermap <- read.csv('Data/SFBB Player ID Map - PLAYERIDMAP.csv')
ftpos <- full_join(
  select(ftrx,ID,Position,ADP),
  select(playermap,FANTRAXNAME,IDFANGRAPHS,FANTRAXID),
  by = c("ID" = "FANTRAXID")
) %>%
## Alter positions here
  mutate(Position = case_when(
    IDFANGRAPHS == '15440' ~ 'SP',
    IDFANGRAPHS == '25880' ~ 'SP,RP',
    TRUE ~ Position
  ))

# Join all the tables together
war.rank <- union(select(hit.repl,PlayerId,Name,Team,winsadded,rv,war),select(pit.repl,PlayerId,Name,Team,winsadded,rv,war)) %>%
  group_by(PlayerId,Name,Team) %>%
  # The Ohtani adjustment
  summarise(winsadded = sum(winsadded),
            rv = sum(rv),
            war = sum(war)) %>%
  # Sort by WAR
  arrange(desc(war)) %>%
  # Join on Fantrax information
  left_join(.,select(ftpos,IDFANGRAPHS,Position,FANTRAXNAME,ADP), by = c("PlayerId" = "IDFANGRAPHS")) %>%
  # Join on the positions from the projections as backup
  left_join(.,select(positions,PlayerId,position)) %>%
  # Take the default name when Fantrax Name isnt in the crosswalk
  mutate(Name = case_when(is.na(FANTRAXNAME) ~ Name
                          , TRUE ~ FANTRAXNAME),
  # Take Auction Calculator positions when Fantrax position is unavailable, and switch out / for , to be the same as Fantrax
         Position = case_when(is.na(Position) ~ str_replace_all(position,',','/')
                              , TRUE ~ Position)) %>%
  # Narrow fields, consider adding FANTRAXID as insurance
    select(PlayerId,Name,Team,Position,winsadded,rv,war,ADP) %>%
  # drop duplicates
      distinct() %>%
        ungroup()

war.rank %>% write.csv('Output/warrank.csv')

# Create list for live update
live.hit <- left_join(
  select(hitdf,playerid,pa,ab,h,hr,r,rbi,bb,sb,avg,obp,slg,ops,x1b,x2b,x3b,sf,hbp),
  select(temphit,playerid,starts_with('week.'),starts_with('star.'))
)

live.hit %>% write.csv('Output/livehit.csv')

live.pit <- left_join(
  select(pitdf,playerid,qs,era,g,gs,sv,hld,svhld,ip,tbf,h,er,bb,k),
  select(temppit,playerid,starts_with('week.'))
)

live.pit %>% write.csv('Output/livepit.csv')

## Find models for spreadsheet
coefs <- tibble(label=c('intercept','slope'))
pct50 <- tibble(label='0.5')
i = 1
sumreg <- function(catg) {
  
  i <<- i + 1
  print(i)
  print(catg)
  summary(get(paste0('reg.',catg)))
  temp <- coef(get(paste0('reg.',catg)))
  coefs <- bind_cols(coefs,temp)
  coefs <<- coefs %>% 
    rename(!!sym(catg) := i)
  
  temp2 <- get(paste0(catg,'.5'))
  pct50 <<- bind_cols(pct50,temp2) %>% 
    rename(!!sym(catg) := i)
  
}
quietly(map(cats,sumreg))
coefs <- union(coefs,pct50)

# Export
coefs %>% write.csv('Output/coefs.csv')

