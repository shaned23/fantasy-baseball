

# Calculate AB and IP estimates
est <- shanevol %>%
  summarise(across(c(ab,ip),mean,.names = '{.col}.est'))
ab.est = est$ab.est[1]
ip.est = est$ip.est[1]

# Load projections for first iterations
hit.base <- read.csv(paste0(.data,'batx_hitting_hc.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  arrange(desc(dollars)) %>%
  mutate(rank = row_number()
         , above.replacement = dollars > 0) %>%
  select(fg.id,rank,above.replacement)

pitch.base <- read.csv(paste0(.data,'atc_pitching_hc.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  arrange(desc(dollars)) %>%
  mutate(rank = row_number()
         , above.replacement = dollars > 0) %>%
  select(fg.id,rank,above.replacement)


base <- bind_rows(hit.base, pitch.base)


# Load ADPs
raw.FantraxADP <- read.csv(paste0(.data,'Fantrax-Players-HC Baseballers.csv'))

adp.hc <- raw.FantraxADP %>%
  rename_with(tolower) %>%
  rename(fantrax.id = 'id'
         , pos = 'position'
         ) %>%
  left_join(select(playerid.map,fantrax.id,fg.id), by = 'fantrax.id') %>%
  mutate(test = as.numeric(adp), .after = adp) %>%
  verify((is.na(test) & adp == '-') | !is.na(test)) %>%
  mutate(adp = test) %>% select(-test) %>%
  distinct()

# Create positions list
# Create a succinct position statement
pos.sort <- tibble(
  pos = c('C','1B','2B','3B','SS','OF')
) %>% 
  mutate(sorter = row_number())

# Get projection positions
fantrax.pos <- adp.hc %>%
  separate_wider_delim(pos, delim = ',', names_sep = '_', too_few = 'align_start') %>%
  pivot_longer(cols = starts_with('pos')
               , values_to = 'pos'
               , names_to = NULL
  ) %>% filter(!is.na(pos))

# Prep projections
hit.hc <- hit.proj %>%
  left_join(fantrax.pos, by = 'fg.id') %>%
  mutate(pos = if_else(is.na(pos),'UT',pos)) %>%
  rename(
    'x1b' = '1b'
    , 'x2b' = '2b'
    , 'x3b'= '3b'
  ) %>%
  select(fg.id,pos,weeks,ab,h,x1b,x2b,x3b,hr,sf,hbp,r,rbi,bb,sb,avg,ops) %>%
  mutate(across(ab:sb,~.x/weeks, .names = 'week.{.col}')) %>%
  filter(!(pos %in% c('INF','SP','RP','P')))

pitch.hc <- pitch.proj %>%
  left_join(fantrax.pos, by = 'fg.id') %>%
  mutate(pos = if_else(is.na(pos),
                       if_else(gs > 2,'SP','RP'), pos)
         ) %>%
  rename(k = so) %>%
  mutate(hip = h.9/9
         , svhld = sv * 1.5 + holds 
         ) %>% select(-c(h.9,sv,holds)) %>%
  select(fg.id,weeks,pos,ip,qs,k,h,er,bb,svhld,era,bb.9,hip) %>%
  rename(bb9 = bb.9) %>%
  mutate(across(ip:svhld,~.x/weeks, .names = 'week.{.col}')) %>%
  filter(pos %in% c('SP','RP','P'))


# Coefficients
coef.hit.hc <- select(coef.mat,coef,r,hr,rbi,sb,avg,ops) %>%
  pivot_longer(cols = -coef
               , names_to = 'cat'
               , values_to = 'value') %>%
  pivot_wider(
    names_from = c(coef,cat)
    , values_from = value
  ) %>%
  rename_with(~str_replace(.,'_','.'))

coef.pitch.hc <- select(coef.mat,coef,k,era,qs,bb9,hip,svhld) %>%
  pivot_longer(cols = -coef
               , names_to = 'cat'
               , values_to = 'value') %>%
  pivot_wider(
    names_from = c(coef,cat)
    , values_from = value
  ) %>%
  rename_with(~str_replace(.,'_','.'))
