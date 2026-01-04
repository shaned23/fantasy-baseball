
# Load projections for first iterations
hit.base.oc <- read.csv(paste0(.data,'positions_ac20.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  arrange(desc(dollars)) %>%
  mutate(rank = row_number()
         , above.replacement = dollars > 0) %>%
  select(fg.id,rank,above.replacement)

hit.base.dc <- read.csv(paste0(.data,'batx_hitting_dc.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  arrange(desc(dollars)) %>%
  mutate(rank = row_number()
         , above.replacement = dollars > 0) %>%
  select(fg.id,rank,above.replacement)

pitch.base.oc <- read.csv(paste0(.data,'oopsy_pitching_oc.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  arrange(desc(dollars)) %>%
  mutate(rank = row_number()
         , above.replacement = dollars > 0) %>%
  select(fg.id,rank,above.replacement)

pitch.base.dc <- read.csv(paste0(.data,'oopsy_pitching_dc.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  arrange(desc(dollars)) %>%
  mutate(rank = row_number()
         , above.replacement = dollars > 0) %>%
  select(fg.id,rank,above.replacement)

# Create base datasets
map(c('oc', 'dc'), function(l){
  
  assign(
    paste0('base.',l)
    , bind_rows(get(paste0('hit.base.', l)), get(paste0('pitch.base.', l))) %>%
      group_by(fg.id) %>%
      summarise(rank = min(rank)
                , above.replacement = max(above.replacement)
                , .groups = 'drop')
    , envir = .GlobalEnv
  )
  
})

# Load ADPs
map(leagues,function(l){

  raw <- read_tsv(paste0(.data,'NFBC ADP/',l,'/ADP.tsv'))
  
  export <- raw %>%
    rename_with(tolower) %>%
    rename(nfbc.id = 'player id'
           , pos = 'position(s)'
           , team = 4
           , min.pick = 'min pick'
           , max.pick = 'max pick') %>%
    left_join(select(playerid.map,nfbc.id,fg.id), by = 'nfbc.id') %>%
    distinct()
    
  
  assign(
    paste0('adp.',tolower(l))
    , export
    , .GlobalEnv
  )  
  
})

# Check if all the names in the ADPs have a matching FanGraphs ID
if(nrow(
  bind_rows(
    adp.dc %>% filter(is.na(fg.id))
    , adp.oc %>% filter(is.na(fg.id))
  ) %>%
  select(nfbc.id, player, fg.id) %>%
  distinct()
) > 0) {
  
  print('Add names to export!')
  export <-   bind_rows(
    adp.dc %>% filter(is.na(fg.id))
    , adp.oc %>% filter(is.na(fg.id))
  ) %>%
  select(nfbc.id, player, fg.id) %>%
  distinct()
  write_clip(export)
  
}

# Create positions list
# Create a succinct position statement
pos.sort <- tibble(
  pos = c('C','1B','2B','3B','SS','OF')
) %>% 
  mutate(sorter = row_number())

# Get projection positions
ac.20pos <- read.csv(paste0(.data,'positions_ac20.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  select(fg.id,pos) %>%
  mutate(pos = str_replace(pos,'/DH','')
         , pos = str_replace(pos,'DH','UT')) %>%
  separate_wider_delim(pos, delim = '/', names_sep = '_', too_few = 'align_start') %>%
  pivot_longer(cols = starts_with('pos')
               , values_to = 'pos'
               , names_to = NULL
  ) %>% filter(!is.na(pos)) %>%
  mutate(pos = str_replace(pos,'P','UT'))

# NFBC positions
adp.pos <- bind_rows(
  adp.dc, adp.oc
) %>%
  distinct(fg.id, pos) %>%
  mutate(pos = if_else(pos == 'UT, P','UT',pos)) %>%
  separate_wider_delim(pos, delim = ', ', names_sep = '_', too_few = 'align_start') %>%
  pivot_longer(cols = starts_with('pos')
               , values_to = 'pos'
               , names_to = NULL
  ) %>% filter(!is.na(pos))

nfbc.pos <- bind_rows(list(
    adp.pos
    , positions %>%
      filter(g >= 20) %>%
      left_join(pos.sort, by = 'pos') %>%
      arrange(sorter) %>%
      select(fg.id,pos)
    , ac.20pos
  ), .id = 'source'
) %>%
  mutate(source = as.numeric(source)) %>%
  group_by(fg.id) %>%
  mutate(max = max(source)) %>%
  ungroup() %>%
  filter(max == source) %>%
  filter(pos != 'P') %>%
  select(fg.id, pos) %>%
  # Henry Davis
  bind_rows(tibble(fg.id = '29617', pos = 'C'))

nfbc.pos.wide <- nfbc.pos %>%
  left_join(pos.sort, by = 'pos') %>%
  group_by(fg.id) %>%
  arrange(sorter) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  select(-sorter) %>%
  pivot_wider(names_from = n
    , values_from = pos) %>%
  mutate(pos = paste(`1`,`2`,`3`,`4`,`5`,sep = ', ')
         , pos = str_replace_all(pos,', NA','')
  ) %>%
  select(fg.id,pos)

# Prep projections
hit.nfbc <- hit.proj %>%
  left_join(nfbc.pos, by = 'fg.id') %>%
  mutate(pos = if_else(is.na(pos),'UT',pos)) %>%
  select(fg.id,pos,weeks,ab,h,hr,r,rbi,sb,avg) 

pitch.nfbc <- pitch.proj %>%
  mutate(pos = 'P') %>%
  select(fg.id,weeks,ip,w,era,so,sv,h,er,bb,era,whip) %>%
  rename(k = so)

hit.450.nfbc <- hit.450.proj %>%
  left_join(nfbc.pos, by = 'fg.id') %>%
  mutate(pos = if_else(is.na(pos),'UT',pos)) %>%
  select(fg.id,pos,weeks,ab,h,hr,r,rbi,sb,avg) 

pitch.120.nfbc <- pitch.120.proj %>%
  mutate(pos = 'P') %>%
  select(fg.id,weeks,ip,w,era,so,sv,h,er,bb,era,whip) %>%
  rename(k = so)
