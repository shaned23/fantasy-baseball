
# Create scoring tibbles
mend.hit.scoring <- tibble(
  cat = c('ab','h','2b','3b','hr','bb','hbp','r','rbi','sb','cs')
  , pts = c(-0.6,2,0.5,1,2,1,1,0.6,0.6,0.6,-0.6)
)

mend.pitch.scoring <- tibble(
  cat = c('ip','so','h','bb','hbp','er','sv','holds','bs')
  , pts = c(2,0.5,-0.4,-0.4,-0.4,-2.2,4,2.5,-3)
)

# Add positions
mend.positions <- positions %>%
  filter(
    (!pos %in% c('SP','RP') & g >= 10) |
      (pos %in% c('SP','RP') & (g >= 5))
  ) %>%
  select(fg.id, pos)

mend.widepos <- mend.positions %>%
  left_join(pos.sort, by = 'pos') %>%
  group_by(fg.id) %>%
  arrange(sorter) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  select(-sorter) %>%
  pivot_wider(names_from = n
    , values_from = pos) %>%
  mutate(pos = str_replace_all(paste(`1`,`2`,`3`,`4`,`5`,sep = '/'),'/NA','')) %>%
  select(fg.id,pos) %>%
  full_join(select(other.raw, pos2,fg.id),by = 'fg.id') %>%
  mutate(pos = if_else(is.na(pos),pos2,pos)) %>%
  select(-pos2) %>%
  mutate(pos = str_replace_all(pos,'/DH','')
         ,pos = str_replace(pos,'DH','Util')
         , pos = if_else(fg.id == '19755','Util',pos)) %>%
  distinct()

# Create hitting table
hit.mend <- hit.proj %>%
  # Do scoring calculations
  select(fg.id,weeks,g,ab,h,`2b`,`3b`,hr,bb,hbp,r,rbi,sb,cs) %>%
  pivot_longer(cols = ab:cs
               , names_to = 'cat') %>%
  left_join(mend.hit.scoring, by = 'cat') %>%
  verify(!is.na(pts)) %>%
  mutate(total.pts = value * pts) %>%
  group_by(fg.id,weeks,g) %>%
  summarise(total.pts = sum(total.pts)) %>%
  ungroup() %>%
  # Add positions
  left_join(filter(mend.positions,!str_detect(pos,'P')), by = 'fg.id') %>%
  left_join(filter(other.positions,!str_detect(pos2,'P')), by = 'fg.id', relationship = 'many-to-many') %>%
  mutate(pos = case_when(
    is.na(pos2) & is.na(pos) ~ 'Util'
    , !is.na(pos2) & is.na(pos) ~ pos2
    , TRUE ~ pos)
    ) %>%
  select(-pos2) %>%
  filter(!str_detect(pos,'P')) %>%
  distinct() %>%
  select(fg.id,pos,weeks,g,total.pts) %>%
  arrange(-total.pts)

# Create pitching table
pitch.mend1 <- pitch.proj %>%
  # Do scoring calculations
  select(fg.id,weeks,ip,so,h,bb,hbp,er,sv,holds) %>%
  mutate(bs = (sv + holds) * 0.1) %>%
  mutate(ip2 = ip, .before = ip) %>%
  pivot_longer(cols = ip:holds
               , names_to = 'cat') %>%
  left_join(mend.pitch.scoring, by = 'cat') %>%
  verify(!is.na(pts)) %>%
  mutate(total.pts = value * pts) %>%
  group_by(fg.id,weeks,ip2) %>%
  summarise(total.pts = sum(total.pts)) %>%
  ungroup() %>%
  # Add positions
  left_join(filter(mend.positions,str_detect(pos,'P')), by = 'fg.id') %>%
  left_join(pitch.other.positions, by = 'fg.id') %>%
  mutate(pos = case_when(
      is.na(pos2) & is.na(pos) ~ 'RP'
    , !is.na(pos2) & is.na(pos) ~ pos2
    , TRUE ~ pos)) %>%
  rename(ip = ip2) %>%
  select(-pos2)

# Put all the pitchers together
pitch.mend <- pitch.mend1 %>%
  # Drop rows for players with less than 100 IP and SP/RP
  group_by(fg.id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1 | (pos == 'SP' & ip > 100) | (pos == 'RP' & ip < 100)) %>%
  select(fg.id,pos,weeks,ip,total.pts) %>%
  arrange(-total.pts)
  
