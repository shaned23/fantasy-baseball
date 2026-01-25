
# Create scoring tibbles
on.hit.scoring <- tibble(
  cat = c('ab','h','2b','3b','hr','bb','hbp','sb','cs')
  , pts = c(-1,5.6,2.9,5.7,9.4,3,3,1.9,-2.8)
)

on.pitch.scoring <- tibble(
  cat = c('ip','so','h','bb','hbp','hr','sv','holds')
  , pts = c(7.4,2,-2.6,-3,-3,-12.3,5,4)
)

# on.pitch.scoring <- tibble(
#   cat = c('ip','so','h','bb','hbp','hr')
#   , pts = c(7.4,2,-2.6,-3,-3,-12.3)
# )

# Add positions
on.positions <- fg.on.xwalk %>%
  rename(pos = ottoneu.positions) %>%
  select(fg.id, pos) %>%
  separate_wider_delim(pos, delim = '/', names_sep = '_', too_few = 'align_start') %>%
  pivot_longer(cols = starts_with('pos')
               , values_to = 'pos'
               , names_to = NULL
  ) %>%
  filter(!is.na(pos))

# Create a succinct position statement
pos.sort <- tibble(
  pos = c('C','1B','2B','3B','SS','OF')
) %>% 
  mutate(sorter = row_number())

on.widepos <- on.positions %>%
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
         ,pos = str_replace(pos,'DH','Util')) %>%
  distinct()

# Create hitting table
hit.on <- hit.proj %>%
  # Do scoring calculations
  select(fg.id,weeks,g,ab,h,`2b`,`3b`,hr,bb,hbp,sb,cs) %>%
  pivot_longer(cols = ab:cs
               , names_to = 'cat') %>%
  left_join(on.hit.scoring, by = 'cat') %>%
  verify(!is.na(pts)) %>%
  mutate(total.pts = value * pts) %>%
  group_by(fg.id,weeks,g) %>%
  summarise(total.pts = sum(total.pts)) %>%
  ungroup() %>%
  # Add positions
  left_join(filter(on.positions,!str_detect(pos,'P')), by = 'fg.id') %>%
  #left_join(filter(other.positions,!str_detect(pos2,'P')), by = 'fg.id', relationship = 'many-to-many') %>%
  mutate(
    #pos = case_when(
  #   is.na(pos2) & is.na(pos) ~ 'Util'
  #   , !is.na(pos2) & is.na(pos) ~ pos2
  #   , TRUE ~ pos)
      set = if_else(pos %in% c('2B','SS'), 'MI',NA)
    ) %>%
  # select(-pos2) %>%
  filter(!str_detect(pos,'P')) %>%
  distinct() %>%
  # Calculate possible key pt values
  mutate(pts.per.g = total.pts / g
         , pts.per.week = total.pts / weeks) %>%
  select(fg.id,pos,set,weeks,g,total.pts,pts.per.g,pts.per.week) %>%
  arrange(-total.pts)

# Create pitching table
pitch.on1 <- pitch.proj %>%
  # Do scoring calculations
  #select(fg.id,weeks,ip,so,h,bb,hbp,hr,sv,holds) %>%
  select(fg.id,weeks,ip,so,h,bb,hbp,hr) %>%
  mutate(ip2 = ip, .before = ip) %>%
  pivot_longer(cols = ip:hr
               , names_to = 'cat') %>%
  left_join(on.pitch.scoring, by = 'cat') %>%
  verify(!is.na(pts)) %>%
  mutate(total.pts = value * pts) %>%
  group_by(fg.id,weeks,ip2) %>%
  summarise(total.pts = sum(total.pts)) %>%
  ungroup() %>%
  # Add positions
  left_join(filter(on.positions,str_detect(pos,'P')), by = 'fg.id') %>%
  rename(ip = ip2) %>%
  left_join(pitch.other.positions, by = 'fg.id') %>%
  mutate(pos = case_when(
    # Erik Fedde
    #   fg.id == '17425' ~ 'SP'
    # , fg.id == '22766' ~ 'SP'
    fg.id %in%  c('sa3025686', 'sa3025685', '10954') ~ 'SP'
    , is.na(pos2) & is.na(pos) ~ 'RP'
    , !is.na(pos2) & is.na(pos) ~ pos2
    , TRUE ~ pos)) %>%
  select(-pos2)

# Create an SP and RP row for each pitcher  that lacks a position
# pitch.na <- bind_rows(list('SP' = pitch.on1 %>%
#                         filter(is.na(pos)) %>%
#                           select(-pos)
#   , 'RP' = pitch.on1 %>%
#     filter(is.na(pos)) %>%
#     select(-pos))
#   , .id = 'pos')


# Put all the pitchers together
pitch.on <- pitch.on1 %>%
  #bind_rows(pitch.on1,pitch.na)  %>%
  # Drop rows for players with less than 100 IP and SP/RP
  group_by(fg.id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(pos = if_else(ip > 100 & pos == 'RP', 'SP', pos)) %>%
  filter(n == 1 | (pos == 'SP' & ip > 100) | (pos == 'RP' & ip < 100)) %>%
  # Calculate possible key pt values
  mutate(pts.per.ip = total.pts / ip
         , pts.per.week = total.pts / weeks) %>%
  select(fg.id,pos,weeks,ip,total.pts,pts.per.ip,pts.per.week) %>%
  arrange(-total.pts)
  
