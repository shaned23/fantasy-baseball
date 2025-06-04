
# Load team wins
wins.proj <- read.xlsx(paste0(.data,'proj2024wins.xlsx'), sheet = 'load')

# Load positions
pos.load <- read_csv(paste0(.data,'positions_mlb.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid) %>%
  select(fg.id,pos,g,gs) %>%
  filter(pos != 'P') %>%
  mutate(pos = if_else(pos %in% c('LF','RF','CF'),'OF',pos)) %>%
  group_by(fg.id,pos) %>%
  summarise(g = sum(g)
            , gs = sum(gs)) %>%
  ungroup() %>%
  mutate(g = if_else(g > 162,162,g)
         , gs = if_else(gs > 162,162,gs))
  
# Load other positions
other.raw <- read_csv(paste0(.data,'positions_ac.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid
         , pos2 = pos) %>%
  distinct()

other.positions <- other.raw %>%
  select(fg.id, pos2) %>%
  separate_wider_delim(pos2, delim = '/', names_sep = '_', too_few = 'align_start') %>%
  pivot_longer(cols = starts_with('pos2')
               , values_to = 'pos2'
               , names_to = NULL
               ) %>%
  filter(!is.na(pos2) & pos2 != 'DH')

# Pitcher positions
pitch.positions <- read.csv(paste0(.data,'savesandholds.csv')) %>%
  rename_with(tolower) %>%
  filter(season == 2023) %>%
  rename(fg.id = playerid) %>%
  mutate(sp.apps = gs
        , rp.apps = g - gs) %>%
  select(fg.id,sp.apps,rp.apps)

# Load other pitching positions
pitch.other.positions <- read_csv(paste0(.data,'positions_pitch.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = playerid
         , pos2 = pos) %>%
  distinct(fg.id, pos2)

# Create a big positions
positions <- pos.load %>%
  bind_rows(
    pitch.positions %>% 
      select(fg.id, sp.apps) %>%
      rename(g = sp.apps) %>%
      mutate(gs = g
             , pos = 'SP')
    ,     pitch.positions %>% 
      select(fg.id, rp.apps) %>%
      rename(g = rp.apps) %>%
      mutate(gs = g
             , pos = 'RP')
  ) %>%
  filter(g > 0) %>%
  mutate(fg.id = as.character(fg.id))

# FG ID to ottoneu ID
fg.on.xwalk <- read_csv(paste0(.data,'player_universe.csv')) %>%
  rename_with(tolower) %>%
  rename_with(~str_replace_all(.x,' ','.')) %>%
  mutate(fg.id = coalesce(as.character(fg.id),fg.minor.id)) %>%
  bind_rows(read_csv(paste0(.data,'extraplayers.csv')))

# Player map
extra.playerid <- read.csv(paste0(.data,'idsupplemental.csv'))

download.file('https://www.smartfantasybaseball.com/PLAYERIDMAPCSV'
              , destfile = paste0(.data,'playeridmap.csv'))

playerid.map <- read.csv(paste0(.data,'playeridmap.csv')) %>%
  rename_with(tolower) %>%
  rename(fg.id = idfangraphs
         , nfbc.id = nfbcid
         , nfbc.name = nfbcname
         , fantrax.id = fantraxid) %>%
  # Drop pitcher Ohtani
  filter(fantrax.id != '*06als*') %>%
  bind_rows(extra.playerid)

# Create a succinct position statement
pos.sort <- tibble(
  pos = c('C','1B','2B','3B','SS','OF')
) %>% 
  mutate(sorter = row_number())

# Pitch ref
pitch.ref <- read_csv(paste0(.rp, 'pitch_ref.csv'))
