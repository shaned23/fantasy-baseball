
# Load Stuff+
rp.stuffplus_raw <- read_csv(paste0(.rp, 'stuffplus.csv'))

rp.stuffplus <- rp.stuffplus_raw %>%
  rename_with(tolower) %>%
  rename_with(
    ~str_replace(str_replace(.x, '\\+', '_plus'), ' ', '.')
  ) %>%
  rename(ip.previous = ip) %>%
  mutate(ip.previous = floor(ip.previous) + (ip.previous %% 1) * 10/3) %>%
  mutate(playerid = as.character(playerid))

# Load Pitch Mix
rp.pitchmix_raw <- read_csv(paste0(.rp, 'pitchperc.csv'))

rp.pitchmix <- rp.pitchmix_raw %>%
  rename_with(tolower) %>%
  rename_with(
    ~str_replace(paste0('perc.', .x), '\\%', ''), contains('%')
  ) %>%
  rename(ip.previous = ip) %>%
  mutate(ip.previous = floor(ip.previous) + (ip.previous %% 1) * 10/3) %>%
  mutate(playerid = as.character(playerid))

# Load Win Probability
rp.winprob_raw <- read_csv(paste0(.rp, 'winprobability.csv'))

rp.winprob <- rp.winprob_raw %>%
  rename_with(tolower) %>%
  mutate(playerid = as.character(playerid))

# Load OOPSY
rp.oopsy_raw <- read_csv(paste0(.rp, 'oopsy_projection.csv'))

rp.oopsy <- rp.oopsy_raw %>%
  rename_with(tolower)  %>%
  rename_with(
    ~str_replace(str_replace(.x, '/','.'), '\\%', '.perc')
  ) %>%
  mutate(playerid = as.character(playerid))

## Create variables of interest

# Create pitch classifications
pitch.class <- rp.stuffplus %>%
  select(playerid, starts_with('stf_plus')) %>%
  inner_join(select(rp.pitchmix, playerid, starts_with('perc.')), by = 'playerid') %>%
  pivot_longer(
    cols = starts_with('stf_plus.') | starts_with('perc.')
    , names_to = c('type', 'pitchabbreviation')
    , names_pattern = '(.+)\\.(.+)'
    , values_to = 'value'
  ) %>%
  pivot_wider(
    names_from = type
    , values_from = value
  ) %>%
  filter(!(is.na(stf_plus) & is.na(perc))) %>%
  mutate(pitchabbreviation = toupper(pitchabbreviation)) %>%
  inner_join(pitch.ref, by = 'pitchabbreviation') %>%
  mutate(primary.secondary = if_else(class == 'Fastball', 'Fastball', 'Secondary'))

# Find best pitches
best.by.class <- pitch.class %>%
  filter(perc > 0.1) %>%
  group_by(playerid, primary.secondary) %>%
  mutate(rank = min_rank(-stf_plus)) %>%
  ungroup() %>%
  filter(rank == 1) %>%
  select(-c(pitchabbreviation, class, rank)) %>%
  pivot_wider(
    names_from = primary.secondary
    , values_from = c(stf_plus, perc, pitch)
  ) %>%
  select(playerid, ends_with('Fastball'), ends_with('Secondary'))

# Find HHI
hhi.rp <- pitch.class %>%
  mutate(perc2 = (perc*100)^2) %>%
  group_by(playerid) %>%
  summarise(hhi = sum(perc2), .groups = 'drop')

# rank reliever on a team
rank.rp <- rp.oopsy %>%
  filter(sv >= 2 | hld >= 2) %>%
  group_by(team) %>%
  mutate(era.rank = min_rank(era)) %>%
  ungroup() %>%
  left_join(select(rp.stuffplus, playerid, stuff_plus), by = 'playerid') %>%
  group_by(team) %>%
  mutate(stuffplus.rank = min_rank(-stuff_plus)) %>%
  ungroup() %>%
  select(playerid, team, ends_with('rank'))
