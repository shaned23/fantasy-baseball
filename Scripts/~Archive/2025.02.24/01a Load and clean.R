
# Consider adding H/9 before running the projections

# Load data
svhld.raw <- read.csv(paste0(.data,'savesandholds.csv'))

# Clean data
dfsvhld <- svhld.raw %>%
  rename_with(tolower,everything()) %>%
  rename(perc.k = k.
         , perc.bb = bb.
         , fg.id = playerid
         , mlbam.id = mlbamid) %>%
  mutate(ip = floor(ip) + (ip %% 1) * 10/3
         , team = case_when(
              team == 'FLA' ~ 'MIA'
            , team == '- - -' ~ 'MLB'
            , TRUE ~ team)
         )

# Load historical win/loss data
wins.raw <- read.csv(paste0(.data,'winsbyseason.csv'))

# Clean wins
dfwins <- wins.raw %>%
  rename(season = Year
         , g = G) %>%
  pivot_longer(cols = -c(season,g)
    , names_to = 'team'
    , values_to = 'team.wins') %>%
  filter(team.wins != 'NA') %>%
  rowwise() %>%
  mutate(g = min(g,162)) %>%
  ungroup() %>%
  mutate(winpct = team.wins / g)

# Combine data
df.predict <- dfsvhld %>%
  left_join(dfwins %>% select(team,season,winpct)
            , by = join_by(team,season)) %>%
  verify(!is.na(winpct) | team == 'MLB') %>%
  mutate(winpct = if_else(is.na(winpct), 0.5, winpct)
         , sv.hld = sv + hld
         , sv.pct = sv / sv.hld
         , sv.per.ip = sv / ip
         , hld.per.ip = hld / ip
         , perc.rpg = if_else(g == 0, 0,(g-gs)/g))

df.model <- df.predict %>%
  filter(season != 2020) %>%
  select(-c(name, team, nameascii, fg.id, mlbam.id)) %>%
  mutate(id = row_number())

# Create test and train sets
set.seed(23875)

df.train <- df.model %>%
  slice_sample(by = season, prop = 0.8)

df.test <- anti_join(df.model, df.train, by = 'id') %>%
  select(-id)

df.train <- select(df.train,-id)

# Create test and train sets w/o null save percentage
df.nona <- df.model %>%
  filter(!is.na(sv.pct))

df.train.nona <- df.nona %>%
  slice_sample(by = season, prop = 0.8)

df.test.nona <- anti_join(df.nona, df.train.nona, by = 'id') %>%
  select(-id)

df.train.nona <- select(df.train.nona,-id)
