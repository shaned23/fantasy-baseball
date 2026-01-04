
# Load projections
hit.raw <- read.xlsx(paste0(.proj,'masterprojections.xlsx'),sheet = 'Hitters-Master')
pitch.raw <- read.xlsx(paste0(.proj,'masterprojections.xlsx'),sheet = 'Pitchers-Master')
hit.450.raw <- read.xlsx(paste0(.proj,'masterprojections.xlsx'),sheet = 'Hitters-450')
pitch.120.raw <- read.xlsx(paste0(.proj,'masterprojections.xlsx'),sheet = 'Pitchers-120')

# Clean data
sides <- list('hit','pitch','hit.450','pitch.120')

map(sides, function(x){
  
  df <- get(paste0(x,'.raw'))
  
  assign(
     paste0(x,'.proj')
    ,df %>%
      rename_with(tolower) %>%
      rename_with(~str_replace(.x,'/','.')) %>%
      rename_with(~paste0('perc.',str_replace(.x,'%','')), ends_with('%')) %>%
      rename(fg.id = playerid)
    , .GlobalEnv
  )
  
})

# Add the saves to pitch.proj
# pitch.proj <- pitch.proj %>%
#   mutate(team = if_else(is.na(team),'MLB',team)
#          , season = 2024) %>%
#   left_join(wins.proj, by = 'team') %>%
#   select(-mlbteam)
# 
# pitch.proj <- pitch.proj %>%
#   select(-c(sv,holds)) %>%
#   mutate(sv = predict(sv1,pitch.proj), .before = h)
# 
# pitch.proj <- pitch.proj %>%
#   mutate(hld = predict(hld1,pitch.proj), .before = h)

