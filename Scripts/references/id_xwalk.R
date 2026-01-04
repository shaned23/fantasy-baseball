# Fantrax, FG, ottoneu, NFBC ADP crosswalks
fantrax <- read.csv(paste0(.data,'Fantrax-Players-HC Baseballers.csv')) %>%
  rename_with(tolower) %>%
  rename(fantrax.id = id
         , name = player) %>%
  distinct(fantrax.id, name)


hit.names <- read.xlsx(paste0(.proj,'masterprojections.xlsx'),sheet = 'Hitters-Steamer', startRow = 2)
hit.names2 <- read.xlsx(paste0(.proj,'masterprojections.xlsx'),sheet = 'Hitters-OOPSY', startRow = 2)
pitch.names <- read.xlsx(paste0(.proj,'masterprojections.xlsx'),sheet = 'Pitchers-Steamer', startRow = 2)

fangraphs <- bind_rows(hit.names, hit.names2, pitch.names) %>%
  rename_with(tolower) %>%
  rename(fangraphs.id = playerid
         , name.accents = name
         , name = nameascii) %>%
  distinct(fangraphs.id, name)

#test <- full_join(fangraphs, fantrax, by = 'name')
