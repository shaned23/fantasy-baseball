
# Set parameters
teams = 15
slots = 23
total = 30
c = 2
hits = 14
pits = 9
ab.est = 7300
ip.est = 1300
scal.hit = (hits - 1)/hits
scal.pit = (pits - 1)/pits

# Player map
download.file('https://www.smartfantasybaseball.com/PLAYERIDMAPCSV'
  , destfile = paste0(.data,'playeridmap.csv'))
playerid.map <- read.csv(paste0(.data,'playeridmap.csv')) %>%
  rename_with(tolower)

