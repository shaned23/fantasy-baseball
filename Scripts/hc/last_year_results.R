# Load last year stats
history <- read.xlsx(paste0(.data,'hc_history.xlsx'),'import')

shanevol <- read.xlsx(paste0(.data,'shane ab ip.xlsx'),'Sheet1')

# Join together
df <- left_join(history,shanevol,by = c("team" = "Team", "scoringperiod" = "scoringperiod",'year' = 'year' )) %>% 
  
  # Adjust saves
  # Using x1.5 for now
  # 1.5x = 27.41%
  # .9 removes the BS in theory
  mutate(svhld = if_else(year == 2022,(svhld/.9) * (1+.2741),svhld)) %>%
  # Adjust SBs
  mutate(sb = if_else(year == 2022,sb * (1+.2), sb)) 
  
  # Adjust OPS (done very badly in terms of coding etiquette)
  #mutate(ops = ops * (0.773/0.761))

# Create df7
df7 <- df %>% filter(daysinperiod == 7)
