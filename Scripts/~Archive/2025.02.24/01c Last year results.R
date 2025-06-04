
# Load last year's results data
# Draft Champions
season.list <- list.files(.nfbc, pattern = ".csv$", full.names = TRUE)

nfbc.raw <- tibble()
f = season.list[1]

map(season.list, function(f){
  
  print(str_extract(f,"(?<=/)(\\d{4}\\w+)(?=\\.csv$)"))
  
  if(str_detect(f, 'draftchampions')) {
    lt = 'DC'
    t = 15
  } else if(str_detect(f, 'onlinechampionship')) {
    lt = 'OC'
    t = 12    
  } else {
    print('ERROR!')
  }
    
  year = str_extract(f,"(?<=/)(\\d{4})(?=\\w+\\.csv$)")
  
  loaded <- read_csv(f
                     , col_types = 'c') %>%
    mutate(league.type = lt
           , teams = t
           , season = year) %>%
    rename_with(~str_trim(tolower(.x), 'both')) %>%
    rename_with(~str_replace(.x,'x.','')) %>%
    rename('h' = 'h...11'
           , h.pitcher = 'h...18')
  
  nfbc.raw <<- bind_rows(nfbc.raw, loaded)
  
})

# Combine the datasets
nfbc.standings <- nfbc.raw %>%
  rename(overall.rank = rank
         , overall.pts = points
         , sv = s) %>%
  filter(overall.rank != '' & team != '') %>%
  mutate(across(c(overall.rank,ab,h,ip,er,bb,h.pitcher),~as.numeric(.))) %>%
  mutate(ip = floor(ip) + (ip %% 1) * 10/3
         , era = (er/ip)*9
         , whip = (bb+h.pitcher)/ip
         , avg = h/ab) %>%
  group_by(league, league.type, season) %>%
  mutate(across(c(r,hr,rbi,sb,avg,w,sv,k), ~rank(., ties.method = 'average'), .names = 'pts.{.col}')) %>%
  mutate(across(c(era,whip), ~rank(desc(.), ties.method = 'average'), .names = 'pts.{.col}')) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pts.total = sum(c_across(starts_with('pts.')))) %>%
  group_by(league) %>%
  mutate(league.rank = min_rank(-pts.total)) %>%
  ungroup() %>%
  mutate(across(starts_with('pts.'), ~(. - 1) / (teams - 1), .names = 'y{.col}')) %>%
  rename_with(~str_replace(.x,'ypts.','y.'))

# Run plots
# nfbc.melted <- nfbc.standings %>%
#   select(team, league, season, league.type, starts_with('pts'), r, hr, rbi, sb, avg, w, k, sv, era, whip) %>%
#   rename_with(~paste0('value.', .x), r:whip) %>%
#   select(-pts.total) %>%  # Remove the 'pts.total' column
#   pivot_longer(
#     cols = starts_with('pts') | starts_with('value'),  # Select both 'pts' and 'value' columns
#     names_to = c(".value", "category"),  # Split the column name into 'pts' and 'category'
#     names_pattern = "(.*)\\.(.*)"  # Pattern to separate 'pts' from the 'category'
#   ) %>%
#   mutate(season = as.factor(season))
# 
# nfbc.dc.chart <- ggplot(filter(nfbc.melted, league.type == 'DC'), aes(x = value, y = pts, color = season)) +
#   geom_point(alpha = 0.4, size = 0.1) +  # Scatter plot with some transparency
#   geom_smooth(method = "loess", aes(group = season), se = FALSE, size = 0.1) +  # Add loess lines for 2023 and 2024
#   facet_wrap(~category, scales = "free", ncol = 2) +  # Facet by category with free scales
#   scale_color_manual(values = c("2023" = "blue", "2024" = "orange")) +
#   labs(
#     x = "Value",
#     y = "Points",
#     title = "Points vs Category Value by Year (2023 and 2024)"
#   ) +
#   theme_minimal() +  # Clean theme
#   theme(
#     strip.text = element_text(size = 10),  # Adjust facet text size
#     legend.title = element_blank()  # Remove legend title for simplicity
#   )
# plot(nfbc.dc.chart)
