
# Write logisitc function
log.nfbc <- function(data, league, cat) {
  
  # filter data
  df <- data %>%
    filter(league.type == !!league)
  
  # Run regression
  y.var = paste0('y.',cat)
  log.model <- glm(reformulate(cat,y.var), data = df, family = quasibinomial())
  
  return(log.model)
  
}

map(leagues, function(l){
  
  coef.mat <- tibble(coef = c('b0','b1','0.5'))
  
  map(cats, function(s){
    
    model <- log.nfbc(data = nfbc.standings, league = l, cat = s)
    
    # Add to coef.mat
    coef.mat <<- coef.mat %>%
      mutate(!!sym(s) := case_when(
        coef == 'b0' ~ model$coefficients[1]
        , coef == 'b1' ~ model$coefficients[2]
        , coef == '0.5' ~ -model$coefficients[1]/model$coefficients[2]
      )
      , )
    
  })
    
    googlesheets4::write_sheet(coef.mat %>%
                                 select('coef','r','hr','rbi','sb','avg','w','k','sv','era','whip')
                               , ss = get(paste0(tolower(l),'.book'))
                               , sheet = 'coefficients')
    
    assign(paste0('coef.hit.',l)
      , coef.mat %>%
        select(coef:avg) %>%
      pivot_longer(cols = -coef
                   , names_to = 'cat'
                   , values_to = 'value') %>%
      pivot_wider(
        names_from = c(coef,cat)
        , values_from = value
      ) %>%
      rename_with(~str_replace(.,'_','.'))
      , .GlobalEnv)
    
    assign(paste0('coef.pitch.',l)
           , coef.mat %>%
             select(coef,w:whip) %>%
             pivot_longer(cols = -coef
                          , names_to = 'cat'
                          , values_to = 'value') %>%
             pivot_wider(
               names_from = c(coef,cat)
               , values_from = value
             ) %>%
             rename_with(~str_replace(.,'_','.'))
           , .GlobalEnv)

})
  


  