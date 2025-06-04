## Run regressions

cats <- c('r','hr','rbi','sb','avg','ops','k','era','qs','bb9','hip','svhld')

runreg <- function(data,catg){
  
  if(catg %in% c('sb','svhld')) {
    run.data <- filter(data,year != 2022)
  } else {
    run.data <- filter(data,get(catg) != 99999)
  }
  
  # Run regression
  y.var = paste0('w.',catg)
  log.model <- glm(reformulate(catg,y.var), data = run.data, family = quasibinomial())
  
  return(log.model)
  
}

coef.mat <- tibble(coef = c('b0','b1','0.5'))

map(cats, function(s){
  
  model <- runreg(data = df7, catg = s)
  
  # Add to coef.mat
  coef.mat <<- coef.mat %>%
    mutate(!!sym(s) := case_when(
      coef == 'b0' ~ model$coefficients[1]
      , coef == 'b1' ~ model$coefficients[2]
      , coef == '0.5' ~ -model$coefficients[1]/model$coefficients[2]
    )
    , )
  
})

write_sheet(coef.mat,'11VwAjO9bgMODtlKxfrJdwTi34OmPyu77k0KVr19mrkk','coefficients')
