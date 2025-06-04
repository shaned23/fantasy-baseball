
# model = sv4
# testdata = hld4
# yvar = 'hld'
# predict = FALSE

rfr2 <- function(model=NULL, testdata, yvar, predict = TRUE) {
  
  # Predict variable
  if(predict == TRUE) {
    df <- testdata %>%
      mutate(y.hat = predict(model, testdata)
             , y.bar = mean(!!sym(yvar))
             , se = (!!sym(yvar) - y.hat)^2
             , sq = (!!sym(yvar) - y.bar)^2) %>%
      summarise(rss = sum(se, na.rm = TRUE)
                , tss = sum(sq, na.rm = TRUE)) %>%
      mutate(r2 = 1 - rss/tss)
  } else {
    df <- testdata %>%
      mutate(y.bar = mean(!!sym(yvar))
             , se = (!!sym(yvar) - y.hat)^2
             , sq = (!!sym(yvar) - y.bar)^2) %>%
      summarise(rss = sum(se, na.rm = TRUE)
                , tss = sum(sq, na.rm = TRUE)) %>%
      mutate(r2 = 1 - rss/tss) 
      
  }
  
  return(df$r2[1])
  
}
