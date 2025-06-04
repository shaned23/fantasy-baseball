
#### SET ENVIRONMENT #####

# packages that are needed. 
libs <- c("tidyverse", "DBI", "RPostgreSQL", "glue","openxlsx",'clipr','knitr','flextable','assertr','scales'
          ,'randomForest','rpart', 'googlesheets4', 'plotly')

# install and load needed packages
lapply(libs, function(x){
  if(!(x %in% installed.packages())) install.packages(x)
})
lapply(libs,library, character.only = TRUE)

# Clear environment
rm(list = ls())

# Set links
.scripts <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),'/')
setwd(.scripts)
setwd('../..')
.data <- paste0(getwd(),'/Data/')
print(.data)

# Load data
raw.k <- read_csv(paste0(.data,'kper_statcast.csv')) %>%
  rename_with(~tolower(str_replace(.x, '[+%]','')))

# Load predict data
predict.df <- read_csv(paste0(.data,'maxevdata.csv'))

# Chart Ks vs maxEV
k.df <- raw.k %>%
  filter(g > 162 & pa >= 200 & k <= 0.4)

k_maxev <- ggplot(k.df, aes(x = k, y = maxev)) +
  geom_point() +           # Scatterplot points
  geom_smooth(method = "lm", se = FALSE) 
k_maxev

k_maxev100 <- ggplot(k.df, aes(x = k, y = maxev)) +
  geom_point() +           # Scatterplot points
  geom_smooth(method = "lm", se = FALSE) 
k_maxev100

# Plotly
k.df90 <- filter(k.df, wrc >= 90)
k.maxev90.lm <- lm(maxev ~ k, data = k.df90)
summary(k.maxev90.lm)
plot <- plot_ly(k.df90, x = ~k, y = ~maxev, type = 'scatter', mode = 'markers', 
                text = ~paste("Name: ", name, 
                              "<br>K: ", percent(k, accuracy = 0.1),   # Format 'k' as a percentage
                              "<br>MaxEV: ", round(maxev, 1)),  # Format 'maxev' to 1 decimal place
                hoverinfo = 'text') 

plot <- plot %>% add_trace(x = k.df90$k, y = k.maxev90.lm$fitted.values, 
                           mode = 'lines', name = 'Linear Regression')
plot


wrc.lm <- lm(wrc ~ poly(maxev,3) + poly(k,10) + poly(maxev,3):poly(k,10), data = k.df)
summary(wrc.lm)
wrc.k <- ggplot(k.df, aes(x = k, y = wrc)) +
  geom_point() +           # Scatterplot points
  geom_smooth(method = "loess", se = FALSE) 
wrc.k
wrc.maxev <- ggplot(k.df, aes(x = ,maxev, y = wrc)) +
  geom_point() +           # Scatterplot points
  geom_smooth(method = "loess", se = FALSE) 
wrc.maxev


predict.df$xwrc <- predict.lm(wrc.lm, newdata = predict.df)
