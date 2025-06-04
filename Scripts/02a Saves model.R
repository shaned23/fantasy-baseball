
# Error matrix
error.mat <- tibble(model = character()
                    , sv.rsq = numeric()
                    , hld.rsq = numeric()
                    , sv.hld.rsq = numeric()
                    )

# Build random forest
set.seed(23875)

# Saves then holds
sv1 <- randomForest(sv ~ ., data = select(df.train,-c(sv.pct,sv.per.ip,hld.per.ip,sv.hld,hld)))
hld1 <- randomForest(hld ~ ., data = select(df.train,-c(sv.pct,sv.per.ip,hld.per.ip,sv.hld)))
svhld1 <- df.test %>%
  mutate(sv = predict(sv1, df.test)
         , hld = predict(hld1, df.test)
         , y.hat = sv + hld)

error.mat <- bind_rows(error.mat,
              tibble(model = 'saves, then holds'
                , sv.rsq = rfr2(sv1, df.test, 'sv')
                , hld.rsq = rfr2(hld1, df.test, 'hld')
                , sv.hld.rsq = rfr2(testdata = svhld1, yvar = 'sv.hld', predict = FALSE)
              ))

# Holds then saves
hld2 <- randomForest(hld ~ ., data = select(df.train,-c(sv.pct,sv.per.ip,hld.per.ip,sv.hld,sv)))
sv2 <- randomForest(sv ~ ., data = select(df.train,-c(sv.pct,sv.per.ip,hld.per.ip, sv.hld)))
svhld2 <- df.test %>%
  mutate(sv = predict(sv2, df.test)
         , hld = predict(hld2, df.test)
         , y.hat = sv + hld)

error.mat <- bind_rows(error.mat,
                       tibble(model = 'holds, then saves'
                              , sv.rsq = rfr2(sv2, df.test, 'sv')
                              , hld.rsq = rfr2(hld2, df.test, 'hld')
                              , sv.hld.rsq = rfr2(testdata = svhld2, yvar = 'sv.hld', predict = FALSE)
                       ))

# Saves and holds then save percentage
svhld3 <- randomForest(sv.hld ~ . - sv - hld - sv.pct, select(df.train.nona, -ends_with('per.ip')))
svpct3 <- randomForest(sv.pct ~ . - sv - hld, select(df.train.nona, -ends_with('per.ip')))
sv3 <- df.test.nona %>%
  mutate(sv.hld = predict(svhld3, df.test.nona)
         , sv.pct = predict(svpct3, df.test.nona)
         , y.hat = sv.hld * sv.pct)
hld3 <- df.test.nona %>%
  mutate(sv.hld = predict(svhld3, df.test.nona)
         , sv.pct = predict(svpct3, df.test.nona)
         , y.hat = sv.hld * (1 - sv.pct))

error.mat <- bind_rows(error.mat,
                       tibble(model = 'SV+HLD then save%'
                              , sv.rsq = rfr2(testdata = sv3, yvar = 'sv', predict = FALSE)
                              , hld.rsq = rfr2(testdata = hld3, yvar = 'hld', predict = FALSE)
                              , sv.hld.rsq = rfr2(model = svhld3, testdata = df.test.nona, yvar = 'sv.hld')
                       ))


# Saves/IP and Holds/IP
svip4 <- randomForest(sv.per.ip ~ era + fip + perc.k + perc.bb + hr.9 + winpct + rpg.pct, na.omit(select(df.train,-sv.pct)))
hldip4 <- randomForest(hld.per.ip ~ era + fip + perc.k + perc.bb + hr.9 + winpct + rpg.pct + sv.per.ip, na.omit(select(df.train,-sv.pct)))
sv4 <- df.test %>%
  mutate(sv.per.ip = predict(svip4, df.test)
         , y.hat = sv.per.ip * ip)
hld4 <- df.test %>%
  mutate(hld.per.ip = predict(hldip4, df.test)
         , y.hat = hld.per.ip * ip)
svhld4 <- df.test %>%
  mutate(sv.per.ip = predict(svip4, df.test)
         , hld.per.ip = predict(hldip4, df.test)
         , y.hat = (sv.per.ip + hld.per.ip) * ip)

error.mat <- bind_rows(error.mat,
                       tibble(model = 'By per IP, saves then holds'
                              , sv.rsq = rfr2(testdata = sv4, yvar = 'sv', predict = FALSE)
                              , hld.rsq = rfr2(testdata = hld4, yvar = 'hld', predict = FALSE)
                              , sv.hld.rsq = rfr2(testdata = svhld4, yvar = 'sv.hld', predict = FALSE)
                       ))

# Holds/IP then Saves/IP
hldip5 <- randomForest(hld.per.ip ~ era + fip + perc.k + perc.bb + hr.9 + winpct + rpg.pct, na.omit(select(df.train,-sv.pct)))
svip5 <- randomForest(sv.per.ip ~ era + fip + perc.k + perc.bb + hr.9 + winpct + rpg.pct + hld.per.ip, na.omit(select(df.train,-sv.pct)))
sv5 <- df.test %>%
  mutate(sv.per.ip = predict(svip5, df.test)
         , y.hat = sv.per.ip * ip)
hld5 <- df.test %>%
  mutate(hld.per.ip = predict(hldip5, df.test)
         , y.hat = hld.per.ip * ip)
svhld5 <- df.test %>%
  mutate(sv.per.ip = predict(svip5, df.test)
         , hld.per.ip = predict(hldip5, df.test)
         , y.hat = (sv.per.ip + hld.per.ip) * ip)

error.mat <- bind_rows(error.mat,
                       tibble(model = 'By per IP, holds then saves'
                              , sv.rsq = rfr2(testdata = sv5, yvar = 'sv', predict = FALSE)
                              , hld.rsq = rfr2(testdata = hld5, yvar = 'hld', predict = FALSE)
                              , sv.hld.rsq = rfr2(testdata = svhld5, yvar = 'sv.hld', predict = FALSE)
                       ))

error.mat <- error.mat %>%
  mutate(sumrsq = sv.rsq + hld.rsq + sv.hld.rsq) %>%
  arrange(-sumrsq)

print(error.mat)
# sv2 and hld2 winners
print(sv2$importance)
print(hld2$importance)
# w, l, era, g, gs, ip, fip, perc.k, perc.bb, hr.9, winpct, rpg.pct

reg1 <- rpart(sv ~ ., data = select(df.train,-c(sv.pct,sv.per.ip,hld.per.ip,sv.hld,hld)))
reg2 <- rpart(hld ~ ., data = select(df.train,-c(sv.pct,sv.per.ip,hld.per.ip,sv.hld)))
plot(reg1, uniform = TRUE, main= "Saves")
text(reg1,use.n=TRUE, all=TRUE,cex=0.8)
summary(reg1)

plot(reg2, uniform = TRUE, main= "Holds")
text(reg2,use.n=TRUE, all=TRUE,cex=0.8)
summary(reg2)
