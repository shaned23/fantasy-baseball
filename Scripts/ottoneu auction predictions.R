
positions = c('C','1B','2B','3B','SS','OF','SP','RP','Util')

# ottoneu value regressions
auction.results <- read_sheet('1PKvHDoibKCL4ZsnNRrOrAjTyAl9TpQcnwW_JeHhyrSk'
                              , 'Auction')
past.projections <- read_sheet('1PKvHDoibKCL4ZsnNRrOrAjTyAl9TpQcnwW_JeHhyrSk'
                              , 'Projection')

# Combine them
auctiondf <- inner_join(auction.results,past.projections, by = join_by(PlayerID,Year)) %>%
  filter(Projection > 0) %>%
  # Add dummy values for positions
  mutate(`.C` = str_detect(Position,'C')
         , `.1B` = str_detect(Position,'1B')
         , `.2B` = str_detect(Position,'2B')
         , `.3B` = str_detect(Position,'3B')
         , `.SS` = str_detect(Position,'SS')
         , `.OF` = str_detect(Position,'OF')
         , `.SP` = str_detect(Position,'SP')
         , `.RP` = str_detect(Position,'RP')
         , `.Util` = str_detect(Position,'Util')
         , proj2 = Projection ^ 2
         , proj3 = Projection ^ 3)

auc.val1 <- lm(Auction ~ poly(Projection,3) + `.C` + `.1B` + `.2B` + `.3B` + `.SS` + `.OF` + `.SP` + `.RP` + `.Util`
               + Projection * `.C` + Projection * `.1B` + Projection * `.2B` + Projection * `.3B` + Projection * `.SS` + Projection * `.OF` + Projection * `.SP` + Projection * `.RP` + Projection * `.Util`
               + proj2 * `.C` + proj2 * `.1B` + proj2 * `.2B` + proj2 * `.3B` + proj2 * `.SS` + proj2 * `.OF` + proj2 * `.SP` + proj2 * `.RP` + proj2 * `.Util`
               + proj3 * `.C` + proj3 * `.1B` + proj3 * `.2B` + proj3 * `.3B` + proj3 * `.SS` + proj3 * `.OF` + proj3 * `.SP` + proj3 * `.RP` + proj3 * `.Util`, auctiondf)
summary(auc.val1)

auctiondf$Auction_hat1 <- predict(auc.val1)

auc.val2 <- lm(Auction ~ poly(Projection,2) + `.C` + `.1B` + `.2B` + `.3B` + `.SS` + `.OF` + `.SP` + `.RP` + `.Util`
               + Projection * `.C` + Projection * `.1B` + Projection * `.2B` + Projection * `.3B` + Projection * `.SS` + Projection * `.OF` + Projection * `.SP` + Projection * `.RP` + Projection * `.Util`
               + proj2 * `.C` + proj2 * `.1B` + proj2 * `.2B` + proj2 * `.3B` + proj2 * `.SS` + proj2 * `.OF` + proj2 * `.SP` + proj2 * `.RP` + proj2 * `.Util`
               , auctiondf)
summary(auc.val2)

auctiondf$Auction_hat2 <- predict(auc.val2)
