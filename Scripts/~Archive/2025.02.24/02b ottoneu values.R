
# Create scoring tibbles
on.hit.scoring <- tibble(
  cat = c('ab','h','2b','3b','hr','bb','hbp','sb','cs')
  , pts = c(-1,5.6,2.9,5.7,9.4,3,3,1.9,-2.8)
)

on.pitch.scoring <- tibble(
  cat = c('ip','k','h','bb','hbp','hr','sv','holds')
  , pts = c(7.4,2,-2.6,-3,-3,-12.3,5,4)
)

# Add positions
on.positions <- positions %>%
  filter(
    (!pos %in% c('SP','RP') & (g >= 10 | gs >= 5)) |
      (pos %in% c('SP','RP') & (g >= 5))
  ) %>%
  select(fg.id, pos)

# Create hitting table
hit.on <- hit.proj %>%
  # Do scoring calculations
  select(fg.id,weeks,g,ab,h,`2b`,`3b`,hr,bb,hbp,sb,cs) %>%
  pivot_longer(cols = ab:cs
               , names_to = 'cat') %>%
  left_join(on.hit.scoring, by = 'cat') %>%
  verify(!is.na(pts)) %>%
  mutate(total.pts = value * pts) %>%
  group_by(fg.id,weeks,g) %>%
  summarise(total.pts = sum(total.pts)) %>%
  ungroup() %>%
  # Add positions
  left_join(on.positions, by = 'fg.id')
