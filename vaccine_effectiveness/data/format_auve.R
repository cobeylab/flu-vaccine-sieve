ve = read_csv('~/Downloads/auve.csv') %>%
  mutate(interpolated = FALSE) %>%
  select(season,subtype,VE,lowerCI,upperCI,interpolated,country) 

write_csv(ve,'au_ve_formatted.csv')
