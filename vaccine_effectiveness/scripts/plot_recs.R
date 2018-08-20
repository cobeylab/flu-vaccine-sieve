library(tidyverse)
library(forcats)
library(cowplot)
setwd('../')

recs =read_csv('data/flu_recommend.csv') %>%
  gather(key = 'age', value = 'recommended',-Country, -season) %>%
  mutate(age = factor(age, levels = c("0.5", "2"  , "4"  , "5"  , "11"  ,"12"  ,"18"  ,"50"  ,"55" , "60"  ,"65" ))) 

recs= recs%>%
  mutate(recommended = ifelse(is.na(recommended), 'No', 'Yes'))

recplot = ggplot(recs, aes(x=(age), y=season, fill=recommended)) + 
  geom_tile()+ 
  facet_grid(Country~.) +theme(strip.text.y = element_text(angle = 360)) +
  scale_fill_manual(values = c("grey", 'red')) +
  theme(panel.spacing = unit(0, "lines")) +
  ylab('Season') + xlab('Age')

save_plot('plots/vaccine_recs.pdf', recplot, base_height=12, base_aspect_ratio = .9)
