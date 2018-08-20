library(Hmisc)
library(tidyverse)
library(cowplot)
library(lubridate)


textSize = 10
plot_themes  = 	theme_classic() +
  theme(axis.ticks = element_line(size=.5, color = 'black'),
        axis.ticks.length = unit(-4,'pt'),
        axis.text.x=element_text(size = textSize, color='black', margin=margin(7,7,0,7,'pt')), 
        axis.text.y=element_text(size = textSize, color='black', margin=margin(7,7,7,0,'pt')),
        axis.title=element_text(size= textSize, color='black'),
        plot.title=element_text(size=textSize+2, color='black'),
        plot.margin=unit(c(9,9,9,9),'pt'),
        legend.title=element_text(size=textSize, color='black'),
        legend.text=element_text(size=textSize, color='black'),
        legend.position ='bottom',
        legend.direction='horizontal',
        legend.margin=margin(l = 8, unit='pt'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank())


get_ve = function(){
  
  ve = read_csv('data/ve_summary.csv') %>%
    mutate(hemisphere = ifelse(country == 'Canada', 'north', 'south')) %>%
    select(subtype, VE, country, hemisphere) %>%
    spread(subtype, VE) %>%
    dplyr::rename(h3ve = H3N2, bve = B, h1ve=H1N1)
  return(ve)
}
normalize = function(x){
  return(x/sum(x))
}
