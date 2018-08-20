library(tidyverse)
library(cowplot)

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
        # legend.position ='bottom',
        # legend.direction='horizontal',
        legend.margin=margin(l = 8, unit='pt'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank())

get_data = function(file, this_season){
  data = read_csv(file)
  names(data) = gsub( '/', '_',names(data))
  
  this_season = '2014-2015'
  strains = data %>% 
    filter(season == this_season) 
  strains$region_labels = gsub('_',' ',strains$region) %>% stri_trans_totitle()
  strains = strains %>% filter(  region_labels %in% c('North America','Europe'))
  return(strains)
}

makeplot = function(data, xlabel){
  plot = 
    ggplot(data, aes(x=A_Texas_50_2012, fill = region_labels)) +
    geom_histogram(bins = 20) + 
    facet_wrap(~region_labels) +
    scale_fill_brewer(palette='Dark2', guide=F) +
    ylab('Number of \nviruses in 2014-2015') + 
    xlab(xlabel)
  
  return(plot)
}

hi_data = get_data('../data/hi_h3n2_ha_12y_vaccine_antigenic_distances.csv')
ep_data = get_data('../data/ep_h3n2_ha_12y_vaccine_antigenic_distances.csv')

a = makeplot(hi_data,"HI antigenic distance to\nA/Texas/50/2012")
b = makeplot(ep_data,"Epitope antigenic distance to\nA/Texas/50/2012")

out = plot_grid(a,b, nrow = 2, labels = 'AUTO')
save_plot('../plots/h3_distance.pdf', out, base_aspect_ratio =.7, base_height=5 )

t.test(hi_data$A_Texas_50_2012[hi_data$region == 'europe'], hi_data$A_Texas_50_2012[hi_data$region == 'north_america'])
t.test(ep_data$A_Texas_50_2012[ep_data$region == 'europe'], ep_data$A_Texas_50_2012[ep_data$region == 'north_america'])
