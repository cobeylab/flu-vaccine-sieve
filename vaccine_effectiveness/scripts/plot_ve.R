library(tidyverse)
library(cowplot)

textSize = 12
plot_themes  =	theme_classic() +
  theme(axis.ticks = element_line(size=.5, color = 'black'),
        axis.ticks.length = unit(-4,'pt'),
        axis.text.x=element_text(size = textSize, color='black', margin=margin(7,7,0,7,'pt')), 
        axis.text.y=element_text(size = textSize, color='black', margin=margin(7,7,7,0,'pt')),
        axis.title=element_text(size= textSize, color='black'),
        plot.title=element_text(size=textSize+2, color='black'),
        plot.margin=unit(c(9,9,9,9),'pt'),
        legend.margin=margin(l = 8, unit='pt'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank()
  )

format_season = function(x){
  x = x %>%
    mutate(season = str_c(substr(season,1,4), '-', season+1))
  return(x)
}


calc_ve_means = function(data){
  out = data %>%
    filter(!is.na(VE)) %>%
    mutate(VE = VE/100, lowerCI = lowerCI/100, upperCI = upperCI/100) %>%
    mutate(logVE = log(1-VE),
           logL = log(1-lowerCI),
           logU = log(1-upperCI)) %>%
    group_by(subtype) %>%
    summarise(VEbar = mean(VE, na.rm =T),
              n=n(),
              lowerCI = VEbar - sum((lowerCI - VE)^2, na.rm=T)^.5/n,
              upperCI = VEbar + sum((upperCI - VE)^2, na.rm=T)^.5/n,
              logVEbar = (mean(logVE, na.rm =T)),
              logL = logVEbar + (sum((logL - logVE)^2, na.rm=T)^.5/n),
              logU = logVEbar - (sum((logU - logVE)^2, na.rm=T)^.5/n) ) %>%
    mutate(logVEbar = 1-exp(logVEbar),
           logL = 1-exp(logL),
           logU = 1-exp(logU))
  return(out)
}

cadata = read_csv('../data/ve_summary_revised.csv')  %>%
  filter(country == 'Canada') %>%
  format_season %>%
  filter(interpolated == 'FALSE') %>%
  select(season,subtype, VE, lowerCI, upperCI) %>% mutate(location = 'Canada')
eurdata = read_csv('../data/eur_ve.csv') %>%   select(season,subtype, VE, lowerCI, upperCI) %>% mutate(location = 'Europe')
usdata = read_csv('../data/us_ve.csv') %>%   select(season,subtype, VE, lowerCI, upperCI) %>% mutate(location = 'United States')
audata =  read_csv('../data/auve.csv')  %>% select(season,subtype, VE, lowerCI, upperCI) %>% mutate(location = 'Australia')

allvedata = rbind(cadata,eurdata,usdata,audata) %>%
  filter(as.numeric(substr(season,1,4)) > 2008) %>%
  mutate(lowerCI = as.numeric(lowerCI))

ve_ca_plot = ggplot(cadata %>% filter(!is.na(VE), season !='2005-06') , aes(x=season, y= VE, color = subtype)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI),position = position_dodge(width = 0.5), width = .2) +
  geom_line(aes(group = subtype), position = position_dodge(width = 0.5), alpha = .5) +
  #geom_hline(data=means, aes(yintercept = VE)) +
   plot_themes + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_color_brewer(palette = "Dark2",  name = 'Type/subtype') 

save_plot('../plots/ve_canada.pdf',ve_ca_plot, base_aspect_ratio = 1.4)

ve_plot = ggplot(allvedata %>% filter(!is.na(VE)) , aes(x=season, y= VE, color = subtype)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI),position = position_dodge(width = 0.5), width = .2) +
  geom_line(aes(group = subtype), position = position_dodge(width = 0.5), alpha = .5) +
  facet_wrap(~location, nrow = 1, scale = 'free_x') +
  plot_themes + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_color_brewer(palette = "Dark2",  name = 'Type/subtype') +
  coord_cartesian(ylim = c(-100,100))  +
  xlab('Season')

save_plot('../plots/ve.pdf',ve_plot, base_aspect_ratio =3, base_height=3.5)

ca_vemeans = read_csv('../data/ve_summary_revised.csv')  %>% 
  filter(country == "Canada", as.numeric(substr(season, 1,4))> 2008, interpolated == FALSE) %>%
  calc_ve_means() %>%
  mutate(location = "Canada")
            
us_vemeans = read_csv('../data/us_VE.csv') %>%
  filter(as.numeric(substr(season, 1,4))> 2008) %>%  
  calc_ve_means() %>%
  mutate(location = "United States")

au_vemeans = read_csv('../data/auve.csv') %>%
  mutate(lowerCI = as.numeric(lowerCI)) %>%
  filter(as.numeric(season)> 2008) %>%
  calc_ve_means() %>%
  mutate(location = "Australia")

eur_vemeans = read_csv('../data/eur_ve.csv') %>%
  mutate(lowerCI = as.numeric(lowerCI)) %>%
  calc_ve_means() %>%
  mutate(location = "Europe")


all_vemeans = rbind(ca_vemeans, us_vemeans, au_vemeans, eur_vemeans)
vemeans = ggplot(all_vemeans, aes(x = location, y = logVEbar*100, color = subtype)) +
  geom_point(position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin = logL*100, ymax = logU*100),position = position_dodge(width = .5), width=.5) +
  scale_color_brewer(palette = "Dark2",  name = 'Type/subtype') +
  geom_text(aes(label = n, y=90), position = position_dodge(width = .5), show.legend = FALSE) +
  ylim(-10, 100) +
  xlab('Location') + ylab('Average VE') + plot_themes  
save_plot('../plots/ve_means.pdf',vemeans, base_aspect_ratio =3, base_height = 2.5)

calc_vf_means_byseason = function(data){
  out = data %>%
    filter(!is.na(VE)) %>%
    mutate(VE = VE/100, lowerCI = lowerCI/100, upperCI = upperCI/100) %>%
    mutate(logVE = log(1-VE),
           logL = log(1-lowerCI),
           logU = log(1-upperCI)) %>%
    group_by(subtype, season) %>%
    summarise(VEbar = mean(VE, na.rm =T),
              n=n(),
              lowerCI = VEbar - sum((lowerCI - VE)^2, na.rm=T)^.5/n,
              upperCI = VEbar + sum((upperCI - VE)^2, na.rm=T)^.5/n,
              logVEbar = (mean(logVE, na.rm =T)),
              logL = logVEbar + (sum((logL - logVE)^2, na.rm=T)^.5/n),
              logU = logVEbar - (sum((logU - logVE)^2, na.rm=T)^.5/n) ) %>%
    mutate(logVEbar = 1-exp(logVEbar),
           logL = 1-exp(logL),
           logU = 1-exp(logU))
  return(out)
}


calc_vf_means = function(data){
  out = data %>%
    select(subtype, season, logVEbar, logL, logU) %>%
    rename(VE = logVEbar, lowerCI = logL, upperCI = logU) %>%
    mutate(logVE = log(1-VE),
           logL = log(1-lowerCI),
           logU = log(1-upperCI)) %>%
    group_by(subtype) %>%
    summarise(n=n(),
              logVEbar = (mean(logVE, na.rm =T)),
              logL = logVEbar + (sum((logL - logVE)^2, na.rm=T)^.5/n),
              logU = logVEbar - (sum((logU - logVE)^2, na.rm=T)^.5/n) ) %>%
    mutate(logVEbar = 1-exp(logVEbar),
           logL = 1-exp(logL),
           logU = 1-exp(logU))
  return(out)
}
vfmeans = read_csv('../data/vf.csv') %>%
  mutate(lowerCI = as.numeric(lowerCI)) %>%
  calc_vf_means_byseason() 
vfmean_summ = vfmeans %>% calc_vf_means()

ve_summary = rbind(ca_vemeans %>% mutate(country='Canada'), au_vemeans %>% mutate(country='Australia')) %>%
  select(subtype, logVEbar, logL, logU, country) %>%
  rename(VE = logVEbar,
         lowerCI = logL,
         upperCI = logU)
write_csv(ve_summary,'../data/ve_summary.csv')
