library(Hmisc)
library(wpp2017)
library(cowplot)
library(lubridate)
library(fmsb)
library(MultinomialCI)
source('utils.R')
source('summarize_flunet.R')
source('colors.R')
library(tidyverse)

get_ili = function(){
  data = read_csv('../../surveillance_data/ili_summary.csv')
  data = assign_season(data) %>%
    group_by(country, season) %>%
    dplyr::summarise(flu_proxy = mean(flu_proxy_incidence, na.rm=T))
  return(data)
}

get_pop = function(){
  data(pop)
  pop_sizes = pop %>% mutate(name=as.character(name)) %>% 
    mutate(name = ifelse(name == 'Czechia', 'Czech Republic', name)) %>%
    filter(name %in% unique(data_orig$country)) %>%
    select(name, '2005','2010','2015') %>%
    gather(key = 'season', value = 'pop', -name) %>%
    group_by(season) %>%
    ungroup() %>%
    add_row(name = 'United States', season = 2005, pop = 1) %>%
    add_row(name = 'United States', season = 2010, pop = 1) %>%
    add_row(name = 'United States', season = 2015, pop = 1)
  
  names(pop_sizes) = c('country','census_year','pop')
  return(pop_sizes)
}


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
        # legend.position ='bottom',
        # legend.direction='horizontal',
        legend.margin=margin(l = 8, unit='pt'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank())

get_flunet = function(){
  data = read_csv('../data/flunet_seasonal.csv') %>%
    mutate(subtype = toupper(subtype)) %>%
    spread(subtype, counts) %>%
    mutate(total = B+H1N1+H3N2)
  return(data)
}

calculate_ci = function(a,b,c){
  out = multinomialCI(c(a,b,c), alpha = .05) %>%
    as.data.frame()
  names(out) = c('ci_low','ci_high')
  
  out = out %>% 
    mutate(subtype = c('h3n2','h1n1','b'))
  return(out)
}
#SEASONS = as.character(2006:2017)
SEASONS = as.character(2009:2016)

#south_timing = 'southlag'
south_timing = 'southlead'
#south_timing = NULL
summarize_flunet(south_timing)

exclude_countries = c('Malta', 'Luxembourg', "Netherlands")

data_orig = get_flunet() %>% 
  filter(!country %in% exclude_countries) %>%
  rename_all(tolower) %>% 
  filter(season %in% SEASONS) %>%
  gather(key = subtype, value = counts, b, h1n1, h3n2) %>%
  select(-total) %>%
  mutate(location = ifelse(!(country %in% c('Australia','United States', 'China', 'Canada')), 'Europe', NA)) %>%
  mutate(location = ifelse(country == 'United States', 'United States', location)) %>%
  filter(!is.na(location)) %>%
  arrange(country,season) %>%
  group_by(country,season) %>%
  filter(min(counts) > 5) %>% #at least 5 sequences for any given subtype per season
  mutate(ci_low = multinomialCI(counts, alpha = .05)[,1],
        ci_high = multinomialCI(counts, alpha = .05)[,2],
        freq = counts/sum(counts)) %>%
  ungroup() %>%
  mutate(err = ci_high-freq) 

countries_with_5seasons = data_orig %>%
  group_by(country, subtype) %>%
  summarise(nseasons = n()) %>%
  filter(nseasons >=5) %>%
  pull(country) %>% unique()

data_orig = data_orig %>% filter(country %in% countries_with_5seasons)

#seasonal data

#pop_weights = get_pop()

ili_data = get_ili() %>% filter(!country %in% exclude_countries) %>%
  mutate(census_year = round(season/5)*5) %>%
  merge(get_pop()) %>%
  mutate(location = ifelse(!(country %in% c('Australia','United States', 'China', 'Canada')), 'Europe', NA)) %>%
  mutate(location = ifelse(country == 'United States', 'United States', location)) %>%
  dplyr::group_by(location, season) %>%
  mutate(pop = pop/sum(pop), test = sum(season)) %>%
  dplyr::summarise(flu_proxy = sum(flu_proxy*pop, na.rm=T)) %>%
  ungroup() %>%
  group_by(location) %>%
  mutate(normalized_flu_proxy = normalize(flu_proxy)) %>%
  filter(season %in% SEASONS)

data = data_orig %>%
  mutate(census_year = round(season/5)*5) %>%
  merge(get_pop()) %>%
  group_by(location, season, subtype) %>%
  mutate(pop = pop/sum(pop)) %>%
  dplyr::summarise(freq = sum(freq*pop),
         err = sum(err^2*pop^2)^.5) %>%
  ungroup() 

flunet_eur_us = ggplot(data %>% mutate(subtype = toupper(subtype)), aes(x=str_c(season, '-',substr(as.numeric(season)+1,3,4)), y = freq, fill = subtype)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~location, scales = 'free_y') +
  scale_fill_brewer(palette='Dark2') +
  xlab('Season') + ylab('Counts') +
  theme(legend.position = 'bottom')
save_plot('../plots/flunet_eur_us_5seasons.pdf', flunet_eur_us, base_aspect_ratio=1.4)

ili_eur_us = ggplot(ili_data, aes(x=str_c(season, '-',substr(as.numeric(season)+1,3,4)), y = flu_proxy)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line(aes(group = location)) + 
  facet_wrap(~location, scales = 'free_y') +
  xlab('Season') + ylab('Influenza intensity') +
  theme(legend.position = 'bottom')
save_plot('../plots/ili_eur_us_5seasons.pdf', ili_eur_us, base_aspect_ratio=1.4)


data = data %>%  merge(ili_data) %>%
  group_by(location, subtype) %>%
  #summarise(freq = mean(freq),  err = sum(err^2)^.5) %>%
  summarise(freq = sum(freq*normalized_flu_proxy), err = sum((err*normalized_flu_proxy)^2)^.5) %>%
  ungroup() 

data_wide = data %>% mutate(err = round(err,5)) %>% spread(subtype, freq) %>%
  mutate(h3b = h3n2/b,
         h3h1 = h3n2/h1n1,
         h3b_err = (h3b^2*((err/h3n2)^2+(err/b)^2))^.5,
         h3h1_err = (h3h1^2*((err/h3n2)^2+(err/h1n1)^2))^.5)
         
data_wide = data_wide %>% mutate(h3b_low = h3b-h3b_err,
                                 h3b_high = h3b+h3b_err,
                                 h3h1_low = h3h1-h3h1_err,
                                 h3h1_high = h3h1+h3h1_err)

a = ggplot(data_wide, aes(x=location, y=h3b, fill = location)) + 
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin = h3b-h3b_err, ymax = h3b+h3b_err), width = .5) +
  scale_fill_brewer(palette = 'Dark2', guide = F) +
  xlab('Location') + ylab('Ratio H3N2:B') 

b = ggplot(data_wide, aes(x=location, y=h3h1, fill = location)) + 
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin = h3h1-h3h1_err, ymax = h3h1+h3h1_err), width = .5) +
  scale_fill_brewer(palette = 'Dark2', guide = F) +
  xlab('Location') + ylab('Ratio H3N2:H1N1')


expect_plot = ggplot(data.frame(x=c(1,3), y=c(.2,1)), aes(x=x, y=y)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme(axis.title = element_blank(), 
        axis.text=element_blank(), 
        axis.ticks = element_blank(),
        plot.margin=unit(c(10,10,5,46),'pt')) +
  xlim(c(0,4)) + ylim(c(0,1))


ratio_row = plot_grid(a, b, nrow = 1)#labels='AUTO', nrow=1)
expectation_row = plot_grid(expect_plot, expect_plot, nrow = 1, labels='AUTO')
ratio_plot = plot_grid(expectation_row, ratio_row, ncol=1, rel_heights = c(.25,1))

save_plot('../plots/us_europe_subtype.pdf',ratio_plot,base_aspect_ratio = 1.5)

