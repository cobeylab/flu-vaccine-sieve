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
        #legend.margin=margin(l = 8, unit='pt'),
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

SEASONS = as.character(2006:2016)

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
  arrange(country,season) %>%
  group_by(country,season) %>%
  filter(min(counts) > 5) %>% #at least 5 sequences for any given subtype per season
  mutate(ci_low = multinomialCI(counts, alpha = .05)[,1],
         ci_high = multinomialCI(counts, alpha = .05)[,2],
         freq = counts/sum(counts)) %>%
  ungroup() %>%
  mutate(err = ci_high-freq) 

flunet_data = data_orig %>% filter(country %in% c("Australia",
                                                  "China",
                                                  "Germany",
                                                  "Italy",
                                                  "Norway",
                                                  "Romania",
                                                  "United Kingdom of Great Britain and Northern Ireland",
                                                  "United States","Canada",
                                                  "Denmark",
                                                  "Finland",
                                                  "Iceland",
                                                  "Spain",
                                                  "Belgium",
                                                  "Croatia","Estonia","France","Greece","Ireland","Latvia","Poland","Portugal","Slovenia","Sweden",
                                                  "Austria","Slovakia","Hungary","Lithuania")) %>% 
  mutate(country = ifelse(country == 'United Kingdom of Great Britain and Northern Ireland', 'United Kingdom', country))

flunet_plot = ggplot(flunet_data %>% mutate(subtype = toupper(subtype)), aes(x=str_c(season, '-',substr(as.numeric(season)+1,3,4)), y = counts, fill = subtype)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~country,nrow=7,ncol=4, scales = 'free_y') +
  scale_fill_brewer(palette='Dark2') +
  xlab('Season') + ylab('Counts') +
  theme(legend.position = 'bottom')
  
save_plot('../plots/flunet_summary.pdf', flunet_plot, base_height=12, base_aspect_ratio=1)

ili_data = get_ili() %>% filter(!country %in% exclude_countries, season %in% SEASONS) %>%
  group_by(country) %>%
  mutate(normalized_flu_proxy = normalize(flu_proxy)) %>% 
  ungroup()

data_weighted = merge(flunet_data, ili_data) %>%
  mutate(freq = freq*normalized_flu_proxy)

weighted_countries = c('Australia','France','Italy','Latvia','Lithuania','Norway','Poland','Slovenia','Portugal','United States','Romania','Slovakia','Canada','Germany','China','Hungary','Estonia','Iceland')
length(weighted_countries)
flunet_weighted_plot = ggplot(data_weighted %>% filter(country %in% weighted_countries) %>% mutate(subtype = toupper(subtype)), 
                              aes(x=str_c(season, '-',substr(as.numeric(season)+1,3,4)), y = freq, fill = subtype)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~country,nrow=6,ncol=3, scales = 'free_y') +
  scale_fill_brewer(palette='Dark2') +
  xlab('Season') + ylab('Influenza intensity-weighted frequency') +
  theme(legend.position = 'bottom')

save_plot('../plots/flunet_season_weighted.pdf', flunet_weighted_plot, base_height = 8, base_aspect_ratio=1.2)

data = merge(data_orig, ili_data) %>%
  dplyr::group_by(country,subtype) %>%
  summarise(freq = sum(freq*normalized_flu_proxy,na.rm=T),
         err = sum((err*normalized_flu_proxy)^2, na.rm=T)^.5) %>%
  ungroup() %>%
  mutate(err = round(err,5)) %>%
  spread(subtype,freq) %>%
  mutate(h3b = h3n2/b,
         h3h1 = h3n2/h1n1,
         h3b_err = (h3b^2*((err/h3n2)^2+(err/b)^2))^.5,
         h3h1_err = (h3h1^2*((err/h3n2)^2+(err/h1n1)^2))^.5,
         total = b+h1n1+h3n2) %>%
  filter(total>0)


ve_n = get_ve() %>% filter(country == 'Canada')
h3ve = ve_n %>% pull(h3ve)
h1ve = ve_n %>% pull(h1ve)
bve = ve_n %>% pull(bve)

vc = get_vc() %>%
  group_by(country) %>%
  summarise(coverage = mean(coverage)) %>%
  ungroup()

data = merge(data,vc)
unique(data$country)
#plotting utils
base_shapes = c(16,17,15,3,7,8)
shapes = rep(base_shapes, ceiling(length(unique(data$country))/length(base_shapes)))[1:length(unique(data$country))]

h3b_fun = function(x) log((1-h3ve*x)/(1-bve*x))
h3h1_fun = function(x) log((1-h3ve*x)/(1-h1ve*x))

a = ggplot(data, aes(x=coverage, y=log(h3b))) + 
  geom_point(aes(color=country, shape = country), size = 2) +
  geom_errorbar(aes(ymin = log(h3b - h3b_err), ymax = log(h3b + h3b_err), color = country)) +
  geom_smooth(method = 'lm', color = 'black', size=.5) +
  ylab('Log ratio H3N2/B') +
  xlab('Average seasonal vaccine coverage') +
  scale_color_tencol(guide = guide_legend(title.position = 'top')) + 
  plot_themes  +
  #annotate("text", label=str_c('r = ',round(h3b_test$estimate, 2),' p = ', round(h3b_test$p.value, 2)), x=0.15, y=1.5,hjust=1, size=3, vjust=1) +
  scale_shape_manual(values = shapes) + 
  stat_function(fun = h3b_fun, color = 'red')  +
  geom_hline(yintercept = 0, lty=2, alpha = .5)

b =  ggplot(data, aes(x=coverage, y=log(h3h1))) + 
  geom_point(aes(color = country, shape = country), size=2) +
  geom_errorbar(aes(ymin = log(h3h1 - h3h1_err), ymax = log(h3h1 + h3h1_err), color = country)) +
  geom_smooth(method = 'lm', color = 'black', size=.5) +
  ylab('Log ratio H3N2/H1N1') +
  xlab('Average seasonal vaccine coverage') +
  #scale_color_brewer(palette='Set1', guide = guide_legend(title.position = 'top')) + 
  scale_color_tencol(guide = guide_legend(title.position = 'top')) + 
  plot_themes + 
  #annotate("text", label=str_c('r = ',round(h3h1_test$estimate, 2),' p = ', round(h3h1_test$p.value, 2)),x = .25, y=2,hjust=1, size=3, vjust=1) +
  scale_shape_manual(values = shapes) +
  stat_function(fun = h3h1_fun, color = 'red') +
  geom_hline(yintercept = 0, lty=2, alpha = .5)

coverage_ratio = plot_grid(a + theme(legend.position = 'none'), b + theme(legend.position = 'none'), 
                           labels='AUTO', 
                           nrow=1,
                           align = 'vh', 
                           hjust = -1)
legend = get_legend(a + theme(legend.position="bottom"))
out = plot_grid(coverage_ratio, legend, ncol=1, rel_heights = c(1,.4))

save_plot(str_c('../plots/coverage_ratio.pdf'),out, base_aspect_ratio = 1.5, base_height = 4)

h3b_test = cor.test(data$coverage, data$h3n2/data$b, method = 'pearson')
h3h1_test = cor.test(data$coverage, data$h3n2/data$h3h1, method = 'pearson')

data =  data %>% mutate(h3b_hat = (1-h3ve*coverage)/(1-bve*coverage),
                        h3h1_hat = (1-h3ve*coverage)/(1-h1ve*coverage))
a = ggplot(data, aes(x=log(h3b_hat), y=log(h3b))) + 
  geom_point(aes(color=country, shape = country), size = 3) +
  geom_errorbar(aes(ymin = log(h3b - h3b_err), ymax = log(h3b + h3b_err), color = country)) +
  geom_smooth(method = 'lm', color = 'black', size=.5) +
  ylab('Log ratio H3N2/B') +
  xlab(expression(atop(Log~ratio~effective~unvaccinated~fraction, (1-italic(p)*italic(E)[H3N2])/(1-italic(p)*italic(E)[B])))) +
  scale_color_tencol(guide = guide_legend(title.position = 'top')) + 
  plot_themes  +
  #annotate("text", label=str_c('r = ',round(h3b_test$estimate, 2),' p = ', round(h3b_test$p.value, 2)), x=0.04, y=1.2,hjust=1, size=3, vjust=1) +
  scale_shape_manual(values = shapes) + 
  stat_function(fun = function(x) x, color = 'red') +
  geom_hline(yintercept = 0, lty=2, alpha = .5)

b =ggplot(data, aes(x=log(h3h1_hat), y=log(h3h1))) + 
  geom_point(aes(color = country, shape = country), size=3) +
  geom_errorbar(aes(ymin = log(h3h1 - h3h1_err), ymax = log(h3h1 + h3h1_err), color = country)) +
  geom_smooth(method = 'lm', color = 'black', size=.5) +
  ylab('Log ratio H3N2/H1N1') +
  xlab(expression(atop(Log~ratio~effective~unvaccinated~fraction, (1-italic(p)*italic(E)[H3N2])/(1-italic(p)*italic(E)[H1N1])))) +
  scale_color_tencol(guide = guide_legend(title.position = 'top')) + 
  plot_themes + 
  #annotate("text", label=str_c('r = ',round(h3h1_test$estimate, 2),' p = ', round(h3h1_test$p.value, 2)),x=.06,y=2,hjust=1, size=3, vjust=1) +
  scale_shape_manual(values = shapes) +
  stat_function(fun = function(x) x, color = 'red') +
  geom_hline(yintercept = 0, lty=2, alpha = .5)

expectation_ratio = plot_grid(a + theme(legend.position = 'none'), b + theme(legend.position = 'none'), 
                           labels='AUTO', 
                           nrow=1,
                           align = 'vh', 
                           hjust = -1)

legend = get_legend(a + theme(legend.position="bottom"))
out = plot_grid(expectation_ratio, legend, ncol=1, rel_heights = c(1,.4))

save_plot('../plots/expectation_ratio.pdf',out, base_aspect_ratio = 1.5, base_height=4)

h3b_test = cor.test(data$h3b_hat, (data$h3n2/data$b), method = 'pearson')
h3h1_test = cor.test(data$h3h1_hat, (data$h3n2/data$h3h1), method = 'pearson')
