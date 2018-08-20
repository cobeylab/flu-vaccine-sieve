library(tidyverse)
library(cowplot)
library(lubridate)

normalize = function(x){
  return(x/sum(x, na.rm=T))
}

assign_season = function(data){
  data = data %>% mutate(
    year= year(ymd(date)),
    season = ifelse(week(ymd(date)) >40, year, year-1)
  )
  return(data)
}

plot_survey = function(data, y, scale = 'fixed'){
  out = ggplot(data, aes_(x = ~date, y = as.name(y))) +
    #geom_point() +
    geom_line() + 
    facet_wrap(~country, scale = scale, nrow=1)
  return(out)
}

files = str_c('tidy_data/',str_c(c('australia', 'canada', 'eur', 'china', 'us'), '_weekly.csv'))
keep_countries = c("Australia",
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
                   "Austria","Slovakia","Hungary","Lithuania","Czechia")


data_orig = vector('list', length(files))
for(i in 1:length(files)){
  data_orig[[i]] = read_csv(files[i]) 
}
data_orig = do.call(rbind, data_orig)
data_orig = data_orig %>%  
  mutate(ili_cases= as.numeric(ili_cases),
         ili_incidence = as.numeric(ili_incidence),
         flu_positive = as.numeric(flu_positive),
         flu_proxy_incidence = as.numeric(flu_proxy_incidence)) %>%
  mutate(date = ymd(str_c(year,"0101")) + weeks(week)) %>%
  filter(date > ymd(20050101)) %>%
  filter(flu_positive < 1) 

data = data_orig %>%
  assign_season() %>%
  filter(country %in% keep_countries)

data = data%>% mutate(country = ifelse(country == 'United Kingdom of Great Britain and Northern Ireland', 'United Kingdom', country))

data$ili_incidence[data$country == 'Slovakia' & data$ili_incidence > 1] = NA
data$ili_incidence[data$country == 'United Kingdom' & data$ili_incidence > .02] = NA

data = data %>% mutate(flu_proxy_incidence = ili_incidence * flu_positive)
#write_csv(data, 'ili_summary.csv')

x=read_csv('tidy_data/eur_weekly.csv')

countries = unique(data$country)
years = unique(data$year)
weeks = unique(data$week)
datespan = expand.grid(country=countries, week = weeks, year = years)

data = merge(data,datespan, all=T) %>%
  arrange(country, year, week) %>%
  mutate(date_orig=date) %>%
  mutate(date = ymd(str_c(year,'0101')) + weeks(week))
  #mutate(flu_positive = ifelse(is.na(ili_cases), ili_cases, flu_positive))

ili_incplot = ggplot(data, aes(x=ymd(date), y = ili_incidence)) +
                     #aes(x=str_c(season, '-',substr(as.numeric(season)+1,3,4)), y= normalized_flu_proxy)) + 
  geom_line(aes(group = country)) +
  facet_wrap(~country, nrow = 8, ncol=4, scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab('Date') + ylab('ILI or ARI incidence')
  

#test = data %>% select(country, flu_positive,date,date2) 
#View(test)
#ggplot(test, aes(x=ymd(date), y=flu_positive)) + geom_line() + geom_point()

flu_posplot = ggplot(data, aes(x=ymd(date), y = flu_positive)) +
  #aes(x=str_c(season, '-',substr(as.numeric(season)+1,3,4)), y= normalized_flu_proxy)) + 
  geom_line(aes(group = country)) +
  facet_wrap(~country, nrow = 8, ncol=4, scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab('Date') + ylab('Fraction of influenza positive respiratory samples') 

flu_intensity = ggplot(data, aes(x=ymd(date), y = flu_proxy_incidence)) +
  #aes(x=str_c(season, '-',substr(as.numeric(season)+1,3,4)), y= normalized_flu_proxy)) + 
  geom_line(aes(group = country)) +
  facet_wrap(~country, nrow = 8, ncol=4, scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab('Date') + ylab(expression(atop(Influenza~intensity,ILI%*%fraction~influenza~positive)))

save_plot('plots/ili.pdf', ili_incplot, base_height=11, base_aspect_ratio=1)
save_plot('plots/flu_positive.pdf', flu_posplot, base_height=11, base_aspect_ratio=1)
save_plot('plots/flu_intensity.pdf', flu_intensity, base_height=11, base_aspect_ratio=1)

