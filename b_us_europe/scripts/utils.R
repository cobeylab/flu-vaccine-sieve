library(Hmisc)
library(tidyverse)
library(cowplot)
library(lubridate)

normalize = function(x){
  return(x/sum(x, na.rm=T))
}

get_vc = function(countries = NULL){
  #countries_tokeep = c('United States', 'China', 'Australia', 'Canada', 'Italy')
  vc = read_csv('coverage.csv') %>%
    mutate(coverage = coverage/100) 
  
  if(!is.null(countries)){
    vc = vc %>%
    filter(country %in% countries)
  }
  
  return(vc)
}

get_ve = function(){
  
  ve = read_csv('ve_summary.csv') %>%
    mutate(VE = VE/100, lowerCI = as.numeric(lowerCI)/100, upperCI = upperCI/100) %>%
    select(country, VE, subtype, season, interpolated) %>%
    filter(country %in% c('Canada', 'Australia')) %>%
    group_by(country, season) %>%
    spread(subtype, VE) %>%
    ungroup() %>%
    rename(h3ve = H3N2, h1ve = H1N1, bve=B) %>%
    mutate(hemisphere = ifelse(country == 'Canada', 'north', 'south')) %>%
    select(season, h3ve, h1ve, bve, interpolated, hemisphere)
  return(ve)
}


assign_date = function(data){
  data = data %>%
    mutate(date = case_when(
      nchar(collection_date) == 7 ~ (ymd(str_c(collection_date, '-01')) + 
                                       round(monthDays(ymd(str_c(collection_date, '-01'))) * runif(1))) %>% as.character(),
      nchar(collection_date) == 4 ~ (ymd(str_c(collection_date, '-01-01')) + sample(364, 1)) %>% as.character(),
      nchar(collection_date) == 10 ~ collection_date
    ),
    week = week(ymd(date))
    )
  return(data)
  
}

assign_season = function(data){
  data = data %>% mutate(
    year= year(ymd(date)),
    season = ifelse(week(ymd(date)) >40, year, year-1)
  )
  return(data)
}


tidy_data = function(data){
  dat = data %>%
    select(isolate_id, isolate_name, subtype, lineage, location, collection_date, host_age) %>%
    filter(subtype %in% c('A / H3N2', 'B', 'A / H1N1')) %>%
    separate(location, into = c('region', 'country','locale'), sep=' / ') %>%
    mutate(
      subtype = 
        case_when(subtype == 'A / H3N2'  ~ 'H3N2',
                  subtype == 'A / H1N1'  ~ 'H1N1',
                  subtype == 'B' ~'B')
    )
  return(dat)
}

get_data = function(files){
  dat = vector('list', length(files))
  for(i in 1:length(files)){
    print(files[i])
    dat[[i]] = read.csv(files[i], stringsAsFactors = FALSE)
  }
  out = do.call('rbind', dat) %>%
    rename_all(tolower)
  return(out)
}

get_ili = function(){
  data = read_csv('data/ili_summary.csv')
  data = assign_season(data) %>%
    group_by(country, season) %>%
    dplyr::summarise(flu_proxy = mean(flu_proxy_incidence, na.rm=T))
  return(data)
}


get_pop = function(){
  keep_countries = c("Austria","Belgium",
                     "Croatia","Czechia",
                     "Denmark","Estonia",
                     "Finland","France",
                     "Germany","Greece",
                     "Hungary","Iceland",
                     "Ireland","Italy",
                     "Latvia","Lithuania",
                     "Luxembourg","Netherlands",
                     "Norway","Poland",
                     "Portugal","Romania",
                     "Slovakia","Slovenia",
                     "Spain","Sweden",
                     "United Kingdom of Great Britain and Northern Ireland”,”United States")
  data(pop)
  pop_sizes = pop %>% mutate(name=as.character(name)) %>% 
    mutate(name = ifelse(name == 'Czechia', 'Czech Republic', name)) %>%
    filter(name %in% keep_countries) %>%
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

