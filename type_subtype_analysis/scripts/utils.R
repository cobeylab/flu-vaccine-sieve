library(Hmisc)
library(tidyverse)
library(cowplot)
library(lubridate)

normalize = function(x){
  return(x/sum(x, na.rm=T))
}

get_vc = function(countries = NULL){
  #countries_tokeep = c('United States', 'China', 'Australia', 'Canada', 'Italy')
  vc = read_csv('../data/coverage.csv') %>%
    mutate(coverage = coverage/100) 
  
  if(!is.null(countries)){
    vc = vc %>%
    filter(country %in% countries)
  }
  
  return(vc)
}

get_ve = function(){
  
  ve = read_csv('../data/ve_summary.csv') %>%
    mutate(hemisphere = ifelse(country == 'Canada', 'north', 'south')) %>%
    select(subtype, VE, country, hemisphere) %>%
    spread(subtype, VE) %>%
    dplyr::rename(h3ve = H3N2, bve = B, h1ve=H1N1)
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
