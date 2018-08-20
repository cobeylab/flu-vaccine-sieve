library(tidyverse)
library(cowplot)
library(lubridate)

assign_season = function(data, south = NULL){
  data = data %>%
    mutate(
      season = ifelse(week >40, year, year-1)
    )
  
  if(is.null(south)){
    return(data)
  }

  if(south == 'southlead'){
    print('southern hemisphere leading')
    data = data %>%
      mutate(
        season = ifelse(country == 'Australia',
                        ifelse(week >18, year, year-1),
                        season
        )
      )
  }
  
  if(south == 'southlag'){
    print('southern hemisphere lagging')
    data = data %>%
      mutate(
        season = ifelse(country == 'Australia',
                        ifelse(week >18, year+1, year),
                        season
        )
      )
  }
  
  return(data)
}

summarize_flunet = function(south_timing){
  file_list = dir(path = '../surveillance_data/flunet_samples/', pattern = '*.csv')
  file_list = file_list[file_list!='europe_flunet.csv']
  #file_list = c('australia', 'canada', 'china','us','germany','portugal','france','romania','poland','italy')
  #file_list = c('australia', 'canada', 'china','us','europe')
  
  files = str_c('../surveillance_data/flunet_samples/', file_list)
  
  data_orig = vector('list', length(files))
  for(i in 1:length(files)){
    data_orig[[i]] = read_csv(files[i], skip = 3)
  }
  flunet_data = do.call(rbind, data_orig) %>%
    rename_all(tolower) 
  flunet_data$country[flunet_data$country=='United States of America'] = 'United States'

  flunet_data = flunet_data %>%
    filter(sdate > ymd(20050101)) %>%
    select(country, year, week, ah1n12009, ah1, ah3, inf_b, byamagata, bvictoria) %>%
    replace_na(list(ah1n12009 = 0, ah1=0)) %>%
    mutate(ah1n12009 = as.numeric(ah1n12009),
           h1 = ah1n12009 + ah1,
           h3 = ah3,
           b=inf_b,
           byam = byamagata,
           bvic = bvictoria) %>%
    assign_season(south_timing) %>%
    select(country, season, week, h1, h3, b, byam, bvic)
  write_csv(flunet_data, 'data/flunet_weekly.csv')
  
  flunet_data = flunet_data %>%
    group_by(season, country) %>%
    dplyr::summarise(h1n1 = sum(h1, na.rm=T),
              h3n2 = sum(h3, na.rm=T), 
              b = sum(b, na.rm=T),
              byam = sum(byam, na.rm=T),
              bvic = sum(bvic, na.rm=T)) %>%
    gather(key=subtype, value = counts, h1n1, h3n2, b, byam, bvic) %>%
    mutate(source = 'flunet')
  write_csv(flunet_data, 'data/flunet_seasonal.csv')
  
}
