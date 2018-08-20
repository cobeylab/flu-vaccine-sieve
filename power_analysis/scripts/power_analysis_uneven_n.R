library(tidyverse)
library(viridis)
library(Hmisc)
library(wpp2017)
library(tidyverse)
library(cowplot)
library(lubridate)
library(fmsb)
library(MultinomialCI)
library(pwr)
source('utils.R')
source('colors.R')

setwd('../')

power_boot = function(data_us, propA, propB){
  power = 0
  iters = 500
  for(k in 1:iters){

    sim_us = data_us %>% mutate(propA = NA,
                                propB = NA)
        # generate proportions 
    for(j in 1:nrow(sim_us)){
      sim_us$propA[j]  = rbinom(n = 1, prob =propA, size = sim_us$total_pair[j])
      sim_us$propB[j]  = rbinom(n = 1, prob =propB, size = sim_us$total_pair[j])
    }
    prop_summ = sim_us %>% 
      mutate(propA = propA/total_pair, propB=propB/total_pair) %>%
      summarise(propA = mean(propA), propB = mean(propB))
    
    # do test
    pval = proptest_boot(sim_us, prop_summ)
    
    if(pval < .05){
      power = power+1
    }
  }
  power = power/iters
  return(power)
}

proptest_boot = function(sim_us, prop_summ){
  iters2 = 500
  test = sim_us
  pval= 0
  for(j2 in 1:iters2){
    # generate proportions from null distribution
    for(j3 in 1:nrow(test)){
      test$propA[j3]  = rbinom(n = 1, prob =.5, size = test$total_pair[j3])
      test$propB[j3]  = rbinom(n = 1, prob =.5, size = test$total_pair[j3])
    }
    test_summ = test %>% 
      mutate(propA = propA/total_pair, propB=propB/total_pair) %>%
      summarise(propA = mean(propA), propB = mean(propB))
    if(abs(ES.h(test_summ$propA,test_summ$propB)) >= abs(ES.h(prop_summ$propA,prop_summ$propB))){
      pval = pval+1
    }
  }
  pval = pval/iters2
  return(pval)
}

get_usdata = function(){
  out = get_flunet() %>% 
    rename_all(tolower) %>% 
    filter(season %in% SEASONS)  %>% 
    filter(country == "United States") %>%
    group_by(season) %>%
    dplyr::summarise(b_h3n2_tot = sum(b+h3n2),
                     h3n2_h1n1_tot = sum(h3n2+h1n1),
                     total = sum(b+h3n2+h1n1)) %>%
    mutate(total_pair = total*2/3) %>%
    mutate(total_pair = round(total_pair)) %>%
    select(season, total_pair) %>%
    mutate(fraction_pair = normalize(total_pair))
  return(out)
}

get_flunet = function(){
  data = read_csv('data/flunet_seasonal.csv') %>%
    mutate(subtype = toupper(subtype)) %>%
    spread(subtype, counts) %>%
    mutate(total = B+H1N1+H3N2)
  return(data)
}

SEASONS = as.character(2006:2016)

#south_timing = 'southlag'
south_timing = 'southlead'
#south_timing = NULL
summarize_flunet(south_timing)

ve_n = get_ve() %>% filter(country == 'Canada')
h3ve = ve_n %>% pull(h3ve)
h1ve = ve_n %>% pull(h1ve)
bve = ve_n %>% pull(bve)

coverageA = .2
coverageB = .4

get_prop = function(VE_1, VE_2, coverageA, coverageB){
  propA = (1-coverageA*VE_1)/(1-coverageA*VE_2)
  propB = (1-coverageB*VE_1)/(1-coverageB*VE_2)
  propA = propA/(1+propA)
  propB = propB/(1+propB)
  return(list(propA=propA, propB= propB))
}
propA = get_prop(h3ve, h1ve, coverageA, coverageB)$propA
propB = get_prop(h3ve, h1ve, coverageA, coverageB)$propB

#propA = get_prop(0, .5, coverageA, coverageB)$propA
#propB = get_prop(0, .5, coverageA, coverageB)$propB

totaln = 10000
data_us = get_usdata()
#data_us = data_us %>% mutate(fraction_pair = 1/nrow(data_us))

data_us  = data_us %>% 
  filter(season > 2008) %>%
  mutate(total_pair = totaln*fraction_pair ) %>%
  mutate(total_pair = round(total_pair))# %>%

# data_us = b_data %>% filter(location == 'Europe') %>%
#   mutate(total_pair = matched+unmatched) %>%
#   mutate(fraction_pair = normalize(total_pair))

power = power_boot(data_us, propA, propB)

pwr.2p.test(h=ES.h(propA,propB),power = .9)

pwr.2p.test(h=ES.h(propA,propB),n = 4000)

sum(data_us$total_pair)
