library(tidyverse)
library(binom)
library(stringi)
library(ggsci)
library(cowplot)
library(MultinomialCI)

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

get_strains = function(data, this_season){
  strains = data %>% 
    filter(season == this_season) %>%
    mutate(clade_membership = ifelse(clade_membership %in% c('3c', '3c3'), 'ancestral', clade_membership)) %>%
    group_by(region, clade_membership) %>%
    summarise(counts = n()) %>% ungroup() %>%
    group_by(region) %>%
    spread(clade_membership, counts, fill=0) %>%
    gather(clade_membership, counts, -region) %>% 
    mutate(total = sum(counts), freq = counts/total) %>%
    ungroup() %>%
    mutate(lower = NA, upper = NA) %>%
    arrange(region) 
  
  for(this.region in unique(strains$region)){
    CIs = multinomialCI(strains %>% filter(region == this.region) %>% pull(counts), alpha = .05)
    strains[strains$region == this.region,][,c('lower','upper')] = CIs
  }
  
  return(strains)
}

make_plots = function(data, this_season){
  
  regions_by_clade = ggplot(data, aes(x=clade_membership, y=freq)) +
    geom_bar(stat = 'identity', aes(fill = clade_membership)) + 
    geom_errorbar(aes(ymin = lower, ymax=upper), width = .2) +
    facet_wrap(~region_labels, nrow=1) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    ylab('Frequency') + xlab('Clade') +
    guides(fill='none') +
    scale_fill_brewer(palette = 'Set1')
  
  clades_by_region = ggplot(data, aes(x=region_labels, y=freq)) +
    geom_bar(stat = 'identity', aes(fill = region_labels)) + 
    geom_errorbar(aes(ymin = lower, ymax=upper), width = .2) +
    facet_wrap(~clade_membership, nrow=2) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    ylab('Frequency') + xlab('Region') +
    guides(fill='none')# +
    #scale_fill_brewer(palette = 'Set3')
  
  # clades_3c2a_3c3b = ggplot(data %>% filter(clade_membership %in% c('3c2.A', '3c3.B')), aes(x=region_labels, y=freq)) +
  #   geom_bar(stat = 'identity', aes(fill = region_labels)) + 
  #   geom_errorbar(aes(ymin = lower, ymax=upper), width = .2) +
  #   facet_wrap(~clade_membership) +
  #   theme(axis.text.x = element_text(angle=45, hjust=1)) +
  #   ylab('Frequency') + xlab('Clade') +
  #   guides(fill='none') #+
  #  # scale_fill_brewer(palette = 'Set3')
  
  save_plot(str_c('../plots/regions_by_clade_',this_season,'.pdf'),regions_by_clade, base_height=3,base_aspect_ratio = 1.8)
  save_plot(str_c('../plots/clades_by_region_',this_season,'.pdf'),clades_by_region, base_height=3,base_aspect_ratio = 1.8)
  #save_plot(str_c('../plots/clades_3c2a_3c3b_',this_season,'.pdf'),clades_3c2a_3c3b, base_aspect_ratio = 1.6)
}

data = read_csv('../data/hi_h3n2_ha_12y_vaccine_antigenic_distances.csv')
names(data) = gsub( '/', '_',names(data))

this_season = '2014-2015'
strains = get_strains(data = data, this_season = this_season)
strains$region_labels = gsub('_',' ',strains$region) %>% stri_trans_totitle()
strains = strains %>% filter(  region_labels %in% c('North America','Europe'))

compare.ratios = function(strains,clade1, clade2, ylabel){
  
  ratios = strains %>% filter(clade_membership %in% c(clade1, clade2)) %>%
    group_by(region) %>%
    mutate(freq_old = freq) %>%
    mutate(freq = freq_old/sum(freq_old),
           total = sum(counts)) %>%
    mutate(upper = freq + (upper-freq_old)/sum(freq_old),
           lower = freq - (freq_old-lower)/sum(freq_old)) %>% 
    ungroup() %>%
    mutate(err = max(upper-freq, freq-lower)) %>%
    select(region, clade_membership, err, freq) %>%
    spread(clade_membership, freq) %>%
    as.data.frame()
  
  ratios$ratio = ratios[,clade1]/ratios[,clade2]
  ratios$err = ((ratios$err/ratios[,clade1])^2 + (ratios$err/ratios[,clade2])^2)^.5
  
  ratios = ratios %>%
    mutate(region = ifelse(region == 'europe', 'Europe', 'North America')) %>%
    group_by(region) %>% mutate(
           lowerCI = max(0,ratio - err),
           upperCI = ratio+err)
  print(ratios)
  
  ratio_plot = ggplot(ratios, aes(x = region, y= ratio)) + 
    geom_bar(stat='identity', aes(fill = region)) +
    scale_fill_brewer(palette = 'Dark2', guide = F) +
    geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.5) + 
    xlab('Region') + 
    ylab(ylabel) 
  
  return(ratio_plot)
}

a = compare.ratios(strains,'3c2.A', '3c3.B', 'Ratio 3C2.a:3C3.b') 
b = compare.ratios(strains,'3c2.A', 'ancestral', 'Ratio 3C2.a:Ancestral')
c = compare.ratios(strains,'3c3.B', 'ancestral', 'Ratio 3C3.b:Ancestral')

expect_plot = ggplot(data.frame(x=c(1,3), y=c(.2,1)), aes(x=x, y=y)) +
  geom_line(color = 'blue') +
  geom_point(color = 'blue') +
  theme(axis.title = element_blank(), 
        axis.text=element_blank(), 
        axis.ticks = element_blank(),
        plot.margin=unit(c(10,10,5,40),'pt')) +
  xlim(c(0,4)) + ylim(c(0,1))

ratio_row = plot_grid(a, b, c, nrow = 1)#labels='AUTO', nrow=1)
expectation_row = plot_grid(expect_plot, expect_plot, expect_plot, nrow = 1, labels='AUTO')
ratio_plot = plot_grid(expectation_row, ratio_row, ncol=1, rel_heights = c(.25,1))
save_plot('../plots/h3n2_ratios.pdf',ratio_plot, base_aspect_ratio = 3,base_height=3.5)

make_plots(strains, this_season)
