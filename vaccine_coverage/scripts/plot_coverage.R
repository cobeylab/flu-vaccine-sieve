library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(wpp2017)

labelseason = function(x){
  return(str_c(x, '-', substr(as.numeric(x) + 1,3,4)))
}

get_pop = function(countries){
  data(pop)
  pop_sizes = pop %>% mutate(name=as.character(name)) %>% 
    filter(name %in% unique(countries)) %>%
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
        legend.margin=margin(l = 8, unit='pt'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank())

exclude_countries = c('Malta', 'Luxembourg', "Netherlands")

#keep_countries = c("Australia"  ,   "Italy"   ,"Canada"  ,      "United States", "China"    )

coverage = read_csv('../data/vaccine_coverage.csv') %>%
  mutate(seasonlabel = str_c(season, substr(season+1,3,4), sep='-')) %>%
  filter(!country %in% exclude_countries)


doublepair = c(brewer.pal(12,'Paired'), brewer.pal(12,'Paired'))
coverage
doublepair_palette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- doublepair
  colorRampPalette(pal, ...)
}

scale_color_doublepair = function(palette = 'main', ...) {
  pal <- doublepair_palette(palette = palette)
  discrete_scale("colour", paste0("doublepair_", palette), palette = pal, ...)
}

base_shapes = c(16,17,15,3)
shapes = rep(base_shapes, ceiling(length(unique(coverage$country))/length(base_shapes)))[1:length(unique(coverage$country))]


a = ggplot(coverage %>% filter(!is.na(season)), 
                      aes(x =seasonlabel, y=coverage/100, color = country, group = country, shape = country)) +
  geom_point(size=2) + 
  geom_line() +
  ylab('Seasonal vaccine coverage (fraction)') + 
  xlab('Season') + plot_themes +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_doublepair() +
  scale_shape_manual(values = shapes) 
  

eurtotal = 737e6
noteurope = c('Australia', "Canada", 'United States', "China")
eurcountries = unique(coverage$country[!coverage$country %in%noteurope])
x = coverage %>% mutate(census_year = round(season/5)*5) %>%
  merge(get_pop(eurcountries)) %>%
  mutate(location = ifelse(country %in% noteurope, NA, 'Europe')) %>%
  mutate(location = ifelse(country  == 'United States', "United States", location)) %>%
  filter(!is.na(location)) 
x = x%>%  dplyr::group_by(location,season) %>%
  dplyr::mutate(pop2 = pop/sum(pop),
                eurtot = sum(pop)) %>%
  dplyr::summarise(coverage = sum(pop2*coverage), eurtot = sum(pop*1000)/eurtotal) %>% 
  ungroup() %>%
  mutate(eurtot = ifelse(location == "United States", NA, eurtot))

b = 
  ggplot(x, 
                        aes(x = labelseason(season), y=coverage/100, color = location, group = location)) +
  geom_point(size=2) + 
  geom_line() +
  ylab('Seasonal vaccine coverage (fraction)') + 
  xlab('Season') + plot_themes +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_brewer(palette= 'Dark2')  + ylim(0,.5) + geom_line(aes(y = eurtot), lty=2)

coverage_summ = plot_grid(a, b, labels='AUTO', nrow=2)


save_plot('../plots/coverage_summary.pdf',coverage_summ, base_aspect_ratio = 1.1, base_height=7)
