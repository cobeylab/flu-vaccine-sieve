library(tidyverse)
library(cowplot)

setwd('../')

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


ve.summ = read_csv('data/ve_summary.csv') %>%
  filter(country == 'Canada') %>% 
  select(subtype, VE) %>%
  spread(subtype, VE)

h3ve = ve.summ %>% pull(H3N2)
bve = ve.summ %>% pull(B)
h1ve = ve.summ %>% pull(H1N1)

coverages = seq(0,1, length=100)
k.h3.b = (1-h3ve*coverages)/(1-bve*coverages)
k.h3.h1 = (1-h3ve*coverages)/(1-h1ve*coverages)

mean_ratio = data_frame(coverage=coverages, k.h3.b, k.h3.h1) %>%
  rename('H3N2 to B' = 'k.h3.b', 'H3N2 to H1N1' = 'k.h3.h1') %>%
  gather(key = subtypes, value = ratio, -coverage)

points = mean_ratio %>% filter(round(coverage,2) == .14 | round(coverage,2) == .42)

expect = ggplot(mean_ratio, aes(x=coverage, y = ratio, color = subtypes)) + 
  geom_line(alpha = .7,size=.3) +
  ylab('Expected ratio') +
  xlab('Vaccine coverage') +
  scale_color_brewer(palette='Set1') + plot_themes +
  geom_point(data = points) + 
  geom_path(data = points, aes(group = subtypes), lty = 2) +
  geom_vline(data = points, aes(xintercept = coverage), color = 'black', alpha=.7) +
  annotate(geom='text', angle = 90, label = 'Europe', x=.08, y = 1.4) +
  annotate(geom='text', angle = 90, label = 'United States', x=.48, y = 1.8) +
  theme(legend.title = element_blank())


save_plot('plots/theory.pdf', expect, base_aspect_ratio=1.5, base_height =3)
