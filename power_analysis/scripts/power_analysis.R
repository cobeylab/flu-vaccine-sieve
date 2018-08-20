library(tidyverse)
library(viridis)
library(cowplot)
library(pwr)
source('utils.R')

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
        legend.position ='bottom',
        legend.direction='horizontal',
        legend.margin=margin(l = 8, unit='pt'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank())

ve_n = get_ve() %>% filter(country == 'Canada')
h3ve = ve_n %>% pull(h3ve)
h1ve = ve_n %>% pull(h1ve)
bve = ve_n %>% pull(bve)

coverageA = .2
coverageB = .4
VEs = seq(0,1,length = 100)
data = expand.grid(VE_1 = VEs, VE_2 = VEs)
data = data %>% 
  mutate(propA = (1-coverageA*VE_1)/(1-coverageA*VE_2),
         propB = (1-coverageB*VE_1)/(1-coverageB*VE_2)) %>%
  mutate(propA = propA/(1+propA),
         propB = propB/(1+propB),
         cohen_h = ES.h(propA, propB),
         VE_ratio = VE_1 / VE_2,
         VE_diff = VE_1 - VE_2, 
         samplesize = NA)
for(i in 1:nrow(data)){
  if(data$cohen_h[i] !=0){
    x = pwr.2p.test(h=data$cohen_h[i], power = .9)
    data$samplesize[i] = x$n
  }
}

subtype_ve = data.frame(subtype = c('H3N2', 'H1N1', 'B'),
                        ve = c(h3ve, h1ve,bve))

VE_summary_layer = function(base_plot, guide_title, scale_limits=NULL){
  out = base_plot +
    geom_tile() +
    scale_fill_viridis(limits = scale_limits, guide = guide_colorbar(title.position = 'top', barwidth = 9.5 , barheight = .5, 
                                                                   title = guide_title)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    plot_themes +
    xlab(expression(VE[1])) +
    ylab(expression(VE[2]))
  return(out)
}

velines = function(plot){
  plot = plot+
    geom_hline(data = subtype_ve, aes(yintercept = ve, color=subtype), size=1.5) +
    geom_vline(data = subtype_ve, aes(xintercept = ve, color=subtype), size = 1.5) +
    scale_color_brewer(palette = 'Set2')
  return(plot)
}

a = ggplot(data, aes(x = VE_1, y = VE_2, fill = propA)) %>%
  VE_summary_layer(scale_limits = c(.37, .63), guide_title = expression(italic(P[A])~textstyle('(Freq strain 1 in pop A)')))

b = ggplot(data, aes(x = VE_1, y = VE_2, fill = propB))  %>%
  VE_summary_layer(scale_limits = c(.37, .63), guide_title = expression(italic(P[B])~textstyle('(Freq strain 1 in pop B)')))

c = ggplot(data, aes(x = VE_1, y = VE_2, fill = propA - propB)) %>%
  VE_summary_layer(guide_title = expression(italic(P[A]-P[B])))

d = ggplot(data, aes(x = VE_1, y = VE_2, fill = cohen_h)) %>%
  VE_summary_layer(guide_title = expression(textstyle("Cohen's"~italic(h))))


VE_samplesize_layer = function(base_plot, guide_title){
  out = base_plot+ geom_tile() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_viridis(guide = guide_colorbar(title.position = 'top', barwidth = 9, barheight =0.5,
                                              title = guide_title))+
    plot_themes +
    xlab(expression(VE[1])) +
    ylab(expression(VE[2]))
  
  return(out)
  
}
e = ggplot(data %>% filter(samplesize < 5000), 
           aes(x = VE_1, y = VE_2, fill = samplesize)) %>% 
  VE_samplesize_layer(guide_title = 'N per population')

f = ggplot(data %>% filter(samplesize >= 5000 & samplesize < 20000), 
           aes(x = VE_1, y = VE_2, fill = samplesize)) %>% 
  VE_samplesize_layer(guide_title = 'N per population')

g = ggplot(data %>% filter(samplesize >= 20000 & samplesize < 50000), 
           aes(x = VE_1, y = VE_2, fill = samplesize)) %>% 
  VE_samplesize_layer(guide_title = 'N per population')

h = ggplot(data %>% filter(samplesize >= 50000 & samplesize < 200000), 
           aes(x = VE_1, y = VE_2, fill = samplesize)) %>% 
  VE_samplesize_layer(guide_title = 'N per population')

i = ggplot(data, 
           aes(x = VE_1, y = VE_2, fill = log(samplesize)))  %>%
  VE_samplesize_layer(guide_title = 'Log N per population')

# sample sizes needed based on frequencies alone

j = ggplot(data, 
           aes(x = propA, y = propB, color = log(samplesize))) + 
  geom_point() + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_viridis(guide = guide_colorbar(title.position = 'top', barwidth = 9, barheight =0.5,
                                            title = 'Log N per population'))+
  plot_themes +
  xlab(expression(italic(P[A]))) +
  ylab(expression(italic(P[B])))

data_plot = plot_grid(a,b,c,d, labels= 'AUTO', nrow = 2)
power_plot = plot_grid(e,f,g,h,i,j, labels= 'AUTO', ncol = 2)

save_plot('plots/power_simdata.pdf',data_plot, nrow = 2, base_aspect_ratio = 1.8, base_height = 3.5)
save_plot('plots/power.pdf',power_plot, nrow = 3, base_aspect_ratio = 1.8, base_height = 3.5)
