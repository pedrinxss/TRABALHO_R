library('readr')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('hrbrthemes')
library('grid')
library('shadowtext')
library(viridis)

install.packages('viridis')


#grafico North Dakota
  numeric_northD <- dplyr::filter(testeee, Estado == 'North Dakota') %>%
  mutate(População = População*1000) %>%
  transform(Ano = as.character(Ano))
  
  VIOLENCIA_ND <- numeric_northD %>% 
    ggplot(aes(x = Ano, y = Violência, width = 0.6, label = Violência)) +
    geom_bar(stat= "identity", fill = '#A9A9A9') +
    ylab("Casos de Violência a cada 100 mil habitantes")+
    geom_label(size = 3, alpha = 0.5)
    
  ROUBOS_ND <- numeric_northD %>%
  transform(Ano = as.numeric(Ano)) %>% 
  ggplot(aes(x= Ano, y= Roubos)) +
  scale_x_continuous(breaks = seq(1989,1999,1)) +
  geom_line(stat= "identity")+
  geom_point()
  
  ASSASSINATOS_ND <- numeric_northD %>% 
    ggplot(aes(x = Ano, y = Assassinatos, width = 0.6, label = Assassinatos)) +
    geom_bar(stat= "identity", fill = '#A9A9A9') +
    ylab("Casos de Violência a cada 100 mil habitantes")+
    geom_label(size = 3, alpha = 0.5)+
    coord_flip()

  
  
#BOXPLOT TOTAL:
  
  BOXPLOT_SN_ASSASSINATOS <- ggplot(testeee, aes(x=as.factor(Porte), y=Assassinatos)) + 
    scale_y_continuous(breaks = seq(0,22,5)) +
    geom_boxplot(fill="slateblue", alpha=0.7) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("Número de assassinatos a cada 100mil pessoas")+
    xlab("")
    ylab("")
  
    BOXPLOT_SN_ROUBOS <- ggplot(testeee, aes(x=as.factor(Porte), y=Roubos)) + 
      scale_y_continuous(breaks = seq(0,660,50)) +
      geom_boxplot(fill="green", alpha=0.7) +
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("Número de roubos a cada 100mil pessoas")+
      xlab("")+
    ylab("")
    
    BOXPLOT_SN_ATVIOLE <- ggplot(testeee, aes(x=as.factor(Porte), y=Violência)) + 
      scale_y_continuous(breaks = seq(0,1300,100)) +
      geom_boxplot(fill="green", alpha=0.7) +
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("Número de incidentes de violência a cada 100mil pessoas")+
      xlab("")+
      ylab("")