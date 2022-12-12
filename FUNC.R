library('readr')
library('ggplot2')
library('tidyverse')
library('dplyr')
library('hrbrthemes')
library('grid')
library('shadowtext')
library('viridis')

parse_number(testeee$População)
testeee <- testeee %>% mutate(População = parse_number(População))

#NEVADA
  
  numeric_nevada <- dplyr::filter(testeee, Estado == 'Nevada') %>%
    transform(Ano = as.character(Ano))
  
  VIOLENCIA_NEVADA <- numeric_nevada %>% 
    ggplot(aes(x = Ano, y = Violência, width = 0.6, label = Violência)) +
    geom_bar(stat= "identity", fill = '#6959CD', alpha=0.7) +
    ylab("Casos de Violência a cada 100 mil habitantes")+
    geom_label(size = 3, alpha = 0.5)
  
  ROUBOS_NEVADA <- numeric_nevada %>%
    ggplot(aes(x = Ano, y = Roubos, width = 0.6, label = Roubos)) +
    geom_bar(stat= "identity", fill = '#20B2AA', alpha=0.8) +
    ylab("Casos de Roubos a cada 100 mil habitantes")+
    geom_label(size = 3, alpha = 0.5)
  
  ASSASSINATOS_NEVADA <- numeric_nevada %>% 
    ggplot(aes(x = Ano, y = Assassinatos, width = 0.6, label = Assassinatos)) +
    geom_bar(stat= "identity", fill = '#556B2F', alpha=0.8) +
    ylab("Assassinatos a cada 100 mil habitantes")+
    geom_label(size = 3, alpha = 0.5)+ 
    coord_flip()
  
  
#GRÁFICOS TENNESSEE
  numeric_tennessee <- dplyr::filter(testeee, Estado == 'Tennessee') %>%
    transform(Ano = as.character(Ano))
  
  VIOLENCIA_TENNESSEE <- numeric_tennessee %>% 
    ggplot(aes(x = Ano, y = Violência, width = 0.6, label = Violência)) +
    geom_bar(stat= "identity", fill = '#F0E68C', alpha=1) +
    ylab("Casos de Violência a cada 100 mil habitantes")+
    geom_text(size = 3, alpha = 1)
  
  ROUBOS_TENNESSEE <- numeric_tennessee %>%
    ggplot(aes(x = Ano, y = Roubos, width = 0.6, label = Roubos)) +
    geom_bar(stat= "identity", fill = '#D8BFD8', alpha=0.9) +
    ylab("Casos de Roubos a cada 100 mil habitantes")+
    geom_text(size = 3, alpha = 1)
  
  ASSASSINATOS_TENNESSEE <- numeric_tennessee %>% 
    ggplot(aes(x = Ano, y = Assassinatos, width = 0.6, label = Assassinatos)) +
    geom_bar(stat= "identity", fill = '#FFE4E1', alpha=0.8) +
    ylab("Assassinatos a cada 100 mil habitantes")+
    geom_text(size = 3, alpha = 1)+ 
    coord_flip()
  
  numeric_tennessee %>%
    transform(Ano = as.numeric(Ano)) %>% 
    ggplot(aes(x= Ano, y= Assassinatos)) +
    scale_x_continuous(breaks = seq(1989,1999,1)) +
    geom_line(stat= "identity")+ geom_smooth(method=lm, se=FALSE, alpha=0.5, color="#8B4513")
  
  
#BOXPLOT TOTAL:
  
  BOXPLOT_SN_ASSASSINATOS <- ggplot(testeee, aes(x=as.factor(Porte), y=Assassinatos)) + 
    scale_y_continuous(breaks = seq(0,22,2)) +
    geom_boxplot(fill="slateblue", alpha=0.7, outlier.shape = NA) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("Número de assassinatos a cada 100mil pessoas")+
    xlab("")+
    ylab("")
  
    BOXPLOT_SN_ROUBOS <- ggplot(testeee, aes(x=as.factor(Porte), y=Roubos)) + 
      scale_y_continuous(breaks = seq(0,660,50)) +
      geom_boxplot(fill="green", alpha=0.7, outlier.shape = NA) +
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
      geom_boxplot(fill="gray", alpha=0.9, outlier.shape = NA) +
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
    
    summary(testeee)
    sd(testeee$Violência)
    cor(numeric_northD$Assassinatos,numeric_northD$Violência)
    