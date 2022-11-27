library('readr')
library('ggplot2')
library('tidyverse')
library('dplyr')

#lendo planilha

dados <- read.csv(file = 'planilha_orig.csv', stringsAsFactors = FALSE)

dados <- read.csv(file = 'planilha_orig.csv', header = TRUE, sep = ';')

#renomeando variaveis

rename_1 <- rename(dados, Ano = 'year','Violência' = 'violent',
                   'Assassinatos' = 'murder', 'Roubos' = 'robbery',
                   'Taxa de encarceramento' = 'prisoners', 'População' = 'population',
                   'Porte' = "law", 'Estado' = 'state')

dados1 <- select(rename_1, 'Ano', 'Violência', 'Assassinatos', 'Roubos',
                 'Taxa de encarceramento', 'População', 'Porte', 'Estado')

#traduzindo linhas

dados2 <- dados1 %>%  mutate(across('Porte',str_replace,'no','Não'))
dados2 <- dados2 %>%  mutate(across('Porte',str_replace,'yes','Sim'))


#filtrando tabelas por Estado

df_alabama <-dados2 %>% filter(Estado %in% 'Alabama')
df_colorado <-dados2 %>% filter(Estado %in% 'Colorado')
df_texas <-dados2 %>% filter(Estado %in% 'Texas')
df_pennsylvania <-dados2 %>% filter(Estado %in% 'Pennsylvania')
df_oregon <-dados2 %>% filter(Estado %in% 'Oregon')
df_oklahoma <-dados2 %>% filter(Estado %in% 'Oklahoma')
df_northDakota <-dados2 %>% filter(Estado %in% 'North Dakota')
df_california <-dados2 %>% filter(Estado %in% 'California')
df_newYork <-dados2 %>% filter(Estado %in% 'New York')
df_florida <-dados2 %>% filter(Estado %in% 'Florida')
df_georgia <-dados2 %>% filter(Estado %in% 'Georgia')
df_idaho <-dados2 %>% filter(Estado %in% 'Idaho')
df_indiana <-dados2 %>% filter(Estado %in% 'Indiana')
df_alaska <- dados2 %>% filter(Estado %in% 'Alaska')
df_kentucky <- dados2 %>% filter(Estado %in% 'Kentucky')
df_louisiana <- dados2 %>% filter(Estado %in% 'Louisiana')
df_mississippi <- dados2 %>% filter(Estado %in% 'Mississippi')
df_nevada <- dados2 %>% filter(Estado %in% 'Nevada')
df_southDakota <- dados2 %>% filter(Estado %in% 'South Dakota')
df_tennessee <- dados2 %>% filter(Estado %in% 'Tennessee')
df_utah <- dados2 %>% filter(Estado %in% 'Utah')

#filtrando os últimos 10 anos por estado

att_alabama <- df_alabama %>% filter(Ano >= 1989)
att_colorado <- df_colorado %>% filter(Ano >= 1989)
att_texas <- df_texas %>% filter(Ano >= 1989)
att_pennsylvania <- df_pennsylvania %>% filter(Ano >= 1989)
att_oregon <- df_oregon %>% filter(Ano >= 1989)
att_oklahoma <- df_oklahoma %>% filter(Ano >= 1989)
att_northDakota <- df_northDakota %>% filter(Ano >= 1989)
att_california <- df_california %>% filter(Ano >= 1989)
att_newYork <- df_newYork %>% filter(Ano >= 1989)
att_florida <- df_florida %>% filter(Ano >= 1989)
att_georgia <- df_georgia %>% filter(Ano >= 1989)
att_idaho <- df_idaho %>% filter(Ano >= 1989)
att_indiana <- df_indiana %>% filter(Ano >= 1989)
att_alaska <- df_alaska %>% filter (Ano >= 1989)
att_kentucky <- df_kentucky %>% filter (Ano >= 1989)
att_louisiana <- df_louisiana %>%  filter(Ano >= 1989)
att_mississipi <- df_mississippi %>%  filter(Ano >= 1989)
att_nevada <- df_nevada %>%  filter(Ano >= 1989)
att_southDakota <- df_southDakota %>%  filter(Ano >= 1989)
att_tennessee <- df_tennessee %>%  filter(Ano >= 1989)
att_utah <- df_utah %>%  filter(Ano >= 1989)

#arrumando dados

att_alaska <- transform(att_alaska, População = as.numeric(População))
att_alaska <- mutate(att_alaska,'População' = População*1000)

att_florida <- transform(att_florida, População = as.numeric(as.factor(População)))
att_florida <- mutate(att_florida,'População' = População*10)

att_northDakota <- transform(att_northDakota, População = as.numeric(População))
att_northDakota <- mutate(att_northDakota,'População' = População*1000)

att_southDakota <- transform(att_southDakota, População = as.numeric(População))
att_southDakota <- mutate(att_southDakota,'População' = População*1000)


