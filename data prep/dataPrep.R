library(readxl)
library(data.table)
library(tidyverse)
library(shiny)
library(foreign)
library(ggplot2)
library(highcharter)
library(shinydashboard)
library(openxlsx)
library(stringi)
library(leaflet)
library(rgdal)
library(dplyr)
library(scales)
library(htmlwidgets)
library(tm)
library(wordcloud)
library(memoise)
library(sjlabelled)
library(shinyjs)
library(lubridate)
library(zoo)
library(sjstats)

dataset <- read.spss("3269.sav", to.data.frame=TRUE)

fecha_estu <- "Diciembre 2019"

Labelspath <- "labelsclean.xlsx"

CleanLabels <- Labelspath %>% 
  excel_sheets() %>% 
  set_names() %>%
  map(~ read_excel(path = Labelspath, sheet = .x, ,  trim_ws = FALSE))

partido_labels <- data.frame(CleanLabels$partidoslabels)
estulables <- data.frame(CleanLabels$estulabels)
incomehogar_lables <- data.frame(CleanLabels$incomelabels)
incomeperso_lables <- data.frame(CleanLabels$incomelabelsperso)
colorleaderlables <- data.frame(CleanLabels$leaderscolors)
tamunilables <- data.frame(CleanLabels$tamunilables)
partidoperflables <- data.frame(CleanLabels$partidosperflab)
partidomaplables <- data.frame(CleanLabels$partidosmaplabels)
recuerdolables <- data.frame(CleanLabels$recuerdolables)
partidosreculables <- data.frame(CleanLabels$partidosrecu)

# partido_labels <- read.xlsx("partidoslabels.xlsx") %>% 
#   mutate(P29 = Intención.de.Voto)


#-----------new------


estuorden <- estulables %>%
  arrange(orden) %>%
  select(estudios_clean) %>%
  unique() %>%
  pull()

estulables <- estulables %>% 
  select(-orden) %>%
  mutate(estudios_clean = factor(estudios_clean, levels = estuorden)) %>%
  rename(`Nivel de estudios` = P41A)


incomhogorden <- incomehogar_lables %>%
  arrange(orden) %>%
  select(ingresospersonaclean) %>%
  unique() %>%
  pull()

incomehogar_lables <- incomehogar_lables %>% 
  select(-orden) %>%
  mutate(ingresospersonaclean = factor(ingresospersonaclean, levels = incomhogorden)) %>%
  rename(`Ingresos del hogar` = P49)


incomperorden <- incomeperso_lables %>%
  arrange(orden) %>%
  select(incomepersoclean) %>%
  unique() %>%
  pull()

incomeperso_lables <- incomeperso_lables %>% 
  select(-orden) %>%
  mutate(incomepersoclean = factor(incomepersoclean, levels = incomperorden))  %>%
  rename(`Ingresos de la persona` = P50)


colorlables <- partido_labels %>%
  select(partido_clean, color) %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  unique()

tamuniorder <- tamunilables %>%
  arrange(orden) %>%
  select(tamun_iclean) %>%
  unique() %>%
  pull()

tamunilables <- tamunilables %>% 
  select(-orden) %>%
  mutate(tamun_iclean = factor(tamun_iclean, levels = tamuniorder)) %>%
  rename(`Tamaño del Municipio` = TAMUNI)

partido_labels <- partido_labels %>% 
  rename(`Intención de Voto` = P29)

partidomaplables <- partidomaplables %>% 
  rename(`Intención de Voto` = P29)

partidosreculables <- partidosreculables %>% 
  rename(`Intención de Voto` = P29)

recuerdolables <- recuerdolables %>% 
  rename(`Partido Votado 2019` = P36A)

demoinp <- c("Total","Tamaño del Municipio",
             "Sexo", "Edad", "Nivel de estudios", 
             "Religión", "Estado Civil", "Ingresos del hogar", "Ingresos de la persona")



dataset.labels <- as.data.frame(attr(dataset, "variable.labels")) %>% 
  mutate(names = as_character(attr(dataset, 'variable.labels')))

dedupnames <- make.unique(dataset.labels$names)

names(dataset) <- dedupnames

dataset <- setnames(dataset,1:2,c("ESTU","ID"))

dataset1 <- dataset %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en supuestas elecciones generales", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos personales",
             "Recuerdo de voto en las elecciones generales de abril de 2019 de los votantes"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona", "Partido Votado 2019")) %>%
  mutate(Edadperf = case_when(
    Edad %in% 18:25 ~ "18-25",
    Edad %in% 26:35 ~ "26-35",
    Edad %in% 36:45 ~ "36-45",
    Edad %in% 46:55 ~ "46-55",
    Edad %in% 56:65 ~ "56-65",
    Edad %in% 66:75 ~ "66-75",
    Edad %in% 76:85 ~ "76-85",
    Edad %in% 86:140 ~ "85+"
  )) %>% 
  mutate(`Ingresos del hogar perf` = `Ingresos del hogar`) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>% 
  left_join(partidomaplables, by = "Intención de Voto") %>%
  mutate(`Partido` = partido_cleanm) %>%
  left_join(recuerdolables, by = "Partido Votado 2019") %>%
  mutate(`Partido Votado 2019` = recuerdo_clean) %>% 
  left_join(partidosreculables, by = "Intención de Voto") %>% 
  mutate(`Intención de Votor` = partido_cleanr) %>% 
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  mutate(Total = as.factor(Total))

ponderacion <- dataset %>% 
  select(`Comunidad Autonoma`, `Ponderación Total`) %>% 
  unique()


demodata <- dataset1 %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) %>% 
  mutate(Total = fecha_estu)


demoleadersdata <- dataset1 %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`, 
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `Pedro Sánchez`,`Pablo Casado`,`Inés Arrimadas`,
         `Pablo Iglesias`, `Alberto Garzón`, `Santiago Abascal`) %>% 
  gather( `Pedro Sánchez`,`Pablo Casado`,`Inés Arrimadas`,
          `Pablo Iglesias`, `Alberto Garzón`, `Santiago Abascal`, key = leaders, value = valoración) %>%
  mutate(valoración = case_when(valoración == "1 Muy mal" ~ "1",
                                valoración == "N.S." ~ NA_character_,
                                valoración == "N.C." ~ NA_character_,
                                valoración == "No conoce" ~ NA_character_,
                                valoración == "10 Muy bien" ~ "10",
                                TRUE ~ valoración)) %>%
  mutate(valoración = as.numeric(valoración)) %>% 
  mutate(Total = fecha_estu)

leaderscolors <- read.xlsx("leaderscolors.xlsx")


perfdata <- dataset1 %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edadperf`, `Nivel de estudios`, 
         `Religión`, `Estado Civil`, `Ingresos del hogar perf`,
         `Ingresos de la persona`, color) %>% 
  mutate(`Ingresos del hogar` = `Ingresos del hogar perf`) %>% 
  mutate(Edad = Edadperf) %>% 
  rename(Partido = `Intención de Voto`)

#str_replace(perfdata$`Comunidad Autonoma`,"\\(\\)","")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Navarra (Comunidad Foral de)", "Comunidad Foral de Navarra")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Asturias (Principado de)", "Principado de Asturias")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Balears (Illes)", "Islas Baleares")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Madrid (Comunidad de)", "Comunidad de Madrid")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Castilla-La Mancha", "Castilla - La Mancha")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Comunitat Valenciana", "Comunidad Valenciana")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Murcia (Región de)", "Región de Murcia")
perfdata$`Comunidad Autonoma` <- stri_replace_all_fixed(perfdata$`Comunidad Autonoma`,
                                                        "Rioja (La)", "La Rioja")
partidosorden <- perfdata %>%
  filter(!Partido %in% c("NS/NC", "No vota", NA)) %>% 
  group_by(Partido) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  select(Partido) %>%
  unique() %>%
  pull()


partidosorde <- perfdata %>%
  filter(!Partido %in% c("NS/NC", "No vota", NA)) %>% 
  group_by(Partido) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  select(Partido) %>%
  mutate(Partido = factor(Partido, levels = partidosorden))


mapvdata <- dataset1 %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Partido`, `Sexo`, `Edad`, `Nivel de estudios`, 
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `colorm`) %>% 
  rename(color = colorm)

mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Navarra (Comunidad Foral de)", "Comunidad Foral de Navarra")
mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Asturias (Principado de)", "Principado de Asturias")
mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Balears (Illes)", "Islas Baleares")
mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Madrid (Comunidad de)", "Comunidad de Madrid")
mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Castilla-La Mancha", "Castilla - La Mancha")
mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Comunitat Valenciana", "Comunidad Valenciana")
mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Murcia (Región de)", "Región de Murcia")
mapvdata$`Comunidad Autonoma` <- stri_replace_all_fixed(mapvdata$`Comunidad Autonoma`,
                                                        "Rioja (La)", "La Rioja")

md_map <- jsonlite::fromJSON(txt = "mapaespcom.geojson",simplifyVector = FALSE)


# partidosreculables <- partidosreculables %>% 
#   mutate(`Partido Votado 2019` = P29)

shankeydata <- dataset1 %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Votor`, `Sexo`, `Edad`, `Nivel de estudios`, 
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, colorr,
         `Partido Votado 2019`) %>% 
  rename(`Intención de Voto` = `Intención de Votor`) %>% 
  rename(color = colorr)


# partidosorden <- perfdata %>%
#   filter(!Partido %in% c("NS/NC", "No vota", NA)) %>% 
#   group_by(Partido) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   select(Partido) %>%
#   unique() %>%
#   pull()
# 
# partidosorde <- perfdata %>%
#   filter(!Partido %in% c("NS/NC", "No vota", NA)) %>% 
#   group_by(Partido) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   select(Partido) %>%
#   mutate(Partido = factor(Partido, levels = partidosorden))



preocupadata <- dataset1 %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`, 
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, color, `Primer problema`, 
         `Segundo problema`, `Tercer problema`, `Escala de autoubicación ideológica (1-10)`) %>% 
  setnames(c("Primer problema", "Segundo problema", "Tercer problema", "Escala de autoubicación ideológica (1-10)"),
           c("Problemas","Problemas2", "Problemas3", "Escala Ideológica"))

preocupadata$Total<- str_replace(preocupadata$Total,"3261", "Total")
preocupadata$Problemas<- str_replace(preocupadata$Problemas,"Los/as políticos/as en general, los partidos y la política", "Políticos, partidos y política")
preocupadata$Problemas2<- str_replace(preocupadata$Problemas2,"Los/as políticos/as en general, los partidos y la política", "Políticos, partidos y política")
preocupadata$Problemas3<- str_replace(preocupadata$Problemas3,"Los/as políticos/as en general, los partidos y la política", "Políticos, partidos y política")

preocupadata$Problemas<- str_replace(preocupadata$Problemas,"Los problemas relacionados con la calidad del empleo", "Calidad del empleo")
preocupadata$Problemas2<- str_replace(preocupadata$Problemas2,"Los problemas relacionados con la calidad del empleo", "Calidad del empleo")
preocupadata$Problemas3<- str_replace(preocupadata$Problemas3,"Los problemas relacionados con la calidad del empleo", "Calidad del empleo")

preocupadata$Problemas<- str_replace(preocupadata$Problemas,"Los problemas de índole económica", "Problemas índole económica")
preocupadata$Problemas2<- str_replace(preocupadata$Problemas2,"Los problemas de índole económica", "Problemas índole económica")
preocupadata$Problemas3<- str_replace(preocupadata$Problemas3,"Los problemas de índole económica", "Problemas índole económica")

preocupadata$Problemas<- str_replace(preocupadata$Problemas,"La corrupción y el fraude", "Corrupción y fraude")
preocupadata$Problemas2<- str_replace(preocupadata$Problemas2,"La corrupción y el fraude", "Corrupción y fraude")
preocupadata$Problemas3<- str_replace(preocupadata$Problemas3,"La corrupción y el fraude", "Corrupción y fraude")


preocupadata <- preocupadata %>% 
  mutate(Total = fecha_estu)
#----  

datasetene <- read.spss("3238.sav", to.data.frame=TRUE)

datasetene.labels <- as.data.frame(attr(datasetene, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetene, 'variable.labels')))

dedupnamesene <- make.unique(datasetene.labels$names)

names(datasetene) <- dedupnamesene

datasetene <- setnames(datasetene,1:2,c("ESTU","ID"))

demodataene <- datasetene %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en supuestas elecciones generales", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona")) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) 

#----  

datasetfeb <- read.spss("3240.sav", to.data.frame=TRUE)

datasetfeb.labels <- as.data.frame(attr(datasetfeb, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetfeb, 'variable.labels')))

dedupnamesfeb <- make.unique(datasetfeb.labels$names)

names(datasetfeb) <- dedupnamesfeb

datasetfeb <- setnames(datasetfeb,1:2,c("ESTU","ID"))

demodatafeb <- datasetfeb %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en supuestas elecciones generales", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona")) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) 
#----  

datasetmar <- read.spss("3242.sav", to.data.frame=TRUE)

datasetmar.labels <- as.data.frame(attr(datasetmar, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetmar, 'variable.labels')))

dedupnamesmar <- make.unique(datasetmar.labels$names)

names(datasetmar) <- dedupnamesmar

datasetmar <- setnames(datasetmar,1:2,c("ESTU","ID"))

demodatamar <- datasetmar %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en las elecciones generales de 2019", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona"),skip_absent=TRUE) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `color`, `Primer problema`) %>% 
  mutate(`Estado Civil` = NA) %>% 
  mutate(`Ingresos del hogar` = NA) %>% 
  mutate(`Ingresos de la persona` = NA)
#----  

datasetapr <- read.spss("3245.sav", to.data.frame=TRUE)

datasetapr.labels <- as.data.frame(attr(datasetapr, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetapr, 'variable.labels')))

dedupnamesapr <- make.unique(datasetapr.labels$names)

names(datasetapr) <- dedupnamesapr

datasetapr <- setnames(datasetapr,1:2,c("ESTU","ID"))

demodataapr <- datasetapr %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en las elecciones generales de 2019", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona"), skip_absent=TRUE) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `color`, `Primer problema`)  %>% 
  mutate(`Estado Civil` = NA) %>% 
  mutate(`Ingresos del hogar` = NA) %>% 
  mutate(`Ingresos de la persona` = NA) %>% 
  mutate(Total = "3245")
#----  

datasetmay <- read.spss("3247.sav", to.data.frame=TRUE)

datasetmay.labels <- as.data.frame(attr(datasetmay, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetmay, 'variable.labels')))

dedupnamesmay <- make.unique(datasetmay.labels$names)

names(datasetmay) <- dedupnamesmay

datasetmay <- setnames(datasetmay,1:2,c("ESTU","ID"))

demodatamay <- datasetmay %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en supuestas elecciones generales", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona")) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) 
#----  

datasetjun <- read.spss("3252.sav", to.data.frame=TRUE)

datasetjun.labels <- as.data.frame(attr(datasetjun, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetjun, 'variable.labels')))

dedupnamesjun <- make.unique(datasetjun.labels$names)

names(datasetjun) <- dedupnamesjun

datasetjun <- setnames(datasetjun,1:2,c("ESTU","ID"))

demodatajun <- datasetjun %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en supuestas elecciones generales", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona")) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) 

#----  

datasetjul <- read.spss("3257.sav", to.data.frame=TRUE)

datasetjul.labels <- as.data.frame(attr(datasetjul, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetjul, 'variable.labels')))

dedupnamesjul <- make.unique(datasetjul.labels$names)

names(datasetjul) <- dedupnamesjul

datasetjul <- setnames(datasetjul,1:2,c("ESTU","ID"))

demodatajul <- datasetjul %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en supuestas elecciones generales", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona")) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) 


#---------


datasetsep <- read.spss("3261.sav", to.data.frame=TRUE)

datasetsep.labels <- as.data.frame(attr(datasetsep, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetsep, 'variable.labels')))

dedupnamessep <- make.unique(datasetsep.labels$names)

names(datasetsep) <- dedupnamessep

datasetsep <- setnames(datasetsep,1:2,c("ESTU","ID"))

demodatasep <- datasetsep %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en supuestas elecciones generales", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos de la persona entrevistada"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona")) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) 

#-------


datasetoct <- read.spss("3267.sav", to.data.frame=TRUE)

datasetoct.labels <- as.data.frame(attr(datasetoct, "variable.labels")) %>% 
  mutate(names = as_character(attr(datasetoct, 'variable.labels')))

dedupnamesoct <- make.unique(datasetoct.labels$names)

names(datasetoct) <- dedupnamesoct

datasetoct <- setnames(datasetoct,1:2,c("ESTU","ID"))

demodataoct <- datasetoct %>% 
  setnames(c("ESTU", "Comunidad autónoma", "Provincia", "Tamaño de municipio",
             "Intención de voto en las elecciones generales de noviembre de 2019", "Sexo de la persona entrevistada",
             "Edad de la persona entrevistada", "Nivel de estudios alcanzado por la persona entrevistada",
             "Religiosidad de la persona entrevistada", "Estado civil de la persona entrevistada",
             "Ingresos del hogar", "Ingresos personales"),
           c("Total","Comunidad Autonoma", "Provincia", "Tamaño del Municipio",
             "Intención de Voto", "Sexo", "Edad", "Nivel de estudios",
             "Religión", "Estado Civil", "Ingresos del hogar",
             "Ingresos de la persona")) %>% 
  mutate(Edad=case_when(
    Edad %in% 18:24 ~ "18-24",
    Edad %in% 25:39 ~ "25-39",
    Edad %in% 40:64 ~ "40-64",
    Edad %in% 65:140 ~ "65-100"
  )) %>%
  left_join(tamunilables, by = "Tamaño del Municipio") %>%
  mutate(`Tamaño del Municipio` = tamun_iclean) %>%
  left_join(partido_labels, by = "Intención de Voto") %>%
  mutate(`Intención de Voto` = partido_clean) %>%
  left_join(estulables, by = "Nivel de estudios") %>%
  mutate(`Nivel de estudios` = estudios_clean) %>%
  left_join(incomehogar_lables, by = "Ingresos del hogar") %>%
  mutate(`Ingresos del hogar` = ingresospersonaclean) %>%
  left_join(incomeperso_lables, by ="Ingresos de la persona") %>%
  mutate(`Ingresos de la persona` = ingresospersonaclean) %>% 
  select(`Total`,`Comunidad Autonoma`, `Provincia`, `Tamaño del Municipio`,
         `Intención de Voto`, `Sexo`, `Edad`, `Nivel de estudios`,
         `Religión`, `Estado Civil`, `Ingresos del hogar`,
         `Ingresos de la persona`, `color`, `Primer problema`) %>% 
  mutate(Total = as.factor(Total))

#-------


trenddata <- rbind(demodataene, demodatafeb, demodatamar, demodataapr, demodatamay, demodatajun, demodatajul, demodatasep, demodataoct, demodata) %>% 
  # mutate_if(is.factor, as.character) %>% 
  mutate(ESTU = Total) %>% 
  mutate(Total = "Total") %>% 
  mutate(ESTU = as.character(ESTU)) %>% 
  mutate(ESTU = replace(ESTU, str_detect(ESTU, "3238"), "2019-01-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3240"), "2019-02-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3242"), "2019-03-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3245"), "2019-04-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3247"), "2019-05-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3252"), "2019-06-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3257"), "2019-07-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3261"), "2019-09-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3267"), "2019-11-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "3269"), "2019-12-1"),
         ESTU = replace(ESTU, str_detect(ESTU, "Diciembre 2019"), "2019-12-1")) %>% 
  mutate(ESTU = as.yearmon(as.Date(ESTU)))



megadata <- cbind(demodata, preocupadata)  
#megadata <- merge(demodata, preocupadata)
#tibble::enframe(names(megadata)) %>% count(value) %>% filter(n > 1)
megadata = megadata[unique(names(megadata))]


#--------------------------------------------------


wcscdataav <- preocupadata %>% 
  select(Problemas,  `Escala Ideológica`, Total) %>% 
  #filter(Total == input$grupo_seelcpreo) %>% 
  group_by(Problemas, `Escala Ideológica`) %>% 
  mutate(`Escala Ideológica` = as.character(`Escala Ideológica`)) %>% 
  replace(.=="1 Izquierda", "1") %>%
  replace(.== "10 Derecha", "10") %>%
  filter(`Escala Ideológica` != "N.S.") %>%
  filter(`Escala Ideológica` != "N.C.") %>%
  mutate(`Escala Ideológica` = as.numeric(`Escala Ideológica`)) %>% 
  group_by(Problemas) %>%
  summarise(mean_run = mean(`Escala Ideológica`))


wcscorder <- preocupadata %>% 
  select(Problemas,Total) %>% 
  # filter(Total == input$grupo_seelcpreo) %>% 
  group_by(Problemas) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  mutate(freq = freq * 100) %>% 
  mutate(Porcentaje = round(freq, digits = 0)) %>% 
  filter(Problemas != "N.C.") %>% 
  select(-freq, -n) %>% 
  left_join(wcscdataav, by = "Problemas") %>% 
  arrange(desc(Porcentaje)) %>% 
  slice(1:10) %>% 
  select(Problemas) %>% 
  unique() %>% 
  pull()

wcscdata <- preocupadata %>% 
  select(Problemas, Total) %>% 
  #filter(Total == input$grupo_seelcpreo) %>% 
  group_by(Problemas) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  mutate(freq = freq * 100) %>% 
  mutate(Porcentaje = round(freq, digits = 0)) %>% 
  filter(Problemas != "N.C.") %>% 
  select(-freq, -n) %>% 
  left_join(wcscdataav, by = "Problemas") %>% 
  arrange(desc(Porcentaje)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(Problemas = factor(Problemas, levels = wcscorder)) %>% 
  mutate(Problemas2 = Problemas)


#-------------------------------
save.image("../shiny CIS/data/data.RData")





