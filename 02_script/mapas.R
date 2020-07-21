### Configuración inicial ----
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, scales, readxl, sf)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Bases y manipulación de base ----

## Importar base de prevalencia (INEGI)
base <- read_excel("1_data/a_peq_prev_2018.xlsx", 
                              skip = 2)
base <- base %>% 
  filter(Estimador == "Valor",
         `Municipio o delegación` != "Total") %>% 
  rename(id = `Identificador único del municipio`,
         mpio = `Municipio o delegación`,
         obesidad = `Porcentaje de población de 20 años y más con obesidad.`,
         hipertension = `Porcentaje de población de 20 años y más con diagnóstico previo de hipertensión.`,
         diabetes = `Porcentaje de población de 20 años y más con diagnóstico previo de diabetes.`) %>% 
  select(id, mpio, obesidad, hipertension, diabetes)


## Base covid

heat <- read_csv("1_data/datos_abiertos_covid19/200719COVID19MEXICO.csv")

covid <- read_csv("1_data/datos_abiertos_covid19/200719COVID19MEXICO.csv")

# Transformación por edades
brks <- c(0,10,20,30,40,50,60,70,80,90,100,
          max(data$EDAD, na.rm = TRUE))

# Creación de variable i
covid <- covid %>%
  filter(ENTIDAD_RES != "97",
         ENTIDAD_RES != "98",
         ENTIDAD_RES != "99") %>% 
  mutate(id = str_c(ENTIDAD_RES, MUNICIPIO_RES, sep = "")) %>% 
  filter(FECHA_DEF != is.na(FECHA_DEF),
         RESULTADO == 1) %>% 
  count(id)

# Primera unión
bd <- left_join(base, covid, by = "id")


## Base de población
pob <- read_csv("1_data/mapa_imp_predial.csv")

pob <- pob %>% 
  mutate(id = str_c(ID_ENTIDAD_FEDERATIVA, ID_MUNICIPIO, sep = "")) %>% 
  filter(CICLO == 2019) %>% 
  select(id, POBLACION)

# Segunda unión
data <- left_join(bd, pob, by = "id")


## Base de mapas
mex_map <- st_read("C:/Users/pablo/RStudio/2020/trabajo/1_data/01_32_mun/01_32_mun.shp")

edos <- st_read("C:/Users/pablo/RStudio/2020/trabajo/1_data/dest_2015gw/dest_2015gw.shp")

mapa <- mex_map %>% 
  mutate(id = CVEGEO) 

# Tercera unión
data <- left_join(mapa, data, by = "id")



### Creación de variables ----

## Conteo per cápita de vodi

data <- data %>% 
  mutate_at(vars(n), ~replace(., is.na(.), 0))

data <- data %>% 
  mutate(covid = (n/POBLACION) * 1e6)


## Estimaciones de IQR para variables

# Obesidad
ob_iqr <- IQR(data$obesidad, na.rm = T)
ob_q1 <- quantile(data$obesidad, .25, na.rm = T)
ob_q3 <- quantile(data$obesidad, .75, na.rm = T)

ob_min <- ob_q1 - (1.5*ob_iqr)
ob_max <- ob_q3 + (1.5*ob_iqr)

# Diabetes
dia_iqr <- IQR(data$diabetes, na.rm = T)
dia_q1 <- quantile(data$diabetes, .25, na.rm = T)
dia_q3 <- quantile(data$diabetes, .75, na.rm = T)

dia_min <- dia_q1 - (1.5*dia_iqr)
dia_max <- dia_q3 + (1.5*dia_iqr)

# Hipertensión
hip_iqr <- IQR(data$hipertension, na.rm = T)
hip_q1 <- quantile(data$hipertension, .25, na.rm = T)
hip_q3 <- quantile(data$hipertension, .75, na.rm = T)

hip_min <- hip_q1 - (1.5*hip_iqr)
hip_max <- hip_q3 + (1.5*hip_iqr)

# Covid
covid_iqr <- IQR(data$covid, na.rm = T)
covid_q1 <- quantile(data$covid, .25, na.rm = T)
covid_q3 <- quantile(data$covid, .75, na.rm = T)

covid_min <- covid_q1 - (1.5*ob_iqr)
covid_max <- covid_q3 + (1.5*ob_iqr)


### Mapas ----

## Hipertensión
brks <- c(min(data$hipertension, na.rm = TRUE),
          10,20,30,
          max(data$hipertension, na.rm = TRUE))

data %>%
  mutate(casos_cut = cut(hipertension, breaks = brks, right = FALSE)) %>%
  filter(casos_cut != is.na(casos_cut)) %>% 
  ggplot() + 
  geom_sf(aes(fill = casos_cut),
          colour = "transparent", size = 0.001) + 
  scale_fill_brewer(palette = "Blues") +
  geom_sf(color = "grey0",
          fill = "transparent",
          data = edos) +
  labs(title = "Prevalencia de hipertensión a nivel municipal en México",
       subtitle = "Población a partir de los 20 años",
       fill = "Población con\nhipertensión (%)",
       caption = "Elaborado por @pCobosAlcala con datos de la ENSANUT 2018 (INEGI)") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.83, 0.80),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())

ggsave("hipertension.png",
       width = 8,
       height = 6,
       dpi = 800)


### Obesidad ----

brks <- c(min(data$obesidad, na.rm = TRUE),
          10,20,30,40,50,
          max(data$obesidad, na.rm = TRUE))

data %>%
  mutate(casos_cut = cut(obesidad, breaks = brks, right = FALSE)) %>%
  filter(casos_cut != is.na(casos_cut)) %>% 
  ggplot() + 
  geom_sf(aes(fill = casos_cut),
          colour = "transparent", size = 0.001) + 
  scale_fill_brewer(palette = "Reds") +
  geom_sf(color = "grey0",
          fill = "transparent",
          data = edos) +
  labs(title = "Prevalencia de obesidad a nivel municipal en México",
       subtitle = "Población a partir de los 20 años",
       fill = "Población con\nobesidad (%)",
       caption = "Elaborado por @pCobosAlcala con datos de la ENSANUT 2018 (INEGI)") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.83, 0.75),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())

ggsave("obesidad.png",
       width = 8,
       height = 6,
       dpi = 800)


### Diabetes ----
brks <- c(min(data$diabetes, na.rm = TRUE),
          5, 10,15,20,
          max(data$diabetes, na.rm = TRUE))

data %>%
  mutate(casos_cut = cut(diabetes, breaks = brks, right = FALSE)) %>%
  filter(casos_cut != is.na(casos_cut)) %>% 
  ggplot() + 
  geom_sf(aes(fill = casos_cut),
          colour = "transparent", size = 0.001) + 
  scale_fill_brewer(palette = "Purples") +
  geom_sf(color = "grey0",
          fill = "transparent",
          data = edos) +
  labs(title = "Prevalencia de diabetes a nivel municipal en México",
       subtitle = "Población a partir de los 20 años",
       fill = "Población con\ndiabetes (%)",
       caption = "Elaborado por @pCobosAlcala con datos de la ENSANUT 2018 (INEGI)") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = c(0.83, 0.75),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())

ggsave("diabetes.png",
       width = 8,
       height = 6,
       dpi = 800)


### Correlaciones ----

# Hipertensión
data %>% 
  filter(obesidad > ob_min,
         obesidad < ob_max) %>% 
  filter(covid > covid_min,
         covid < covid_max,
         covid > 0,
         diabetes > dia_min,
         diabetes < dia_max) %>% 
  filter(hipertension > hip_min,
         hipertension < hip_max) %>% 
  ggplot() +
  geom_point(aes(x = hipertension/100,
                 y = covid),
             alpha = 0.3,
             color = "steelblue") +
  scale_x_continuous(label = percent_format(accuracy = 2)) +
  labs(title = "Relación entre covid-19 e hipertensión a nivel municipal en México",
       subtitle = "Corte: 19 de julio de 2020",
       x = "Porcentaje de población a partir de 20 años con hipertensión",
       y = "Defunciones confirmadas de covid-19\npor millón de habitantes",
       caption = "Nota: esta gráfica no visualiza valores atípicos ni municipios sin defunciones.\n\nElaborado por @pCobosAlcala con datos de la ENSANUT 2018 (INEGI) y la Secretaría de Salud.") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

ggsave("hip.png",
       width = 8,
       height = 6,
       dpi = 800)

# Obesidad
data %>% 
  filter(obesidad > ob_min,
         obesidad < ob_max) %>% 
  filter(covid > covid_min,
         covid < covid_max,
         covid > 0,
         diabetes > dia_min,
         diabetes < dia_max) %>% 
  filter(hipertension > hip_min,
         hipertension < hip_max) %>% 
  ggplot() +
  geom_point(aes(x = obesidad/100,
                 y = covid),
             alpha = 0.3,
             color = "red") +
  scale_x_continuous(label = percent_format(accuracy = 2)) +
  labs(title = "Relación entre covid-19 y obesidad a nivel municipal en México",
       subtitle = "Corte: 19 de julio de 2020",
       x = "Porcentaje de población a partir de 20 años con obesidad",
       y = "Defunciones confirmadas de covid-19\npor millón de habitantes",
       caption = "Nota: esta gráfica no visualiza valores atípicos ni municipios sin defunciones.\n\nElaborado por @pCobosAlcala con datos de la ENSANUT 2018 (INEGI) y la Secretaría de Salud.") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

ggsave("obe.png",
       width = 8,
       height = 6,
       dpi = 800)

# Diabetes
data %>% 
  filter(obesidad > ob_min,
         obesidad < ob_max) %>% 
  filter(covid > covid_min,
         covid < covid_max,
         covid > 0,
         diabetes > dia_min,
         diabetes < dia_max) %>% 
  filter(hipertension > hip_min,
         hipertension < hip_max) %>% 
  ggplot() +
  geom_point(aes(x = diabetes/100,
                 y = covid),
             alpha = 0.3,
             color = "purple") +
  scale_x_continuous(label = percent_format(accuracy = 2)) +
  labs(title = "Relación entre covid-19 y diabetes a nivel municipal en México",
       subtitle = "Corte: 19 de julio de 2020",
       x = "Porcentaje de población a partir de 20 años con diabetes",
       y = "Defunciones confirmadas de covid-19\npor millón de habitantes",
       caption = "Nota: esta gráfica no visualiza valores atípicos ni municipios sin defunciones.\n\nElaborado por @pCobosAlcala con datos de la ENSANUT 2018 (INEGI) y la Secretaría de Salud.") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0))

ggsave("dia.png",
       width = 8,
       height = 6,
       dpi = 800)


# Correlaciones
cor <- data %>% 
  filter(obesidad > ob_min,
         obesidad < ob_max) %>% 
  filter(covid > covid_min,
         covid < covid_max,
         covid > 0,
         diabetes > dia_min,
         diabetes < dia_max) %>% 
  filter(hipertension > hip_min,
         hipertension < hip_max)

cor.test(cor$hipertension, cor$covid)
cor.test(cor$obesidad, cor$covid)
cor.test(cor$diabetes, cor$covid)


### Heatmap----

brks <- c(seq(0, 90,10),
              max(heat$EDAD, na.rm = TRUE))

heat %>%
  mutate(edad_cut = cut(EDAD, breaks = brks, right = FALSE)) %>% 
  filter(RESULTADO == 1,
         TIPO_PACIENTE == 2,
         FECHA_INGRESO <= as.Date("2020-07-05")) %>% 
  select(ID_REGISTRO, FECHA_DEF, edad_cut, DIABETES:TABAQUISMO) %>% 
  mutate(FECHA_DEF = if_else(is.na(FECHA_DEF),
                             2,1)) %>% 
  mutate(SIN = if_else(DIABETES == 2 &
                         EPOC == 2 &
                         ASMA == 2 &
                         INMUSUPR == 2 &
                         OTRA_COM == 2 &
                         CARDIOVASCULAR == 2 &
                         OBESIDAD == 2 &
                         RENAL_CRONICA == 2 &
                         TABAQUISMO == 2,
                       1,2)) %>% 
  pivot_longer(DIABETES:SIN,
               names_to = "com",
               values_to = "valor") %>%
  filter(valor == 1,
         edad_cut != is.na(edad_cut)) %>% 
  filter(FECHA_DEF == 1) %>% 
  group_by(edad_cut, com) %>% 
  count() %>% 
  ggplot() +
  geom_tile(aes(x = edad_cut,
                y = factor(com, order = T,
                           levels = c("SIN",
                                      "TABAQUISMO",
                                      "OTRA_COM",
                                      "OBESIDAD",
                                      "RENAL_CRONICA",
                                      "INMUSUPR",
                                      "HIPERTENSION",
                                      "CARDIOVASCULAR",
                                      "EPOC",
                                      "DIABETES",
                                      "ASMA")),
                fill = n)) +
  geom_text(aes(x = edad_cut,
                y = factor(com, order = T,
                           levels = c("SIN",
                                      "TABAQUISMO",
                                      "OTRA_COM",
                                      "OBESIDAD",
                                      "RENAL_CRONICA",
                                      "INMUSUPR",
                                      "HIPERTENSION",
                                      "CARDIOVASCULAR",
                                      "EPOC",
                                      "DIABETES",
                                      "ASMA")),
                label = n), size = 2.5,
            color = "grey0") +
  scale_y_discrete(labels = c("Sin comorbilidad",
                                "Tabaquismo",
                                "Otra",
                                "Obesidad",
                              "Ins. renal crónica",
                                "Inmunosupresión",
                                "Hipertensión",
                                "EPOC",
                              "Enf. cardiovascular",
                                "Diabetes",
                                "Asma")) +
  scale_fill_gradient2(low = "white",
                       high = "darkgoldenrod4",
                       label = comma) +
  scale_x_discrete(labels = c("0-9",
                              "10-19",
                              "20-29",
                              "30-39",
                              "40-49",
                              "50-59",
                              "60-69",
                              "70-79",
                              "80-89",
                              "90-120")) +
  labs(title = "Fallecimientos confirmados de covid-19 por tipo de comorbilidad",
       subtitle = "Personas cuya hospitalización comenzó antes del 6 de julio",
       x = "Rango de edad",
       y = "Comorbilidad",
       fill = "Fallecimientos",
       caption = "Nota: como una persona puede tener dos o más comorbilidades, el cálculo de fallecimientos es por persona-comorbilidad.\n\nElaborado por @pCobosAlcala con datos de la Secretaría de Salud.") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   hjust = 1,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))


ggsave("com.png",
       width = 8,
       height = 6,
       dpi = 750)


heat %>%
  mutate(edad_cut = cut(EDAD, breaks = brks, right = FALSE)) %>% 
  filter(RESULTADO == 1,
         TIPO_PACIENTE == 2,
         TABAQUISMO == 1,
         FECHA_DEF != is.na(FECHA_DEF),
         FECHA_INGRESO <= as.Date("2020-07-06")) %>% 
  count(edad_cut)
  
  
