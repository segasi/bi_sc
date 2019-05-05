### Importar y preprocesar datos ----
# source("02_codigo/cargar_preparar_datos_carpetas_investigacion.R")

# Frecuencia de CI por cada tipo de violación y año ----
bd_cdmx %>% 
  filter(categoria_de_delito == "Violación") %>%
  group_by(ano, delito) %>%
  summarise(num = n()) %>% 
  ungroup()

# Frecuencia de CI de violaciones tumultuarias por año ----
bd_cdmx %>% 
  filter(str_detect(delito, "tumul")) %>%
  group_by(ano) %>%
  summarise(num = n()) %>% 
  ungroup() %>% 
  arrange(-num)

# Frecuencia de CI de violaciones tumultuarias, primer trimestre de cada año ----
bd_cdmx %>%
  mutate(trimestre = case_when(mes %in% c("Enero", "Febrero", "Marzo") ~ "Primero",
                               TRUE ~ "Otros")) %>%
  filter(str_detect(delito, "tumul"),
         trimestre == "Primero") %>% 
  group_by(ano) %>%
  summarise(num = n()) %>% 
  ungroup()

# Frecuencia de CI de violaciones tumultuarias en 2019 por alcaldía ----
bd_cdmx %>% 
  filter(str_detect(delito, "tumul"), 
         ano == 2019) %>%
  group_by(ano, alcaldia) %>%
  summarise(num = n()) %>% 
  ungroup() %>% 
  arrange(-num)


# Frecuencia de CI de violaciones equiparadas por año ----
bd_cdmx %>% 
  filter(str_detect(delito, "equiparada")) %>%
  group_by(ano) %>%
  summarise(num = n()) %>% 
  ungroup()


# Frecuencia de CI de violaciones equiparadas, primer trimestre de cada año ----
bd_cdmx %>%
  mutate(trimestre = case_when(mes %in% c("Enero", "Febrero", "Marzo") ~ "Primero",
                               TRUE ~ "Otros")) %>%
  filter(str_detect(delito, "equiparada"),
         trimestre == "Primero") %>% 
  group_by(ano) %>%
  summarise(num = n()) %>% 
  ungroup()
select(delito)


# Frecuencia de CI de violaciones equiparada en 2019 por alcaldía ----
bd_cdmx %>% 
  filter(str_detect(delito, "equipara"), 
         ano == 2019) %>%
  group_by(ano, alcaldia) %>%
  summarise(num = n()) %>% 
  ungroup() %>% 
  arrange(-num)
