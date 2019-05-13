### Importar y preprocesar datos ----
# source("02_codigo/cargar_preparar_datos_carpetas_investigacion.R")

### Frecuencia de homicidios dolosos, por tipo ----
bd_cdmx %>% 
  filter(categoria_de_delito == "Homicidio doloso") %>%
  count(delito)

### Frecuencia de homicidios dolosos, por calle, colonia y alcaldía ----
bd_cdmx %>% 
  filter(str_detect(categoria_de_delito, "Homicidio doloso"),
         !is.na(colonia)) %>% 
  mutate(calle1 = str_trim(calle1),
         calle1 = str_squish(calle1)) %>% 
  group_by(alcaldia, colonia, calle1) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  arrange(-num_ci)  %>% 
  print(n = 50)
