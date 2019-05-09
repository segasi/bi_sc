### Importar y preprocesar datos ----
# source("02_codigo/cargar_preparar_datos_carpetas_investigacion.R")

### Frecuencia de CI por robo a pasajero a bordo de microbus por calle ----
bd_cdmx %>% 
  filter(str_detect(categoria_de_delito, "micro")) %>% 
  mutate(calle1 = iconv(calle1, to='ASCII//TRANSLIT'), # Quitar acentos
         calle1 = str_replace(calle1, "AV. ", "AVENIDA "),
         calle1 = str_replace(calle1, "CALZ.", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADA GRAL. IGNACIO ZARAGOZA", "CALZADA GENERAL IGNACIO ZARAGOZA"),
         calle1 = str_replace(calle1, "CALZADA GENERAL IGNACIO ZARAGOZA", "CALZADA IGNACIO ZARAGOZA"),
         calle1 = str_replace(calle1, "EJE 8 SUR (CALZADA ERMITA IZTAPALAPA)", "CALZADA ERMITA IZTAPALAPA"),
         calle1 = case_when(str_detect(calle1, "CALZADA ERMITA IZTAPALAPA") ~ "CALZADA ERMITA IZTAPALAPA",
                            str_detect(calle1, "AVENIDA ERMITA IZTAPALAPA") ~ "CALZADA ERMITA IZTAPALAPA",
                            str_detect(calle1, "FRANCISCO DEL PASO Y TRONCOSO") ~ "FRANCISCO DEL PASO Y TRONCOSO",
                            TRUE ~ calle1),
         calle1 = str_trim(calle1),
         calle1 = str_squish(calle1)) %>% 
  group_by(calle1) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  arrange(-num_ci) %>% 
  mutate(por = (num_ci/sum(num_ci))*100,
         por_acum = cumsum(por))


### Frecuencia de CI por robo a pasajero a bordo de microbus por calle, colonia y alcaldía ----
bd_cdmx %>% 
  filter(str_detect(categoria_de_delito, "micro")) %>% 
  mutate(calle1 = iconv(calle1, to='ASCII//TRANSLIT'), # Quitar acentos
         calle1 = str_replace(calle1, "AV. ", "AVENIDA "),
         calle1 = str_replace(calle1, "CALZ.", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADA GRAL. IGNACIO ZARAGOZA", "CALZADA GENERAL IGNACIO ZARAGOZA"),
         calle1 = str_replace(calle1, "CALZADA GENERAL IGNACIO ZARAGOZA", "CALZADA IGNACIO ZARAGOZA"),
         calle1 = str_replace(calle1, "EJE 8 SUR (CALZADA ERMITA IZTAPALAPA)", "CALZADA ERMITA IZTAPALAPA"),
         calle1 = case_when(str_detect(calle1, "CALZADA ERMITA IZTAPALAPA") ~ "CALZADA ERMITA IZTAPALAPA",
                            str_detect(calle1, "AVENIDA ERMITA IZTAPALAPA") ~ "CALZADA ERMITA IZTAPALAPA",
                            str_detect(calle1, "FRANCISCO DEL PASO Y TRONCOSO") ~ "FRANCISCO DEL PASO Y TRONCOSO",
                            TRUE ~ calle1),
         calle1 = str_trim(calle1),
         calle1 = str_squish(calle1)) %>% 
  group_by(alcaldia, colonia, calle1) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  arrange(-num_ci) %>% 
  mutate(por = (num_ci/sum(num_ci))*100,
         por_acum = cumsum(por))