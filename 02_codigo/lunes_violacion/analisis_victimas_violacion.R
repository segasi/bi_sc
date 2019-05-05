### Importar y preprocesar datos ----
# source("02_codigo/cargar_preparar_datos_denunciantes.R")

# Nota: la base de datos de denuncias y víctimas elaborada por la PGJ de la CDMX solo incluye información para 2019

### Generar dataframe que solo incluya (i) denuncias por violación (ii) presentadas por víctimas. ----
den_violacion <- 
  denuncias %>% 
  mutate(categoria = str_replace(categoria, "Violacion", "Violación"),
         delito = str_replace(delito, "Violacion", "Violación")) %>% 
  filter(str_detect(categoria, "Violac"),
         str_detect(calidad_juridica, "Victima"),
         !is.na(edad)) 

### Frecuencia de denuncias presentadas por víctimas, por tipo de violación ----
den_violacion %>% 
  count(categoria, delito) %>% 
  arrange(delito) %>% 
  print(n = Inf)

### Frecuencia de denuncias presentadas por calidad jurídica de la víctima ----
den_violacion %>% 
  group_by(calidad_juridica) %>% 
  summarise(num = n()) %>% 
  ungroup()  %>% 
  mutate(por = (num/sum(num))*100)

### FFrecuencia de denuncias presentadas por víctimas, por sexo ---- 
den_violacion %>% 
  group_by(sexo) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  mutate(por = (num/sum(num))*100)

### Frecuencia de denuncias presentadas por víctimas, por calidad jurídica y sexo ---- 
den_violacion %>% 
  group_by(calidad_juridica, sexo) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  mutate(por = (num/sum(num))*100)

### Frecuencia de denuncias presentadas por víctimas, por tipo de violación y sexo ---- 
den_violacion %>% 
  group_by(delito, sexo) %>% 
  summarise(num = n()) %>% 
  ungroup()


### Frecuencia de denuncias presentadas por víctimas, por edad ---- 
denuncias %>% 
  mutate(categoria = str_replace(categoria, "Violacion", "Violación"),
         delito = str_replace(delito, "Violacion", "Violación")) %>% 
  filter(str_detect(categoria, "Violac"),
         str_detect(calidad_juridica, "Victima")) %>%
  # Calcular frecuencia de denuncias presentadas por edad de víctima
  group_by(edad) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(!is.na(edad)) %>% 
  # Calcular porcentaje por edad y porcentaje acumulado
  mutate(num_acumulado = cumsum(num),
         por = (num/sum(num))*100,
         por_acumulado = (num_acumulado/sum(num))*100) %>% 
  print(n = Inf)

### Histograma de frecuencia de denuncias presentadas por víctimas, por edad ----
den_violacion %>% 
  ggplot(aes(edad)) +
  geom_histogram(binwidth = 5, color = "white") +
  scale_x_continuous(breaks = seq(0, 90, 5)) +
  theme_minimal()


### Boxplot de frecuencia de denuncias presentadas por víctimas, por edad ----
den_violacion %>% 
  ggplot(aes(x = 1 , y = edad)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal()

### Frecuencia de denuncias presentadas por víctimas, por grupo quinquenal de edad ---- 
den_violacion %>% 
  mutate(gpo_edad = case_when(edad <  5 ~ "Menor de 5 años",
                              edad >= 5  & edad < 10 ~ "Entre 5 y 9",
                              edad >= 10  & edad < 15 ~ "Entre 10 y 14",
                              edad >= 15  & edad < 20 ~ "Entre 15 y 19",
                              edad >= 20  & edad < 25 ~ "Entre 20 y 24",
                              edad >= 25  & edad < 30 ~ "Entre 25 y 29",
                              edad >= 30  & edad < 35 ~ "Entre 30 y 34",
                              edad >= 35  & edad < 40 ~ "Entre 35 y 39",
                              edad >= 40  & edad < 45 ~ "Entre 40 y 44",
                              edad >= 45  & edad < 50 ~ "Entre 45 y 49",
                              edad >= 50  & edad < 55 ~ "Entre 50 y 54",
                              edad >= 55  & edad < 60 ~ "Entre 55 y 59",
                              edad >= 60  & edad < 65 ~ "Entre 60 y 64",
                              edad >= 65 ~ "Más de 65"),
         gpo_edad = fct_relevel(gpo_edad, "Menor de 5 años", "Entre 5 y 9")) %>% 
  group_by(gpo_edad) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  mutate(num_acumulado = cumsum(num),
         por = (num/sum(num))*100,
         por_acumulado = (num_acumulado/sum(num))*100) %>% 
  print(n = Inf)


### Histograma de denuncias presentadas por víctimas, por edad y sexo ----
den_violacion %>%
  ggplot(aes(edad)) +
  geom_histogram(binwidth = 5, color = "white") +
  scale_x_continuous(breaks = seq(0, 90, 5)) +
  facet_wrap(~ sexo) +
  theme_minimal()


### Análisis espacial ----

### Importar shapefiles de alcaldías y colonias ----

# Shapefile de alcaldías - Fuente: Agencia Digital de Innovación Pública de la CDMX
alcaldias_sf <- st_read("01_datos/shp/alcaldias/alcaldias.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(.,  crs = 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')

# Shapefile de colonias - Fuente: Agencia Digital de Innovación Pública de la CDMX
colonias_sf <- st_read("01_datos/shp/colonias/colonias.shp", stringsAsFactors = FALSE, quiet = TRUE)  %>% 
  st_transform(., 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')

# Generar objeto "sf" para después calcular la frecuencia de denuncias por alcaldía ----
den_violacion_sf <- 
  den_violacion %>% 
  filter(!is.na(lat)) %>%  # Perdemos 20 casos de víctimas menores a 18 años de edad por falta de coordenadas
  st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(4326) %>%   
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs') 

# Mapa de las denuncias presentadas por víctimas de violación en 2019 ----
den_violacion_sf %>% 
  filter(edad < 18) %>% 
  ggplot() +
  geom_sf(color = "steelblue") +
  geom_sf(data = alcaldias_sf, color = "grey40", fill = NA, size = 0.5) +
  coord_sf(xlim = c(-99.5, -98.8), datum = NA) 


### Frecuencia de denuncias de violación presentadas por víctimas de menos de 18 año, por alcaldía ----
resultados_hd <- 
  st_within(den_violacion_sf %>%
              filter(edad < 18), alcaldias_sf, sparse = FALSE) # Código adaptado de https://stackoverflow.com/questions/45899768/r-cloropleth-of-the-data-points-that-fall-within-a-polygon-what-percentage-h 


alcaldias_sf %>%
  mutate(num_carpetas = apply(resultados_hd, 2, sum)) %>% 
  arrange(-num_carpetas)

