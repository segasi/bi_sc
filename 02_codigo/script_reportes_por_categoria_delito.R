### Paquetes ----
library(pacman)
p_load(cowplot, extrafont, ggcal, ggrepel, grid, gridExtra, ineq, janitor, kableExtra, knitr, lubridate, readxl, rmarkdown, scales, sf, tidyverse, treemapify, wesanderson, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(color = "grey35"),
        plot.title = element_text(size = 16, face = "bold", margin = margin(10,0,10,0), color = "grey25"),
        plot.subtitle = element_text(size = 12, face = "bold", colour = "#666666", margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(hjust = 0, size = 9),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title.align = 0.5,
        axis.title = element_text(size = 14, hjust = 1, face = "bold", margin = margin(0,0,0,0)),
        axis.text = element_text(size = 12),
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 14))

### Importar shapefiles de alcaldías y colonias ----

# Shapefile de alcaldías - Fuente: Agencia Digital de Innovación Pública de la CDMX
alcaldias_sf <- st_read("01_datos/shp/alcaldias/alcaldias.shp", stringsAsFactors = FALSE, quiet = TRUE) %>% 
  st_transform(.,  crs = 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')

# Shapefile de colonias - Fuente: Agencia Digital de Innovación Pública de la CDMX
colonias_sf <- st_read("01_datos/shp/colonias/colonias.shp", stringsAsFactors = FALSE, quiet = TRUE)  %>% 
  st_transform(., 4326) %>% 
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs')


### Importar datos de población por alcaldía ----

# Fuente: Anuario estadístico y geográfico de la Ciudad de México 2017, INEGI + CDMX, url: http://internet.contenidos.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/actualizacion_continua/702825094683.zip (consultada el 14 de marzo)

datos_poblacion_alcaldias <- 
  read_excel("01_datos/inegi/anuario_estadistico_2017/702825094683/c09_03.xls", 
             sheet = "3.36", range = "a28:g43", col_names = F) %>% 
  select(-c("..2", "..3", "..4")) %>% 
  rename(alcaldia = "..1",
         pob_total = "..5",
         pob_hombres = "..6",
         pob_mujeres = "..7") %>% 
  mutate(alcaldia = case_when(alcaldia == "Cuajimalpa de Morelos" ~ "Cuajimalpa",
                              alcaldia == "La Magdalena Contreras" ~ "Magdalena Contreras",
                              TRUE ~ alcaldia))


### Importar datos de población de la CDMX ----

# Fuente: Población a mitad de año. Para la República Mexicana el periodo es de 1950-2050, para las entidades federativas el periodo es de 1970-2050, CONAPO, url: http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv (consultada el 14 de marzo)

# Importar

datos_poblacion <- 
  read_delim("01_datos/conapo/pob_mit_proyecciones.csv", "," , locale = locale(encoding = "latin1")) 

# Calcular valores anuales 
datos_poblacion_cdmx <- 
  datos_poblacion %>% 
  clean_names() %>% 
  filter(ano > 2015 & ano < 2020, 
         entidad == "Ciudad de México") %>% 
  group_by(ano) %>% 
  summarise(pob_tot = sum(poblacion)) %>% 
  ungroup()

### Importar datos de carpetas de investigación ----
bd <- read_delim("01_datos/adip/carpetas-de-investigacion-pgj-cdmx.csv", 
                 ";", 
                 escape_double = FALSE, 
                 col_types = cols(`Fecha inicio` = col_character()), 
                 trim_ws = TRUE) %>% 
  clean_names() # "Limpiar" nombre de columnas

### Cambiar tipo a variable fecha_inicio para que sea dttm ----

# Tengo que hacer esto en dos pasos porque el formato de fecha-hora usado en las observaciones de 2016 a 2018 es diferente al usado en 2019
bd <- 
  bd %>% 
  mutate(fecha_inicio = case_when(ano == 2019 & mes %in% c("Enero", "Febrero") ~ dmy_hm(fecha_inicio), 
                                  TRUE ~ ymd_hms(fecha_inicio)))

### Reordenar niveles de la variable mes ----
bd <- 
  bd %>% 
  mutate(mes = fct_relevel(mes, 
                           "Enero", "Febrero", "Marzo", "Abril",
                           "Mayo", "Junio", "Julio", "Agosto",
                           "Septiembre", "Octubre", "Noviembre", "Diciembre"))

### Cambiar valores de diversas variables a mayúscula (primera letra) y nminúsculas (resto de las letras) ----
bd <- 
  bd %>% 
  mutate(alcaldia = str_to_title(alcaldia),
         categoria_de_delito = str_to_sentence(categoria_de_delito),
         delito = str_to_sentence(delito))

### Generar variable para registrar en qué día de la semana se abrió la carpeta de investigación, y otra para registrar el día del año al que corresponde cada día calendario ----

# Tomamos dia_semana como proxy de la fecha en que ocurrió el presunto delito
bd <- 
  bd %>% 
  mutate(dia_semana = wday(fecha_inicio, 
                           week_start = getOption("lubridate.week.start", 1), # Especificar que semana empieza en lunes, no en domingo (el default)
                           locale = Sys.getlocale("LC_TIME")),
         dia_ano = yday(fecha_inicio)) 

### Generar versión en texto del día de la semana se abrió la carpeta de investigación ----
bd <- 
  bd %>% 
  mutate(dia_semana_texto = case_when(dia_semana == 1 ~ "Lun",
                                      dia_semana == 2 ~ "Mar",
                                      dia_semana == 3 ~ "Mié",
                                      dia_semana == 4 ~ "Jue",
                                      dia_semana == 5 ~ "Vie",
                                      dia_semana == 6 ~ "Sáb",
                                      dia_semana == 7 ~ "Dom"),
         dia_semana_texto = fct_relevel(dia_semana_texto, "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom"))


### Generar dataframe con presuntos delitos cometidos en las alcaldías de la CDMX ----
bd_cdmx <- 
  bd %>% 
  filter(alcaldia %in% c("Alvaro Obregon", "Azcapotzalco", "Benito Juarez", "Coyoacan", "Cuajimalpa De Morelos", "Cuauhtemoc", "Gustavo A Madero", "Iztacalco", "Iztapalapa", "La Magdalena Contreras", "Miguel Hidalgo", "Milpa Alta", "Tlahuac", "Tlalpan", "Venustiano Carranza", "Xochimilco"))

### Corregir nombres de alcaldías de la CDMX ----
bd_cdmx <- 
  bd_cdmx %>% 
  mutate(alcaldia = case_when(alcaldia == "Alvaro Obregon" ~ "Álvaro Obregón",
                              alcaldia == "Benito Juarez" ~ "Benito Juárez",
                              alcaldia == "Coyoacan" ~ "Coyoacán",
                              alcaldia == "Cuajimalpa De Morelos" ~ "Cuajimalpa",
                              alcaldia == "Cuauhtemoc" ~ "Cuauhtémoc",
                              alcaldia == "Gustavo A Madero" ~ "Gustavo A. Madero",
                              alcaldia == "La Magdalena Contreras" ~ "Magdalena Contreras",
                              alcaldia == "Tlahuac" ~ "Tláhuac",
                              TRUE ~ alcaldia))

### Construir una versión de la fecha de inicio de las carpetas en texto ----
bd_cdmx <- 
  bd_cdmx %>% 
  mutate(dia = day(fecha_inicio), 
         fecha_texto = str_c(dia, "de", str_to_lower(mes), "de", ano, sep = " "))

### Agregar comas a algunos nombres de categorías de delito ----
bd_cdmx <- 
  bd_cdmx %>% 
  mutate(categoria_de_delito = str_replace(categoria_de_delito, " con y sin v", ", con y sin v"),
         categoria_de_delito = str_replace(categoria_de_delito, " con v", ", con v"))


### Generar informes en PDF ----

# Generar lista de categoría de delitos
id_list <- 
  bd_cdmx %>%
  distinct(categoria_de_delito) %>%
  filter(!str_detect(categoria_de_delito, "bajo impac|no deli|ecu")) %>% 
  pull()

# Loop para 13 categorías de delitos ----
for (i in seq_along(id_list)) {
  newdir <- paste(id_list[i], sep='_')
  
  # Selección de categoría de delito para hacer informes
  datos_delito <- 
    bd_cdmx %>% 
    filter(categoria_de_delito == id_list[i])
  
  knit_meta(class = NULL, clean = TRUE)
  
  # Para reportes pdf
  render(input = "02_codigo/reporte_por_delito.Rmd",
         # output_format = "pdf_document",
         output_file = paste("reporte_de_", str_replace_all(str_replace_all(str_to_lower(newdir), " ", "_"), ",", ""),".pdf", sep=''),
         output_dir = "04_reportes/")
}

# Loop para secuestro ----

# Necesitamos un loop por separado porque la base de datos no incluye carpetas de investigación por secuestro en 2018. Esto provoca que el código y la redacción tengan que ser ligeramente diferentes.

lista_delitos <-
  bd_cdmx %>%
  distinct(categoria_de_delito) %>%
  filter(str_detect(categoria_de_delito, "Secue"))

id_list <- lista_delitos$categoria_de_delito

# Loop 
for (i in seq_along(id_list)) {
  newdir <- paste(id_list[i], sep='_')
  
  # Selección de categoría de delito para hacer informes
  datos_delito <- 
    bd_cdmx %>% 
    filter(categoria_de_delito == id_list[i])
  
  knit_meta(class = NULL, clean = TRUE)
  
  # # Para reportes pdf
  render(input = "02_codigo/reporte_secuestro.Rmd",
         # output_format = "pdf_document",
         output_file = paste("reporte_de_", str_replace_all(str_replace_all(str_to_lower(newdir), " ", "_"), ",", ""),".pdf", sep=''),
         output_dir = "04_reportes/")
}



# Loop para violencia doméstica ----

# Necesitamos un loop por separado porque este es un delito, no una categoría de delito. 

foo <- 
  bd_cdmx %>% 
  filter(categoria_de_delito == "Delito de bajo impacto",
         delito == "Violencia familiar") %>% 
  mutate(categoria_de_delito = "Violencia familiar")

id_list <- "Violencia familiar"



# Loop 
for (i in seq_along(id_list)) {
  newdir <- paste(id_list[i], sep='_')
  
  # Selección de categoría de delito para hacer informes
  datos_delito <- foo 
  
  knit_meta(class = NULL, clean = TRUE)
  
  # Para reportes pdf
  render(input = "02_codigo/reporte_violencia_familiar.Rmd",
         # output_format = "pdf_document",
         output_file = paste("reporte_de_", str_replace_all(str_replace_all(str_to_lower(newdir), " ", "_"), ",", ""),".pdf", sep=''),
         output_dir = "04_reportes/")
}

