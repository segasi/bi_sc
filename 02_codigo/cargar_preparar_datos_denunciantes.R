### Paquetes ----
library(pacman)
p_load(cowplot, extrafont, ggcal, ggrepel, ineq, janitor, lubridate, readxl, scales, sf, tidyverse, treemapify, wesanderson, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family = "Archivo Narrow Bold", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15, family = "Archivo Narrow"),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family = "Archivo Narrow Bold"),
        legend.text = element_text(size = 14, family = "Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family = "Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family = "Didact Gothic Regular"),
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 14))

### Importar datos ----
denuncias <- read_delim("01_datos/adip/denuncias-victimas-pgj.csv", 
                        ";", 
                        escape_double = FALSE, 
                        col_types = cols(`Mes_año_hecho` = col_character()), 
                        trim_ws = TRUE) %>% 
  clean_names() # "Limpiar" nombre de columnas

### Reordenar niveles de la variable mes_hecho ----
denuncias <- 
  denuncias %>% 
  mutate(mes_hecho = fct_relevel(mes_hecho, 
                                 "Enero", "Febrero", "Marzo", "Abril",
                                 "Mayo", "Junio", "Julio", "Agosto",
                                 "Septiembre", "Octubre", "Noviembre", "Diciembre"))


### Cambiar tipo de la variable fecha_hecho para que se dttm ----
denuncias <- 
  denuncias %>% 
  mutate(fecha_hecho = dmy_hm(fecha_hecho))

### Cambiar valores de diversas variables a mayúscula (primera letra) y nminúsculas (resto de las letras) ----
denuncias <- 
  denuncias %>% 
  mutate(delito = str_to_sentence(delito),
         categoria = str_to_sentence(categoria),
         calidad_juridica = str_to_sentence(calidad_juridica))