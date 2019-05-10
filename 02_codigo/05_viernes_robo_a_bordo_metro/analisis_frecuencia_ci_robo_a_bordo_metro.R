### Importar datos de afluencia por estación ----
# Fuente: https://metro.cdmx.gob.mx/afluencia-de-estacion-por-linea-2018

afluencia <- read_excel("01_datos/metro/afluencia_estaciones_metro_2018.xlsx")

### Calcular estaciones con mayor afluencia ----
afluencia %>% 
  group_by(estacion) %>% 
  summarise(afluencia_total = sum(afluencia)) %>% 
  ungroup() %>% 
  arrange(-afluencia_total) %>% 
  print(n = 20)


### Frecuencia de CI por estación del metro
bd_cdmx %>% 
  filter(str_detect(categoria_de_delito, "metro")) %>% 
  mutate(calle1 = str_trim(calle1),
         calle1 = str_squish(calle1),
         calle1 = str_replace(calle1, "AV\\.", "AVENIDA"),
         calle1 = str_replace(calle1, "AV\\,", "AVENIDA"),
         calle1 = str_replace(calle1, "AV ", "AVENIDA "),
         calle1 = str_replace(calle1, "CALZ\\.", "CALZADA"),
         calle1 = str_replace(calle1, "ESTACIÓN DEL METRO ", "METRO "),
         calle1 = str_replace(calle1, "ESTACION DEL METRO ", "METRO "),
         calle1 = str_replace(calle1, "ESTACION METRO ", "METRO "),
         calle1 = str_replace(calle1, "INTERIOR DE LA METRO ", "METRO "),
         calle1 = str_replace(calle1, "INTERIOR DEL METRO ", "METRO "),
         calle1 = str_replace(calle1, "INT. METRO ", "METRO "),
         calle1 = str_replace(calle1, "ESTACIÓN METRO ", "METRO "),
         calle1 = case_when(str_detect(calle1, "METRO BELLAS") ~ "METRO BELLAS ARTES",
                            str_detect(calle1, "METRO PINO SUAREZ") ~ "METRO PINO SUAREZ",
                            str_detect(calle1, "ESTACION PINO SUAREZ") ~ "METRO PINO SUAREZ",
                            str_detect(calle1, "ESTACIÓN PINO SUAREZ") ~ "METRO PINO SUAREZ",
                            str_detect(calle1, "JOSE MARIA PINO SUAREZ \\(METRO\\) PINO SUAREZ") ~ "METRO PINO SUAREZ",
                            str_detect(calle1, "METRO JOSE MARIA PINO SUAREZ") ~ "METRO PINO SUAREZ",
                            str_detect(calle1, "JOSE MA. PINO SUAREZ") ~ "AVENIDA JOSE MARIA PINO SUAREZ",
                            str_detect(calle1, "AVENIDA PINO SUAREZ") ~ "AVENIDA JOSE MARIA PINO SUAREZ",
                            calle1 == "PINO SUAREZ" ~ "AVENIDA JOSE MARIA PINO SUAREZ",
                            calle1 == "JOSE MARIA PINO SUAREZ" ~ "AVENIDA JOSE MARIA PINO SUAREZ",
                            calle1 == "AVENIDA JOSE MARIA PINO SUAREZ SIN NUMERO" ~ "AVENIDA JOSE MARIA PINO SUAREZ",
                            calle1 == "JOISE MARIA PINO SUAREZ" ~ "AVENIDA JOSE MARIA PINO SUAREZ",
                            calle1 == "JOSE MA PINO SUAREZ" ~ "AVENIDA JOSE MARIA PINO SUAREZ",
                            str_detect(calle1, "AVENIDA HIDALGO") ~ "AVENIDA HIDALGO",
                            str_detect(calle1, "METRO HIDALGO") ~ "METRO HIDALGO",
                            str_detect(calle1, "METRO EN HIDALGO") ~ "METRO HIDALGO",
                            str_detect(calle1, "ESTACIÓN HIDALGO") ~ "METRO HIDALGO",
                            str_detect(calle1, "ESTACION HIDALGO") ~ "METRO HIDALGO",
                            str_detect(calle1, "ESTACIO HIDALGO") ~ "METRO HIDALGO",
                            calle1 == "PASEO DE LA REFORMA (METRO) HIDALGO" ~ "PASEO DE LA REFORMA",
                            calle1 == "PASEO DE REFORMA (ESTACION) HIDALGO" ~ "PASEO DE LA REFORMA",
                            calle1 == "AVENIDA DE CHAPULTEPEC (METRO BALDERAS)" ~ "AVENIDA CHAPULTEPEC",
                            str_detect(calle1, "AVENIDA BALDERAS") ~ "AVENIDA BALDERAS",
                            str_detect(calle1, "METRO BALDERAS") ~ "METRO BALDERAS",
                            str_detect(calle1, "ESTACIÓN BALDERAS") ~ "METRO BALDERAS",
                            str_detect(calle1, "ESTACION BALDERAS") ~ "METRO BALDERAS",
                            str_detect(calle1, "ESTACION DEL MERO BALDERAS") ~ "METRO BALDERAS",
                            str_detect(calle1, "ESTACION BELLAS ARTES") ~ "METRO BELLAS ARTES",
                            str_detect(calle1, "ESTACIÓN BELLAS ARTES") ~ "METRO BELLAS ARTES",
                            str_detect(calle1, "ESTACION ZOCALO") ~ "METRO ZOCALO",
                            str_detect(calle1, "PLAZA DE LA CONSTITUCION \\(METRO ZOCALO\\)") ~ "PLAZA DE LA CONSTITUCION",
                            str_detect(calle1, "METRO ZOCALO") ~ "METRO ZOCALO",
                            str_detect(calle1, "\\(METRO\\) ZOCALO") ~ "METRO ZOCALO",
                            str_detect(calle1, "METRO CHABACANO") ~ "METRO CHABACANO",
                            str_detect(calle1, "ESTACION CHABACANO") ~ "METRO CHABACANO",
                            str_detect(calle1, "CHABACANO DE LA LINEA") ~ "METRO CHABACANO",
                            str_detect(calle1, "CHABACANO LINEA") ~ "METRO CHABACANO",
                            str_detect(calle1, "AVENIDA JALISCO \\(METRO ") ~ "AVENIDA JALISCO",
                            str_detect(calle1, "METRO TACUBAYA") ~ "METRO TACUBAYA",
                            str_detect(calle1, "ESTACION TACUBAYA") ~ "METRO TACUBAYA",
                            str_detect(calle1, "METRO SALTO DEL AGUA") ~ "METRO SALTO DEL AGUA",
                            str_detect(calle1, "ESTACION SALTO DEL AGUA") ~ "METRO SALTO DEL AGUA",
                            calle1 == "\"METRO \"\"SALTO DEL AGUA\"\"\"" ~ "METRO SALTO DEL AGUA",
                            str_detect(calle1, "METRO PANTITLAN") ~ "METRO PANTITLAN",
                            str_detect(calle1, "ESTACION PANTITLAN") ~ "METRO PANTITLAN",
                            str_detect(calle1, "ESTACION PANTITLÁN") ~ "METRO PANTITLAN",
                            str_detect(calle1, "ESTACION CANDELARIA") ~ "METRO CANDELARIA",
                            str_detect(calle1, "METRO CANDELARIA") ~ "METRO CANDELARIA",
                            calle1 == "AVENIDA CUAUHTEMOC (ESTACION CENTRO MEDICO)" ~ "AVENIDA CUAUHTEMOC",  
                            calle1 == "EJE 3 SUR (METRO CENTRO MEDICO)" ~ "EJE 3 SUR",
                            calle1 == "JARDIN RAMON LOPEZ VELARDE (METRO CENTRO MEDICO)" ~ "JARDIN RAMON LOPEZ VELARDE",
                            str_detect(calle1, "METRO CENTRO MEDICO") ~ "METRO CENTRO MEDICO",
                            str_detect(calle1, "ESTACION CENTRO MEDICO") ~ "METRO CENTRO MEDICO",
                            str_detect(calle1, "METRO LA RAZA\\,") ~ "METRO LA RAZA",
                            str_detect(calle1, "ESTACION LA RAZA") ~ "METRO LA RAZA",
                            str_detect(calle1, "ESTACION SAN LAZARO") ~ "METRO SAN LAZARO",
                            str_detect(calle1, "METRO SAN LAZARO") ~ "METRO SAN LAZARO",
                            str_detect(calle1, "MTRO SAN LAZARO") ~ "METRO SAN LAZARO",
                            calle1 == "AVENIDA JOSE MARIA IZAZAGA (ESTACION ISABEL LA CATOLICA)" ~ "AVENIDA JOSE MARIA IZAZAGA",
                            calle1 == "JOSE MARIA IZAZAGA (METRO ISABEL LA CATOLICA)" ~ "AVENIDA JOSE MARIA IZAZAGA",
                            str_detect(calle1, "METRO ISABEL LA CATOLICA") ~ "METRO ISABEL LA CATOLICA",
                            str_detect(calle1, "ESTACION ISABEL LA CATOLICA") ~ "METRO ISABEL LA CATOLICA",
                            str_detect(calle1, "METRO GUERRERO|ESTACION GUERRERO") ~ "METRO GUERRERO",
                            str_detect(calle1, "METRO-MERCED") ~ "METRO MERCED",
                            str_detect(calle1, "ESTACION MERCED|ESTACIÓN MERCED") ~ "METRO MERCED",
                            calle1 == "ANILLO DE CIRCUNVALACION (METRO MERCED)" ~ "ANILLO DE CIRCUNVALACION",
                            calle1 == "AVENIDA ANILLO DE CIRCUNVLACION SIN NUMERO (METRO MERCED)" ~ "ANILLO DE CIRCUNVALACION",
                            calle1 == "AVENIDA CIRCUNVALACION Y ADOLFO GURRION (METRO MERCED)" ~ "ANILLO DE CIRCUNVALACION",
                            str_detect(calle1, "METRO MERCED|METRO LA MERCED") ~ "METRO MERCED",
                            TRUE ~ calle1)) %>%
  filter(!is.na(calle1),
         # ano == 2019,
         # str_detect(calle1, "METRO")
  ) %>% 
  count(calle1, sort = T) %>%
  # filter(str_detect(calle1, "CUATRO")) %>%
  # filter(str_detect(calle1, "MERCED")) %>%
  print(n = 30)
