### Importar y preprocesar datos ----
# source("02_codigo/cargar_preparar_datos_carpetas_investigacion.R")

### Frecuencia de CI por robo a transeuntes por calle, colonia y alcaldia  ----
bd_cdmx %>% 
  filter(str_detect(categoria_de_delito, "transe")) %>% 
  mutate(calle1 = iconv(calle1, to='ASCII//TRANSLIT'),
         calle1 = str_replace(calle1, "AV ", "AVENIDA "),
         calle1 = str_replace(calle1, "AV. ", "AVENIDA "),
         calle1 = str_replace(calle1, "AV.. ", "AVENIDA "),
         calle1 = str_replace(calle1, "CALZ.", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADA MEXICO TACUBA", "CALZADA MEXICO-TACUBA"),
         calle1 = str_replace(calle1, "AVENIDA ERMITA IZTAPALAPA", "CALZADA ERMITA IZTAPALAPA"),
         calle1 = str_replace(calle1, "AVENIDA INSURGENTES NTE.", "INSURGENTES NORTE"),
         calle1 = str_replace(calle1, "EJE CENETRAL LAZARO CARDENAS", "EJE CENTRAL LAZARO CARDENAS"),
         calle1 = str_replace(calle1, "EJE CENTRAL LAZARO CADENAS", "EJE CENTRAL LAZARO CARDENAS"),
         calle1 = str_replace(calle1, "EJECENTRAL LAZARO CARDENAS", "EJE CENTRAL LAZARO CARDENAS"),
         calle1 = case_when(str_detect(calle1, "EJE CENTRAL LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTRAL DE LAZARO CARDENAS.") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTRAL LAZARO ACRDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EjE CENTRAL LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTRAL LAZARO CAREDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTTRAL LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "CALZADA IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "CALZADA GRAL. IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "A IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "CALZADA") & str_detect(calle1, "IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(alcaldia, "Iztapalapa|Iztacalco|Carranza") & str_detect(calle1, "IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "CALZADA MEXICO-TACUBA") ~ "CALZADA MEXICO-TACUBA",
                            str_detect(calle1, "CALZADA MEXICO - TACUBA") ~ "CALZADA MEXICO-TACUBA",
                            str_detect(calle1, "INSURGENTES NORTE") ~ "INSURGENTES NORTE",
                            str_detect(calle1, "INSURGENTES SUR") ~ "INSURGENTES SUR",
                            str_detect(calle1, "AVENIDA UNIVERSIDAD") ~ "AVENIDA UNIVERSIDAD",
                            str_detect(calle1, "PASEO DE LA REFORMA") ~ "AVENIDA PASEO DE LA REFORMA",
                            str_detect(calle1, "AVENIDA REFORMA") ~ "AVENIDA PASEO DE LA REFORMA",
                            str_detect(alcaldia, "Cuauhtémoc") & str_detect(calle1, "REFORMA") ~ "AVENIDA PASEO DE LA REFORMA",
                            TRUE ~ calle1),
         
         calle1 = str_trim(calle1),
         calle1 = str_squish(calle1)) %>% 
  group_by(alcaldia, colonia, calle1) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  arrange(-num_ci)


### Frecuencia de CI por robo a transeuntes por calle  ----
bd_cdmx %>% 
  filter(str_detect(categoria_de_delito, "transe")) %>% 
  mutate(calle1 = iconv(calle1, to='ASCII//TRANSLIT'),
         calle1 = str_replace(calle1, "AV ", "AVENIDA "),
         calle1 = str_replace(calle1, "AV. ", "AVENIDA "),
         calle1 = str_replace(calle1, "AV.. ", "AVENIDA "),
         calle1 = str_replace(calle1, "CALZ.", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADADA", "CALZADA"),
         calle1 = str_replace(calle1, "CALZADA MEXICO TACUBA", "CALZADA MEXICO-TACUBA"),
         calle1 = str_replace(calle1, "AVENIDA ERMITA IZTAPALAPA", "CALZADA ERMITA IZTAPALAPA"),
         calle1 = str_replace(calle1, "AVENIDA INSURGENTES NTE.", "INSURGENTES NORTE"),
         calle1 = str_replace(calle1, "EJE CENETRAL LAZARO CARDENAS", "EJE CENTRAL LAZARO CARDENAS"),
         calle1 = str_replace(calle1, "EJE CENTRAL LAZARO CADENAS", "EJE CENTRAL LAZARO CARDENAS"),
         calle1 = str_replace(calle1, "EJECENTRAL LAZARO CARDENAS", "EJE CENTRAL LAZARO CARDENAS"),
         calle1 = case_when(str_detect(calle1, "EJE CENTRAL LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTRAL DE LAZARO CARDENAS.") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTRAL LAZARO ACRDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EjE CENTRAL LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTRAL LAZARO CAREDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "EJE CENTTRAL LAZARO CARDENAS") ~ "EJE CENTRAL LAZARO CARDENAS",
                            str_detect(calle1, "CALZADA IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "CALZADA GRAL. IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "A IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "CALZADA") & str_detect(calle1, "IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(alcaldia, "Iztapalapa|Iztacalco|Carranza") & str_detect(calle1, "IGNACIO ZARAGOZA") ~ "CALZADA IGNACIO ZARAGOZA",
                            str_detect(calle1, "CALZADA MEXICO-TACUBA") ~ "CALZADA MEXICO-TACUBA",
                            str_detect(calle1, "CALZADA MEXICO - TACUBA") ~ "CALZADA MEXICO-TACUBA",
                            str_detect(calle1, "INSURGENTES NORTE") ~ "INSURGENTES NORTE",
                            str_detect(calle1, "INSURGENTES SUR") ~ "INSURGENTES SUR",
                            str_detect(calle1, "AVENIDA UNIVERSIDAD") ~ "AVENIDA UNIVERSIDAD",
                            str_detect(calle1, "PASEO DE LA REFORMA") ~ "AVENIDA PASEO DE LA REFORMA",
                            str_detect(calle1, "AVENIDA REFORMA") ~ "AVENIDA PASEO DE LA REFORMA",
                            str_detect(alcaldia, "Cuauhtémoc") & str_detect(calle1, "REFORMA") ~ "AVENIDA PASEO DE LA REFORMA",
                            TRUE ~ calle1),
         
         calle1 = str_trim(calle1),
         calle1 = str_squish(calle1)) %>% 
  group_by(calle1) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  arrange(-num_ci)
