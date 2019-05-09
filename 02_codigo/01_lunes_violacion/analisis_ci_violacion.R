### Importar y preprocesar datos ----
# source("02_codigo/cargar_preparar_datos_carpetas_investigacion.R")

### Generar subconjunto de la base de datos que sólo contenga los datos de carpetas de investigación (CI) de la categoría de delito de violación ----
# Selección de categoría de delito para hacer informes
datos_delito <- 
  bd_cdmx %>% 
  filter(categoria_de_delito == "Violación")

### Gráfica del número de carpetas de investigación iniciadas cada día de la semana entre enero de 2016 y marzo de 2019 ----
datos_delito %>%
  count(dia_semana) %>% 
  filter(!is.na(dia_semana)) %>% 
  ggplot(aes(dia_semana, n)) +
  geom_col(fill = "#a50300") +
  geom_text(aes(label = comma(n)), color = "white", vjust = 1.5, fontface = "bold") +
  scale_x_continuous(breaks = 1:7, labels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")) +
  scale_y_continuous(labels = comma) +
  labs(title = str_wrap(str_to_upper(str_c("número de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), " iniciadas en cada día de la semana", sep = "")), width = 100),
       subtitle = "Datos de enero de 2016 al 31 de marzo de 2019",
       x = NULL,
       y = NULL,
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40") +
  tema +
  theme(axis.text.y = element_blank(), 
        panel.grid = element_blank()) + 
  ggsave(str_c("03_graficas/01_lunes/violacion/01_frecuencia_dia_semana_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),".png", sep = ""), width = 13, height = 8, dpi = 200)


### Gráfica del número de carpetas de investigación por violáción acumulado diariamente en la CDMX cada año entre 2016 y 2019 ----
datos_delito %>% 
  arrange(fecha_inicio) %>% 
  group_by(ano) %>% 
  mutate(dia_ano = yday(fecha_inicio)) %>% 
  ungroup() %>% 
  group_by(ano, dia_ano) %>% 
  summarise(num_carpetas_diarias = n()) %>% 
  ungroup() %>% 
  group_by(ano) %>% 
  mutate(num_acumulado_carpetas = cumsum(num_carpetas_diarias), 
         etiqueta = ifelse(dia_ano == max(dia_ano), ano, ""),
         color_linea = ifelse(ano == 2019, "sí", "no")) %>%
  ungroup() %>%
  ggplot(aes(dia_ano, num_acumulado_carpetas, group = ano, color = color_linea)) +
  geom_line(size = 1) +
  geom_text_repel(aes(label = etiqueta), hjust = -0.15, color = "grey40", fontface = "bold", size = 5) +
  scale_x_continuous(breaks = c(1, seq(30, 360, 30)), limits = c(0, 380)) +
  scale_y_continuous(label = comma) +
  scale_color_manual(values = c("grey70", "#a50300")) +
  labs(title = str_wrap(str_to_upper(str_c("número de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), ", acumulado diariamente en la ciudad de méxico", paste = "")), width = 80),
       subtitle = "Datos de enero de 2016 al 31 de marzo de 2019",
       x = "\nDías del año transcurridos",
       y = "Número de carpetas de investigación\n", 
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40") +
  tema +
  theme(legend.position = "none") + 
  ggsave(str_c("03_graficas/01_lunes/violacion/02_frecuencia_ci_acumulada_diariamente_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),".png", sep = ""), width = 12, height = 8, dpi = 200)


### Gráfica de la tasa mensual anualizada de CI por violación ----
datos_delito %>% 
  arrange(fecha_inicio) %>% 
  mutate(fecha_techo_mes = ceiling_date(fecha_inicio, unit = "month") - 1) %>% 
  select(fecha_inicio, fecha_techo_mes) %>% 
  # Contar número de CI por mes
  group_by(fecha_techo_mes) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  mutate(año = year(fecha_techo_mes), 
         mes = month(fecha_techo_mes)) %>% 
  # Unir datos de población anual de la CDMX 
  left_join(datos_poblacion_cdmx, by = c("año" = "ano")) %>% 
  # Generar tasa mensual anualizada
  mutate(tasa_x_100k = (num_ci/pob_tot)*100000*12) %>% 
  select(fecha_techo_mes, año, mes, everything()) %>% 
  mutate(color_linea = ifelse(año == 2019, "sí", "no")) %>%
  group_by(año) %>% 
  mutate(etiqueta = ifelse(fecha_techo_mes == max(fecha_techo_mes), año, "")) %>% 
  ungroup() %>% 
  ggplot(aes(x = mes, y = tasa_x_100k, group = año, color = color_linea)) +
  geom_line(size = 1) +
  geom_text_repel(aes(label = etiqueta), hjust = -0.15, color = "grey40", fontface = "bold", size = 5) +
  scale_x_continuous(limits = c(1, 12.5), breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  # scale_y_continuous(label = comma) +
  scale_color_manual(values = c("grey70", "#a50300")) +
  labs(title = str_wrap(str_to_upper(str_c("tasa mensual anualizada de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), ", en la ciudad de méxico", paste = "")), width = 80),
       subtitle = "Tasa por cada 100 mil habitantes",
       x = "\n",
       y = "Tasa mensual anualizada\n", 
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la CDMX y proyecciones poblacionales de CONAPO.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  tema +
  theme(legend.position = "none") + 
  ggsave(str_c("03_graficas/01_lunes/violacion/03_tasa_mensual_anualizada_de_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),".png", sep = ""), width = 12, height = 8, dpi = 200) 


### Gráfica de la tasa trimenstral anualizada de CI por violación ----
datos_delito %>% 
  filter(mes %in% c("Enero", "Febrero", "Marzo")) %>% 
  group_by(ano) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  # Unir datos de población anual de la CDMX 
  left_join(datos_poblacion_cdmx, by = c("ano" = "ano")) %>% 
  # Generar tasa trimestral anualizada
  mutate(tasa_x_100k = (num_ci/pob_tot)*100000*4) %>% 
  ggplot(aes(x = ano, y = tasa_x_100k)) +
  geom_col(fill = "#a50300") +
  geom_text(aes(label = round(tasa_x_100k, 2)), color = "white", vjust = 1.5, fontface = "bold", size = 5)  +
  scale_x_continuous(breaks = 2016:2019, labels = c("Ene. - mar. \'16", "Ene. - mar. \'17", "Ene. - mar. \'18", "Ene. - mar. \'19")) +
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper(str_c("tasa trimestral anualizada de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), ", en la ciudad de méxico", sep = "")), width = 80),
       subtitle = "Tasa por cada 100 mil habitantes",
       x = NULL,
       y = NULL,
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México y proyecciones poblacionales de CONAPO.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  tema +
  theme(axis.text.y = element_blank(), 
        panel.grid = element_blank()) + 
  ggsave(str_c("03_graficas/01_lunes/violacion/04_tasa_trimestral_anualizada_de_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),".png", sep = ""), width = 12, height = 8, dpi = 200) 



### Cambio en el número de CI en el 1T de 2018 vs. el 1T 2019, por alcaldía ----
cambio_num_acumulado_delitos_alcaldia <- 
  datos_delito %>% 
  filter(ano %in% c(2018, 2019), 
         mes %in% c("Enero", "Febrero", "Marzo")) %>% 
  group_by(ano, alcaldia) %>% 
  summarise(num_acumulado_carpetas = n(), 
            categoria_de_delito = last(categoria_de_delito)) %>% 
  ungroup() %>% 
  select(ano, alcaldia, num_acumulado_carpetas, categoria_de_delito) %>% 
  complete(alcaldia, nesting(ano)) %>% # Completar renglones que faltan en algunas alcadías porque no hubo carpeta en el año correspondiente
  mutate(num_acumulado_carpetas = ifelse(is.na(num_acumulado_carpetas), # Reemplazar NAs de num_acumulado_carpetas por 0
                                         0, num_acumulado_carpetas),
         etiqueta_alcaldia = ifelse(ano == 2019, alcaldia, "")) %>%  # Generar etiquetas para gráfica
  arrange(alcaldia, ano) %>% 
  group_by(alcaldia) %>% 
  mutate(cambio = num_acumulado_carpetas - lag(num_acumulado_carpetas), # Cambio absoluto
         color_geoms = ifelse(num_acumulado_carpetas > lag(num_acumulado_carpetas), # Generar variable para color de geoms
                              "rojo", "verde"),
         color_geoms = na.locf(color_geoms)) %>% # Llenar NAs de color_geoms
  ungroup() 

cambio_num_acumulado_delitos_alcaldia %>% 
  ggplot(aes(x = ano, y = num_acumulado_carpetas, group = alcaldia,  color = color_geoms)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = str_wrap(etiqueta_alcaldia, width = 15)), nudge_x = 2, color = "grey20", segment.color = "grey80", size = 3) +
  scale_x_continuous(breaks = c(2018, 2019), labels = c("Ene. - mar. \'18\n", "Ene. - mar. \'19\n"), 
                     limits = c(2018, 2019.05)) +
  scale_color_manual(values = c("tomato", "#66bd63")) +
  labs(title = str_wrap(str_to_upper(str_c("número acumulado de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), ", primer trimestre de 2019 vs. primer trimestre de 2018, por alcaldía", sep = "")), width = 78),
       subtitle = str_wrap("La gráfica muestra el cambio en el número de carpetas de investigación acumuladas en el primer trimestre de cada año", width = 95),
       x = NULL,
       y = "Número acumulado de\ncarpetas de investigación\n",
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  tema +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") + 
  ggsave(str_c("03_graficas/01_lunes/violacion/05_cambio_numero_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),"_1T_2018_vs_1T_2019_por_alcaldia.png", sep = ""), width = 12, height = 8, dpi = 200) 


### Cambio en el número acumulado de CI por violación en el 1T de 2019 vs. 1T de 2018, por alcaldía----
cambio_num_acumulado_delitos_alcaldia %>% 
  filter(!is.na(cambio)) %>% 
  mutate(etiqueta_cambio_positivo_grande = ifelse(cambio > 2, paste("+", comma(round(cambio, 1)), sep = ""), ""),
         etiqueta_cambio_positivo_pequenio = ifelse(cambio >= 0 & cambio < 2, paste("+", comma(round(cambio, 1)), sep = ""), ""),
         etiqueta_cambio_negativo_grande = ifelse(cambio < -2, comma(round(cambio, 1)), ""),
         etiqueta_cambio_negativo_pequenio = ifelse(cambio <= 0 & cambio > -2, comma(round(cambio, 1)), "")) %>% 
  ggplot(aes(fct_reorder(alcaldia, cambio), cambio, fill = color_geoms)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = etiqueta_cambio_positivo_grande), hjust = 1.2, color = "white", fontface = "bold", size = 4) +
  geom_text(aes(label = etiqueta_cambio_positivo_pequenio), hjust = -0.4, color = "grey30", fontface = "bold", size = 4) +
  geom_text(aes(label = etiqueta_cambio_negativo_grande), hjust = -0.6, color = "white", fontface = "bold", size = 4) +
  geom_text(aes(label = etiqueta_cambio_negativo_pequenio), hjust = 1.2, color = "grey30", fontface = "bold", size = 4) +
  coord_flip() +
  scale_y_continuous(breaks = c(min(cambio_num_acumulado_delitos_alcaldia$cambio, na.rm = T), max(cambio_num_acumulado_delitos_alcaldia$cambio, na.rm = T)),
                     labels = c("Mejor", "Peor")) +
  scale_fill_manual(values = c("tomato", "#66bd63")) +
  labs(title = str_wrap(str_to_upper(str_c("cambio en el número acumulado de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), ", primer trimestre de 2019 vs. primer trimestre de 2018, por alcaldía", sep = "")), width = 78),
       subtitle = str_wrap("La gráfica muestra el cambio absoluto en el número de carpetas de investigación acumuladas en el primer trimestre de cada año", width = 85),
       x = NULL,
       y = "\nCambio en el número de carpetas de investigación\n",
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  tema +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15)) + 
  ggsave(str_c("03_graficas/01_lunes/violacion/06_cambio_absoluto_numero_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),"_1T_2018_vs_1T_2019_por_alcaldia.png", sep = ""), width = 12, height = 10, dpi = 200) 


### Cambio en la tasa trimestral anualizada de CI en el 1T de 2018 vs. el 1T 2019, por alcaldía ----
datos_delito %>% 
  filter(ano %in% c(2018, 2019), 
         mes %in% c("Enero", "Febrero", "Marzo")) %>% 
  group_by(ano, alcaldia) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  # Unir datos de población anual de la CDMX 
  left_join(datos_poblacion_alcaldias, by = "alcaldia") %>% 
  # Generar tasa trimestral anualizada
  mutate(tasa_x_100k = (num_ci/pob_total)*100000*4) %>% 
  arrange(alcaldia, ano) %>% 
  complete(alcaldia, nesting(ano)) %>% # En caso de ser necesaerio, completar renglones que faltan en algunas alcadías porque no hubo carpeta en el año correspondiente
  mutate(tasa_x_100k = ifelse(is.na(tasa_x_100k), # Reemplazar NAs de tasa_x_100k por 0
                              0, tasa_x_100k),
         etiqueta_alcaldia = ifelse(ano == 2019, alcaldia, "")) %>%  # Generar etiquetas para gráfica
  group_by(alcaldia) %>% 
  mutate(color_geoms = ifelse(tasa_x_100k > lag(tasa_x_100k), # Generar variable para color de geoms
                              "rojo", "verde"),
         color_geoms = na.locf(color_geoms)) %>% # Llenar NAs de color_geoms
  ungroup() %>% 
  ggplot(aes(x = ano, y = tasa_x_100k, 
             group = alcaldia,  
             color = color_geoms)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = str_wrap(etiqueta_alcaldia, width = 15)), nudge_x = 2, color = "grey20", segment.color = "grey80", size = 3) +
  scale_x_continuous(breaks = c(2018, 2019), labels = c("Ene. - mar. \'18\n", "Ene. - mar. \'19\n"), 
                     limits = c(2018, 2019.05)) +
  scale_color_manual(values = c("tomato", "#66bd63")) +
  labs(title = str_wrap(str_to_upper(str_c("tasa trimestral anualizada de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), ", primer trimestre de 2019 vs. primer trimestre de 2018, por alcaldía", sep = "")), width = 80),
       subtitle = "Tasa por cada 100 mil habitantes",
       x = NULL,
       y = "Tasa trimestral anualizada\n",
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México y el Anuario estadístico y geográfico\nde la Ciudad de México 2017 del INEGI y la CDMX.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  tema +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") + 
  ggsave(str_c("03_graficas/01_lunes/violacion/07_tasa_trimestral_anualizada_de_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),"_1T_2018_vs_1T_2019_por_alcaldia.png", sep = ""), width = 12, height = 10, dpi = 200) 


### Gráfica de la concentracion porcentual de CI por delito y alcaldía ----
bd_cdmx %>% 
  # Calcular el número total de CI por categoría de delito
  # y alcaldía. Esta cifra incluye todas las CI iniciadas 
  # entre el 1 de enero de 2016 y el 31 de marzo de 2019
  group_by(alcaldia, categoria_de_delito) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  # Calcular (i) el número total de CI por categoría de delito, 
  # (ii) el % de CI por categoría de delito concentrado en cada 
  # alcaldía y (iii) el ranking de cada alcaldía de acuerdo con
  # lo obtenido en (ii)
  group_by(categoria_de_delito) %>% 
  mutate(total = sum(num), 
         por = round((num/sum(num))*100, 1), 
         rank_del = rank(-por)) %>% 
  ungroup() %>% 
  arrange(rank_del, -por) %>% 
  # Filtrar para quedarme con la alcaldía rankeada #1 en cada
  # categoría de delito
  filter(rank_del == 1) %>% 
  mutate(rank_concentracion = rank(-por), 
         rank_concentracion_texto = case_when(rank_concentracion == 1 ~"primera",
                                              rank_concentracion == 2 ~"segunda",
                                              rank_concentracion == 3 ~"tercera",
                                              rank_concentracion == 4 ~"cuarta",
                                              rank_concentracion == 5 ~"quinta",
                                              rank_concentracion == 6 ~"sexta",
                                              rank_concentracion == 7 ~"septima",
                                              rank_concentracion == 8 ~"octava",
                                              rank_concentracion == 9 ~"novena",
                                              rank_concentracion == 10 ~"decima",
                                              rank_concentracion == 11 ~"onceava",
                                              rank_concentracion == 12 ~"doceava",
                                              rank_concentracion == 13 ~"decimo tercera",
                                              rank_concentracion == 14 ~"decimo cuarta",
                                              rank_concentracion == 15 ~"decimo quinta",
                                              rank_concentracion == 16 ~"decimo sexta"),
         color_geom = ifelse(categoria_de_delito == unique(datos_delito$categoria_de_delito), "a", "b")) %>% 
  ggplot(aes(x = fct_reorder(str_wrap(categoria_de_delito, width = 30), por), y = por, fill = color_geom)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = str_c(alcaldia, " (", round(por, 1), "%)", sep = "")), hjust = 1.05, color = "white", fontface = "bold", size = 5) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#a50300", "grey70")) +
  labs(title = str_wrap(str_to_upper("concentración geográfica de las carpetas de investigación iniciadas por cada categoría de delito"), width = 55),
       subtitle = str_wrap("Carpetas de investigación acumuladas entre el 1 enero de 2016 y el 31 de marzo de 2019", width = 95),
       x = NULL,
       y = "Porcentaje del total, por categoría de delito",
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  tema +
  theme(plot.title = element_text(size = 25), 
        plot.subtitle = element_text(size = 18), 
        plot.caption = element_text(size = 13),
        axis.title.x = element_text(size = 18),
        legend.position = "none", 
        panel.grid.major = element_blank(),
        axis.text.x = element_blank()) + 
  ggsave("03_graficas/01_lunes/violacion/08_concentracion_porcentual_ci_por_alcaldia.png", width = 13.5, height = 15, dpi = 200) 


### Gráfica del número de CI iniciadas por la categoría de delito seleccionada al comienzo del script para todo el período analizado ----
datos_delito %>%
  # Calcular el número total de CI por alcaldía para el delito
  # filtrado al comienzo del código. Esta cifra incluye todas
  # las CI iniciadas entre el 1 de enero de 2016 y el 
  # 31 de marzo de 2019
  group_by(alcaldia) %>% 
  summarise(num_carpetas = n()) %>% 
  ungroup() %>%
  # Unir datos poblacionales por alcaldía
  left_join(datos_poblacion_alcaldias, by = "alcaldia") %>% 
  # Calcular tasa por cada 100 mil habitantes
  mutate(tasa_x_100k = (num_carpetas/pob_total)*1e5,
         rank_absoluto = rank(-num_carpetas, ties.method = "first"),
         rank_tasa = rank(-tasa_x_100k, ties.method = "first"),
         etiqueta_carpetas_grande = ifelse(num_carpetas > 5, comma(num_carpetas), ""),
         etiqueta_carpetas_pequenio = ifelse(num_carpetas >= 0 & num_carpetas <= 5, comma(num_carpetas), ""),
         etiqueta_tasas_grande = ifelse(tasa_x_100k > 5, round(tasa_x_100k, 1), ""),
         etiqueta_tasas_pequenio = ifelse(tasa_x_100k >= 0 & tasa_x_100k < 5, round(tasa_x_100k, 1), "")) %>% 
  ggplot(aes(fct_reorder(alcaldia, num_carpetas), num_carpetas)) +
  geom_col(fill = "#a50300", alpha = 0.9) +
  geom_text(aes(label = etiqueta_carpetas_grande), hjust = 1.2, color = "white", fontface = "bold", size = 4) +
  geom_text(aes(label = etiqueta_carpetas_pequenio), hjust = -0.8, color = "grey40", fontface = "bold", size = 4) +
  coord_flip() +
  labs(title = str_wrap(str_to_upper(str_c("número de carpetas de investigación por ", unique(datos_delito$categoria_de_delito), ", por alcaldia", paste = "")), width = 70),
       subtitle = str_wrap("Carpetas de investigación acumuladas entre el 1 enero de 2016 y el 31 de marzo de 2019", width = 90),
       x = NULL,
       y = "Número de carpetas\n",
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40") +
  tema +
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(str_c("03_graficas/01_lunes/violacion/09_numero_total_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),"_por_alcaldia.png", sep = ""), width = 12, height = 10, dpi = 200) 
  
  
  
### Gráfica de la tasa por cada 100 mil habitantes de CI iniciadas en todo el período analizado por la categoría de delito seleccionada al comienzo del script ----

datos_delito %>%
  # Calcular el número total de CI por alcaldía para el delito
  # filtrado al comienzo del código. Esta cifra incluye todas
  # las CI iniciadas entre el 1 de enero de 2016 y el 
  # 31 de marzo de 2019
  group_by(alcaldia) %>% 
  summarise(num_carpetas = n()) %>% 
  ungroup() %>%
  # Unir datos poblacionales por alcaldía
  left_join(datos_poblacion_alcaldias, by = "alcaldia") %>% 
  # Calcular tasa por cada 100 mil habitantes
  mutate(tasa_x_100k = (num_carpetas/pob_total)*1e5,
         rank_absoluto = rank(-num_carpetas, ties.method = "first"),
         rank_tasa = rank(-tasa_x_100k, ties.method = "first"),
         etiqueta_carpetas_grande = ifelse(num_carpetas > 5, comma(num_carpetas), ""),
         etiqueta_carpetas_pequenio = ifelse(num_carpetas >= 0 & num_carpetas <= 5, comma(num_carpetas), ""),
         etiqueta_tasas_grande = ifelse(tasa_x_100k > 5, round(tasa_x_100k, 1), ""),
         etiqueta_tasas_pequenio = ifelse(tasa_x_100k >= 0 & tasa_x_100k < 5, round(tasa_x_100k, 1), "")) %>% 
  ggplot(aes(fct_reorder(alcaldia, tasa_x_100k), tasa_x_100k)) +
  geom_col(fill = "#a50300", alpha = 0.9) +
  geom_text(aes(label = etiqueta_tasas_grande), hjust = 1.2, color = "white", fontface = "bold", size = 4) +
  geom_text(aes(label = etiqueta_tasas_pequenio), hjust = -0.7, color = "grey40", fontface = "bold", size = 4) +
  coord_flip() +
  labs(title = str_wrap(str_to_upper(str_c("tasa de carpetas de investigación por cada 100 mil habitantes por ", unique(datos_delito$categoria_de_delito), ", por alcaldia", paste = "")), width = 75),
       subtitle = str_wrap("Carpetas de investigación acumuladas entre el 1 enero de 2016 y el 31 de marzo de 2019", width = 90),
       x = NULL,
       y = "Tasa de carpetas por\ncada 100 mil habitantes\n",
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México y el Anuario\nestadístico y geográfico de la Ciudad de México 2017 del INEGI y la CDMX.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40") +
  tema +
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(str_c("03_graficas/01_lunes/violacion/10_tasa_por_cada_100_mil_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),"_por_alcaldia.png", sep = ""), width = 12, height = 10, dpi = 200) 


### Gráfica de las 20 colonias con más CI iniciadas iniciadas en todo el período analizado por la categoría de delito seleccionada al comienzo del script ----

# Ranking de colonias con el mayor número de capretas de investigación por delito 
top_20_colonias_por_delito <- 
  datos_delito %>%
  # Eliminar observaciones que no incluyen colonia  
  filter(!is.na(colonia)) %>% 
  # Calcular el número total de CI por alcaldía y colonia 
  # para el delito filtrado al comienzo del código. Agrupo 
  # por colonia y alcaldía porque existen colonias con el 
  # mismo nombre en diferentes alcaldías.
  #
  # Esta cifra incluye todas las CI iniciadas entre el 1 de 
  # enero de 2016 y el 31 de marzo de 2019
  group_by(categoria_de_delito, colonia, alcaldia) %>% # 
  summarise(num_carpetas = n()) %>% 
  ungroup() %>% 
  # Rankear colonias
  group_by(categoria_de_delito) %>% 
  mutate(ranking_colonias = rank(-num_carpetas, ties.method = "first")) %>% 
  ungroup() %>% 
  arrange(categoria_de_delito, ranking_colonias) %>% 
  group_by(categoria_de_delito) %>% 
  # Calcular estadísticas que usaré más abajo
  mutate(num_acumulado_carpetas = cumsum(num_carpetas), 
         num_total_carpetas = sum(num_carpetas),
         por_acumulado_carpetas = (num_acumulado_carpetas/num_total_carpetas)*100) %>% 
  ungroup() %>% 
  mutate(col_del = paste(str_to_title(colonia), alcaldia, sep = " - ")) %>% 
  filter(ranking_colonias <= 20)

# Gráfica
top_20_colonias_por_delito %>% 
  ggplot(aes(fct_reorder(str_wrap(col_del, width = 35), num_carpetas), num_carpetas)) +
  geom_col(fill = "#a50300", alpha = 0.9) +
  geom_text(aes(label = comma(num_carpetas)), color = "white", hjust = 1.5, fontface = "bold", size = 4) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  labs(title = str_wrap(str_to_upper(paste("las 20 colonias con el mayor número de carpetas de investigación por ", unique(top_20_colonias_por_delito$categoria_de_delito), sep = "")), width = 50),
       subtitle = str_wrap(paste("Estas 20 colonias concentran el ", round(max(top_20_colonias_por_delito$por_acumulado_carpetas), 1), "% (", comma(max(top_20_colonias_por_delito$num_acumulado_carpetas)), ") de las ", comma(max(top_20_colonias_por_delito$num_total_carpetas)), " carpetas de investigación iniciadas por este delito entre el 1 de enero de 2016 y el 31 de marzo de 2019"  , sep = ""), width = 110), 
       x = NULL,
       y = NULL, 
       caption = "Fuente: Agencia Digital de Innovación Pública de la CDMX con datos de la PGJ de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  coord_flip() +
  tema +
  theme(plot.title = element_text(size = 24), 
        plot.subtitle = element_text(size = 15), 
        plot.caption = element_text(size = 13),
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10)) +
  ggsave(str_c("03_graficas/01_lunes/violacion/11_numero_acumulado_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),"_por_colonia.png", sep = ""), width = 13, height = 15, dpi = 200) 


### Mapa de las CI iniciadas iniciadas en todo el período analizado por la categoría de delito seleccionada al comienzo del script ----

# Nota: La base de datos de carpetas de investigación no incluye un identificador único para cada colonia. Esto impide unir los datos agregados a nivel colonia con su correspondiente polígono en el archivo de forma (*shape file*) de colonias. 

# Asimismo, los nombres de las colonias incluidos en la base de datos de carpetas de investigación no coinciden en todos los casos con los nombres de las colonias en el archivo de forma. Esto dos problemas impiden contar el número de carpetas por colonia en la base de datos de carpetas de investigación, para después unir los datos al polígono correspondiente del archivo de forma. 

# Dadas estas limitantes, opté por medir la frecuencia del número de carpetas de investigación a través de contar el número de puntos que caen en cada polígono. Esta es una solución imperfecta y puede provocar que las cifras asignadas a una o más colonias varíen ligeramente del número real. 

# Otro motivo por el cuál pueden varias las cifras es porque no todos los registros de la base de datos de carpetas de investigación incluyen coordenadas de latitud y/o longitud. El análisis que aquí se presenta elimina los registros que no incluyen una o ambas coordenadas.

foo <-
  datos_delito %>% 
  filter(!is.na(longitud),      # Eliminar observaciones para las que falta
         !is.na(latitud)) %>%   # el dato de latitud y/o longitud. 
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

# Construir vector con TRUE/FALSE para determinar si puntos están dentro de polígonos o no
resultados_hd <- st_within(foo, colonias_sf, sparse = FALSE) # Código adaptado de https://stackoverflow.com/questions/45899768/r-cloropleth-of-the-data-points-that-fall-within-a-polygon-what-percentage-h   

# Generar data frame para después graficar
faa <- 
  colonias_sf %>%
  mutate(num_carpetas = apply(resultados_hd, 2, sum), # Contar número de puntos que caen dentro de cada colonia
         categoria_de_delito = unique(foo$categoria_de_delito),
         categoria_de_delito = str_trim(categoria_de_delito),
         cat_delito_mayusculas = str_to_upper(categoria_de_delito),
         cat_delito_archivo = str_replace_all(str_to_lower(categoria_de_delito), " ", "_"),
         titulo = paste("número de carpetas de investigación iniciadas por ", cat_delito_mayusculas, ", por colonia", sep = ""), 
         titulo = str_to_upper(titulo),
         titulo = str_squish(titulo)) 

# Mapa
faa %>% 
  ggplot() +
  geom_sf(aes(fill = num_carpetas), color = "grey70", size = 0.2) +
  geom_sf(data = alcaldias_sf, color = "grey40", fill = NA, size = 0.3) +
  coord_sf(xlim = c(-99.5, -98.8), datum = NA) +
  scale_fill_gradient(low = "white", high = "#a50300", breaks = round(seq(0, max(faa$num_carpetas), max(faa$num_carpetas)/5), 0), limits = c(0, max(faa$num_carpetas)), labels = comma) +  
  labs(title = str_wrap(faa$titulo, width = 65),
       subtitle = "Datos de enero de 2016 al 31 de marzo de 2019",
       fill = "Núm. de carpetas\nde investigación",
       caption = "\nFuente: Agencia Digital de Innovación Pública de la CDMX con datos de la Procuraduría General de Justicia de la Ciudad de México.\nElaborado por Sebastián Garrido de Sierra (@segasi) para ADN40.") +
  tema +
  theme(plot.title = element_text(size = 24), 
        plot.subtitle = element_text(size = 15), 
        plot.caption = element_text(size = 13),panel.grid.major=element_line(colour = "transparent"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.15, 0.6),
        legend.title = element_text(face = "bold", size = 13),
        legend.text = element_text(face = "bold", size = 11),
        legend.key.height = unit(1.5, "cm"))  +
  ggsave(str_c("03_graficas/01_lunes/violacion/12_mapa_numero_acumulado_ci_por_", str_replace_all(str_to_lower(unique(datos_delito$categoria_de_delito)), " ", ""),"_por_alcaldia.png", sep = ""), width = 13, height = 13, dpi = 200) 
