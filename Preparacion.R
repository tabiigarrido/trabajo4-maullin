library(readxl)
datos <- read_excel("input/Localidad_CPV24.xlsx")
names(datos)
unique(datos$COMUNA)

library(readxl)
library(dplyr)
datos <- read_excel("input/Localidad_CPV24.xlsx")
datos_selec <- datos %>% select(LOCALIDAD, n_per, n_vp, n_edad_60_mas, prom_edad, n_discapacidad, n_serv_hig_fosa, n_serv_hig_pozo, n_serv_hig_alc_dentro, n_serv_hig_alc_fuera, n_serv_hig_no_tiene, n_tipo_viv_mediagua, n_basura_entierra, n_fuente_agua_pozo)
names(datos_selec)

#variables de razon
datos_selec <- datos_selec %>% mutate(
  prop_adulto_mayor = n_edad_60_mas / n_per,
  prop_discapacidad = n_discapacidad / n_per,
  prop_fosa = n_serv_hig_fosa / n_vp,
  prop_mediagua = n_tipo_viv_mediagua / n_vp,
  prop_basura_entierra = n_basura_entierra / n_vp,
  prop_agua_pozo = n_fuente_agua_pozo / n_vp
)

#variable ordinal
datos_selec <- datos_selec %>% mutate(
  riesgo_sanitario = case_when(
    prop_fosa + (n_serv_hig_pozo / n_vp) > 0.5 ~ "Alto",
    prop_fosa + (n_serv_hig_pozo / n_vp) > 0.2 ~ "Medio",
    TRUE ~ "Bajo"
  ),
  riesgo_sanitario = factor(riesgo_sanitario, 
                            levels = c("Bajo", "Medio", "Alto"),
                            ordered = TRUE)
)

#variable nominal
datos_selec <- datos_selec %>% mutate(
  tipo_serv_hig = case_when(
    n_serv_hig_alc_dentro + n_serv_hig_alc_fuera > n_serv_hig_fosa & 
      n_serv_hig_alc_dentro + n_serv_hig_alc_fuera > n_serv_hig_pozo ~ "Alcantarillado",
    n_serv_hig_fosa > n_serv_hig_pozo ~ "Fosa séptica",
    TRUE ~ "Pozo o sin servicio"
  ),
  tipo_serv_hig = factor(tipo_serv_hig)
)

glimpse(datos_selec)

# Tabla descriptiva de variables numéricas
summary(datos_selec %>% select(prom_edad, prop_adulto_mayor, prop_discapacidad, prop_fosa))

# Tabla de frecuencias - Riesgo sanitario (ordinal)
table(datos_selec$riesgo_sanitario)

# Tabla de frecuencias - Tipo servicio higiénico (nominal)
table(datos_selec$tipo_serv_hig)

library(ggplot2)

#grafico- Histograma de promedio de edad por localidad (razón)
ggplot(datos_selec, aes(x = prom_edad)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "white") +
  labs(
    title = "Distribución del promedio de edad por localidad",
    subtitle = "Comuna de Maullín, CPV 2024",
    x = "Promedio de edad",
    y = "Frecuencia"
  ) +
  theme_minimal()

#Gráfico de barras del riesgo sanitario (ordinal)
ggplot(datos_selec, aes(x = riesgo_sanitario, fill = riesgo_sanitario)) +
  geom_bar() +
  scale_fill_manual(values = c("Bajo" = "green4", "Medio" = "orange", "Alto" = "red3")) +
  labs(
    title = "Nivel de riesgo sanitario por localidad",
    subtitle = "Comuna de Maullín, CPV 2024",
    x = "Nivel de riesgo",
    y = "Número de localidades"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#guardar graficos 1 y 2
ggsave("output/grafico_prom_edad.png", 
       ggplot(datos_selec, aes(x = prom_edad)) +
         geom_histogram(bins = 8, fill = "steelblue", color = "white") +
         labs(title = "Distribución del promedio de edad por localidad",
              subtitle = "Comuna de Maullín, CPV 2024",
              x = "Promedio de edad", y = "Frecuencia") +
         theme_minimal(),
       width = 8, height = 5)


ggsave("output/grafico_riesgo_sanitario.png",
       ggplot(datos_selec, aes(x = riesgo_sanitario, fill = riesgo_sanitario)) +
         geom_bar() +
         scale_fill_manual(values = c("Bajo" = "green4", "Medio" = "orange", "Alto" = "red3")) +
         labs(title = "Nivel de riesgo sanitario por localidad",
              subtitle = "Comuna de Maullín, CPV 2024",
              x = "Nivel de riesgo", y = "Número de localidades") +
         theme_minimal() +
         theme(legend.position = "none"),
       width = 8, height = 5)


#tabla resumen por localidad
tabla_resumen <- datos_selec %>% 
  select(LOCALIDAD, n_per, n_vp, prom_edad, prop_adulto_mayor, 
         prop_discapacidad, prop_fosa, riesgo_sanitario, tipo_serv_hig) %>%
  arrange(desc(prop_fosa))
print(tabla_resumen, n = 37)

# grafico Proporción de fosa séptica por localidad
ggsave("output/grafico_fosa_localidad.png",
       ggplot(datos_selec, aes(x = reorder(LOCALIDAD, prop_fosa), y = prop_fosa)) +
         geom_col(fill = "tomato3") +
         coord_flip() +
         labs(title = "Proporción de viviendas con fosa séptica por localidad",
              subtitle = "Comuna de Maullín, CPV 2024",
              x = "", y = "Proporción") +
         theme_minimal() +
         theme(axis.text.y = element_text(size = 7)),
       width = 8, height = 8)

#Gráfico— Relación entre adulto mayor y fosa séptica
ggsave("output/grafico_dispersion_vulnerabilidad.png",
       ggplot(datos_selec, aes(x = prop_fosa, y = prop_adulto_mayor)) +
         geom_point(aes(color = riesgo_sanitario), size = 3) +
         scale_color_manual(values = c("Bajo" = "green4", "Medio" = "orange", "Alto" = "red3")) +
         labs(title = "Relación entre déficit sanitario y población adulta mayor",
              subtitle = "Localidades de Maullín, CPV 2024",
              x = "Proporción fosa séptica",
              y = "Proporción adultos mayores (+60)",
              color = "Riesgo sanitario") +
         theme_minimal(),
       width = 8, height = 6)

localidades_humedal <- datos_selec %>% 
  filter(LOCALIDAD %in% c("EL HABAL", "COYAM", "PUELPÚN", "MISQUIHUÉ", 
                          "OLMOPULLI", "PEÑOL", "CHUYAQUÉN", "CHILCAS", 
                          "CARIQUILDA"))
nrow(localidades_humedal)

#grafico corregido para las 9 localidades
ggsave("output/grafico_fosa_localidad.png",
       ggplot(localidades_humedal, aes(x = reorder(LOCALIDAD, prop_fosa), y = prop_fosa)) +
         geom_col(fill = "tomato3") +
         coord_flip() +
         labs(title = "Proporción de viviendas con fosa séptica por localidad",
              subtitle = "Localidades colindantes al humedal, Maullín, CPV 2024",
              x = "", y = "Proporción") +
         theme_minimal(),
       width = 8, height = 5)

datos_selec <- datos_selec %>% filter(!is.na(LOCALIDAD))

datos_selec <- datos_selec %>% mutate(
  es_humedal = LOCALIDAD %in% c("EL HABAL", "COYAM", "PUELPÚN", "MISQUIHUÉ", 
                                "OLMOPULLI", "PEÑOL", "CHUYAQUÉN", "CHILCAS", 
                                "CARIQUILDA")
)
table(datos_selec$es_humedal)
ggsave("output/grafico_dispersion_vulnerabilidad.png",
       ggplot(datos_selec, aes(x = prop_fosa, y = prop_adulto_mayor)) +
         geom_point(aes(color = riesgo_sanitario), size = 3) +
         geom_text(data = datos_selec %>% filter(es_humedal == TRUE),
                   aes(label = LOCALIDAD), size = 2.5, nudge_y = 0.015) +
         scale_color_manual(values = c("Bajo" = "green4", "Medio" = "orange", "Alto" = "red3")) +
         labs(title = "Relación entre déficit sanitario y población adulta mayor",
              subtitle = "Localidades de Maullín, CPV 2024",
              x = "Proporción fosa séptica",
              y = "Proporción adultos mayores (+60)",
              color = "Riesgo sanitario") +
         theme_minimal(),
       width = 8, height = 6)


summary(datos_selec %>% select(prom_edad, prop_adulto_mayor, prop_discapacidad, prop_fosa))

table(datos_selec$riesgo_sanitario)
table(datos_selec$tipo_serv_hig)
table(localidades_humedal$riesgo_sanitario)










