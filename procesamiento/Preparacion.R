# =============================================================
# Preparacion.R
# Trabajo 4 — Vulnerabilidad Sanitaria Periurbana, Maullín
# Fuente: CPV 2024, INE Chile
# =============================================================

pacman::p_load(readxl, dplyr, ggplot2)

# ── 1. CARGA DE DATOS ─────────────────────────────────────────
datos <- read_excel("input/Localidad_CPV24.xlsx")

# ── 2. SELECCIÓN Y TRANSFORMACIÓN DE VARIABLES ────────────────
datos_selec <- datos %>%
  select(LOCALIDAD, n_per, n_vp, n_edad_60_mas, prom_edad,
         n_discapacidad, n_serv_hig_fosa, n_serv_hig_pozo,
         n_serv_hig_alc_dentro, n_serv_hig_alc_fuera,
         n_serv_hig_no_tiene, n_tipo_viv_mediagua,
         n_basura_entierra, n_fuente_agua_pozo) %>%
  filter(!is.na(LOCALIDAD)) %>%
  mutate(
    # Variables de razón
    prop_adulto_mayor = n_edad_60_mas / n_per,
    prop_discapacidad = n_discapacidad / n_per,
    prop_fosa         = n_serv_hig_fosa / n_vp,
    prop_mediagua     = n_tipo_viv_mediagua / n_vp,
    prop_agua_pozo    = n_fuente_agua_pozo / n_vp,
    deficit_total     = prop_fosa + (n_serv_hig_pozo / n_vp),

    # Variable ordinal: riesgo sanitario
    riesgo_sanitario = case_when(
      deficit_total > 0.5 ~ "Alto",
      deficit_total > 0.2 ~ "Medio",
      TRUE                ~ "Bajo"
    ),
    riesgo_sanitario = factor(riesgo_sanitario,
                              levels = c("Bajo", "Medio", "Alto"),
                              ordered = TRUE),

    # Variable nominal: tipo de servicio higiénico predominante
    tipo_serv_hig = case_when(
      n_serv_hig_alc_dentro + n_serv_hig_alc_fuera > n_serv_hig_fosa &
        n_serv_hig_alc_dentro + n_serv_hig_alc_fuera > n_serv_hig_pozo ~ "Alcantarillado",
      n_serv_hig_fosa > n_serv_hig_pozo ~ "Fosa séptica",
      TRUE ~ "Pozo o sin servicio"
    ),
    tipo_serv_hig = factor(tipo_serv_hig),

    # Identificador de localidades del humedal
    es_humedal = LOCALIDAD %in% c("EL HABAL", "COYAM", "PUELPÚN", "MISQUIHUÉ",
                                   "OLMOPULLI", "PEÑOL", "CHUYAQUÉN", "CHILCAS",
                                   "CARIQUILDA")
  )

# ── 3. FILTRO: solo localidades del humedal ───────────────────
localidades_humedal <- datos_selec %>% filter(es_humedal == TRUE)

# ── 4. ÍNDICE IVSP ────────────────────────────────────────────
# Pesos teóricos justificados por SIVUST/MIDESO 2020:
# fosa 35% | agua pozo 20% | adultos mayores 20% | discapacidad 15% | mediagua 10%
localidades_humedal <- localidades_humedal %>%
  mutate(
    ivsp_raw = (prop_fosa         * 0.35 +
                prop_agua_pozo    * 0.20 +
                prop_adulto_mayor * 0.20 +
                prop_discapacidad * 0.15 +
                prop_mediagua     * 0.10),
    ivsp = round((ivsp_raw - min(ivsp_raw, na.rm = TRUE)) /
                 (max(ivsp_raw, na.rm = TRUE) - min(ivsp_raw, na.rm = TRUE)) * 100, 1)
  )

# ── 5. VARIABLES PARA REGRESIÓN ───────────────────────────────

# Variable dependiente dicotómica para regresión logística:
# ivsp_alto = 1 si IVSP >= mediana del grupo, 0 si no
mediana_ivsp <- median(localidades_humedal$ivsp, na.rm = TRUE)

localidades_humedal <- localidades_humedal %>%
  mutate(
    ivsp_alto = as.integer(ivsp >= mediana_ivsp)
  )

# ── 6. GUARDAR DATOS PROCESADOS ───────────────────────────────
proc_data <- localidades_humedal

save(proc_data, file = "output/datos_proc.RData")

# ── 7. EXPORTAR GRÁFICOS ──────────────────────────────────────

# Gráfico 1: Riesgo sanitario (toda la comuna)
ggsave("output/grafico_riesgo_sanitario.png",
       ggplot(datos_selec, aes(x = riesgo_sanitario, fill = riesgo_sanitario)) +
         geom_bar() +
         scale_fill_manual(values = c("Bajo" = "green4", "Medio" = "orange", "Alto" = "red3")) +
         labs(title = "Nivel de riesgo sanitario por localidad",
              subtitle = "Comuna de Maullín, CPV 2024",
              x = "Nivel de riesgo", y = "Número de localidades") +
         theme_minimal() + theme(legend.position = "none"),
       width = 8, height = 5)

# Gráfico 2: Fosa séptica localidades del humedal
ggsave("output/grafico_fosa_localidad.png",
       ggplot(localidades_humedal, aes(x = reorder(LOCALIDAD, prop_fosa), y = prop_fosa)) +
         geom_col(fill = "tomato3") + coord_flip() +
         labs(title = "Proporción de viviendas con fosa séptica",
              subtitle = "Localidades colindantes al humedal, Maullín, CPV 2024",
              x = "", y = "Proporción") +
         theme_minimal(),
       width = 8, height = 5)

message("✓ Preparacion.R ejecutado correctamente.")
message(paste("  n localidades humedal:", nrow(proc_data)))
message(paste("  Mediana IVSP:", mediana_ivsp))

write.csv(
  proc_data %>% select(LOCALIDAD, ivsp, riesgo_sanitario, ivsp_alto),
  "output/ivsp_localidades.csv",
  row.names = FALSE
)
