# ============================================================
# Procesamiento de datos — Vulnerabilidad Territorial Maullín
# Fuente: CPV 2024 (INE) / DGA-PERHC Maullín 2025
# Protocolo IPO — files/tablas_maullin.R
# ============================================================

# Cargar paquetes con pacman (método del curso)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,      # Manipulación de datos
  kableExtra, # Tablas
  knitr       # Renderizado
)

# ============================================================
# INPUT — Cargar base de datos desde files/data/
# ============================================================

# Los datos del CPV 2024 se ingresan directamente como data.frame
# ya que no existe descarga pública automatizada por localidad

# Base saneamiento (n_vp = viviendas particulares)
saneamiento <- data.frame(
  Localidad  = c("Misquihué","Peñol","Olmopulli","Astilleros",
                 "Chuyaquén","Chilcas","Cariquilda","TOTAL"),
  Med_n      = c(2, 0, 0, 0, 0, 0, 0, 2),
  Med_pct    = c("9%","0%","0%","0%","0%","0%","0%","0,1%"),
  Fosa_n     = c(66, 101, 54, 92, 66, 54, 51, 484),
  Fosa_pct   = c("30%","28%","24%","31%","33%","43%","38%","31%"),
  Pozo_n     = c(103, 56, 59, 38, 44, 28, 58, 386),
  Pozo_pct   = c("48%","16%","26%","13%","22%","22%","43%","25%"),
  Basura_n   = c(48, 90, 27, 24, 15, 4, 12, 220),
  Basura_pct = c("22%","25%","12%","8%","7%","3%","9%","14%"),
  nvp        = c(217, 361, 229, 297, 202, 125, 136, 1567)
)

# Base vulnerabilidad social (n_per = personas)
vulnerabilidad <- data.frame(
  Localidad = c("Misquihué","Peñol","Olmopulli","Astilleros",
                "Chuyaquén","Chilcas","Cariquilda","TOTAL"),
  Disc_n    = c(73, 101, 74, 103, 55, 38, 43, 487),
  Disc_pct  = c("18%","17%","21%","18%","18%","16%","20%","18%"),
  AM_n      = c(166, 237, 169, 148, 110, 64, 76, 970),
  AM_pct    = c("40%","39%","48%","26%","36%","27%","35%","36%"),
  nper      = c(411, 610, 353, 568, 307, 240, 219, 2708)
)

# Base variables IPO
variables <- data.frame(
  Variable = c("Fosa séptica","Pozo","Adultos mayores +60",
               "Discapacidad","Mediagua","Basura enterrada"),
  Fórmula  = c("n_serv_hig_fosa ÷ n_vp",
               "n_fuente_agua_pozo ÷ n_vp",
               "n_edad_60_mas ÷ n_per",
               "n_discapacidad ÷ n_per",
               "n_tipo_viv_mediagua ÷ n_vp",
               "n_basura_entierra ÷ n_vp"),
  Fuente   = rep("CPV 2024", 6)
)

# ============================================================
# PROCESAMIENTO — Construir tablas con kableExtra
# ============================================================

# Tabla variables IPO
tabla_variables <- kable(variables, align = "lll") |>
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE)

# Tabla 1 — Vivienda y saneamiento
tabla1 <- kable(saneamiento,
      col.names = c("Localidad","n°","%","n°","%",
                    "n°","%","n°","%","n_vp"),
      align = "lrrrrrrrrrr") |>
  kable_styling(bootstrap_options = c("striped","hover"),
                full_width = TRUE) |>
  add_header_above(c(" "=1,"Mediagua"=2,"Fosa séptica"=2,
                     "Pozo"=2,"Basura en tierra"=2," "=1)) |>
  row_spec(8, bold = TRUE)

# Tabla 2 — Vulnerabilidad social
tabla2 <- kable(vulnerabilidad,
      col.names = c("Localidad","n°","%","n°","%","n_per"),
      align = "lrrrrrr") |>
  kable_styling(bootstrap_options = c("striped","hover"),
                full_width = TRUE) |>
  add_header_above(c(" "=1,"Discapacidad"=2,
                     "Adultos mayores +60"=2," "=1)) |>
  row_spec(8, bold = TRUE)

# ============================================================
# OUTPUT — objetos disponibles para index.qmd:
#   tabla_variables, tabla1, tabla2
# ============================================================
