#### ============================================================
#### TALLER 2 R - TABLA 7 Y PÁRRAFO CORREGIDOS (final sin errores)
#### ============================================================

library(dplyr)

# 1. Cargar base -------------------------------------------------
data <- read.csv("Taller 2/Data_2025.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# 2. Ajustar nombres ---------------------------------------------
data <- data %>%
  rename(
    IMC = imcnew,
    TLMEETT = Tlmeett
  )

# 3. Variable EDAD_NEW (solo 18–25 / >25) ------------------------
data <- data %>%
  mutate(
    EDAD_NEW = case_when(
      !is.na(EDAD) & EDAD >= 18 & EDAD <= 25 ~ 1,
      !is.na(EDAD) & EDAD > 25 ~ 2,
      TRUE ~ NA_real_
    ),
    EDAD_NEW_LABEL = case_when(
      EDAD_NEW == 1 ~ "18–25",
      EDAD_NEW == 2 ~ ">25",
      TRUE ~ NA_character_
    )
  )

# 4. Variable sobrepeso / obesidad -------------------------------
data <- data %>%
  mutate(
    SO = ifelse(!is.na(IMC) & IMC >= 25, 1,
                ifelse(!is.na(IMC), 0, NA))
  )

# 5. Función para tabla por variable -----------------------------
make_block <- function(df, var_name, nice_name = NULL) {
  if (is.null(nice_name)) nice_name <- var_name
  sub <- df[!is.na(df[[var_name]]), ]
  total_var <- nrow(sub)
  
  out <- sub %>%
    group_by(.data[[var_name]]) %>%
    summarise(
      Frecuencia = n(),
      Porcentaje = 100 * n() / total_var,
      Prevalencia_SO = {
        rows_cat <- sub[sub[[var_name]] == unique(.data[[var_name]]) & !is.na(sub$SO), ]
        if (nrow(rows_cat) == 0) NA_real_ else 100 * mean(rows_cat$SO == 1)
      },
      .groups = "drop"
    )
  
  # asegurar que todas las categorías sean texto
  out <- out %>%
    mutate(
      Categoria = as.character(.data[[var_name]]),
      Variable = nice_name
    ) %>%
    select(Variable, Categoria, Frecuencia, Porcentaje, Prevalencia_SO)
  
  return(out)
}

# 6. Calcular bloques --------------------------------------------
block_edad <- make_block(data %>% mutate(EDAD_BIN = EDAD_NEW_LABEL), "EDAD_BIN", "EDAD_NEW")
block_sexo <- make_block(data, "SEXO", "SEXO")
block_etnia <- make_block(data, "ETNIA", "ETNIA")
block_sisben <- make_block(data, "SISBEN", "SISBEN")
block_escolaridad <- make_block(data, "ESCOLARIDAD", "ESCOLARIDAD")
block_ocupacion <- make_block(data, "OCUPACION", "OCUPACION")
block_tlmeett <- make_block(data, "TLMEETT", "TLMEETT")

# 7. Unir todo ----------------------------------------------------
tabla7 <- bind_rows(
  block_edad,
  block_sexo,
  block_etnia,
  block_sisben,
  block_escolaridad,
  block_ocupacion,
  block_tlmeett
) %>%
  mutate(
    Porcentaje = round(Porcentaje, 2),
    Prevalencia_SO = round(Prevalencia_SO, 2)
  )

# 8. Exportar -----------------------------------------------------
write.csv(tabla7, "Correccion_Tabla7_Taller2_R.csv", row.names = FALSE)

# 9. Crear párrafo descriptivo -----------------------------------
get_val <- function(var, cat, metric){
  val <- tabla7 %>%
    filter(Variable == var, Categoria == cat) %>%
    pull(metric)
  if (length(val) == 0) return(NA)
  val[1]
}

n_total <- nrow(data)

p_juv <- get_val("EDAD_NEW", "18–25", "Porcentaje")
p_ad <- get_val("EDAD_NEW", ">25", "Porcentaje")
prev_juv <- get_val("EDAD_NEW", "18–25", "Prevalencia_SO")
prev_ad <- get_val("EDAD_NEW", ">25", "Prevalencia_SO")

p_muj <- get_val("SEXO", "2", "Porcentaje")
p_hom <- get_val("SEXO", "1", "Porcentaje")
prev_muj <- get_val("SEXO", "2", "Prevalencia_SO")
prev_hom <- get_val("SEXO", "1", "Prevalencia_SO")

p_cumple <- get_val("TLMEETT", "1", "Porcentaje")
p_nocumple <- get_val("TLMEETT", "0", "Porcentaje")
prev_cumple <- get_val("TLMEETT", "1", "Prevalencia_SO")
prev_nocumple <- get_val("TLMEETT", "0", "Prevalencia_SO")

parrafo <- paste0(
  "La muestra analizada (n=", n_total, ") está compuesta por ",
  sprintf("%.1f", p_juv), "% de adultos jóvenes (18–25 años) y ",
  sprintf("%.1f", p_ad), "% de mayores de 25 años; ",
  sprintf("%.1f", p_muj), "% mujeres y ",
  sprintf("%.1f", p_hom), "% hombres. ",
  "La prevalencia de sobrepeso/obesidad fue de ",
  sprintf("%.1f", prev_juv), "% en 18–25 años y ",
  sprintf("%.1f", prev_ad), "% en mayores de 25 años; ",
  "ligeramente mayor en mujeres (",
  sprintf("%.1f", prev_muj), "%) que en hombres (",
  sprintf("%.1f", prev_hom), "%). ",
  "Cumplir las recomendaciones de actividad física se asoció con menor prevalencia (",
  sprintf("%.1f", prev_cumple), "% en 'Cumple' vs ",
  sprintf("%.1f", prev_nocumple), "% en 'No cumple')."
)

cat("\n\nPÁRRAFO DESCRIPTIVO:\n", parrafo, "\n\n")
#### ============================================================
