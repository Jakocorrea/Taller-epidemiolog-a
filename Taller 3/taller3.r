# ======================================
# TALLER 3 R – MEDIDAS DE ASOCIACIÓN
# ======================================

# Cargar librerías
library(epiDisplay)
library(car)
library(dplyr)

# 1. Base de datos (ya limpia del Taller 2)
base <- read.csv("Data_2025.csv")

# 2. Variables binarias 0/1 (ajusta nombres si difieren)
# Outcome: sobrepeso/obesidad (1 = sí, 0 = no)
# Variables explicativas: edad (18–25 = 0, >25 = 1), sexo, etc.
base <- base %>%
  mutate(
    EDAD_25 = ifelse(EDAD <= 25, 0, 1),
    SEXO_bin = ifelse(SEXO == "Mujer", 1, 0),
    ETNIA_bin = ifelse(ETNIA == "Minoría", 1, 0),
    SISBEN_bin = ifelse(SISBEN %in% c("4–6"), 1, 0),
    ESCOLARIDAD_bin = ifelse(ESCOLARIDAD %in% c("Téc / Univ / Pos"), 1, 0),
    OCUPACION_bin = ifelse(OCUPACION == "Activo", 1, 0),
    DISCAP_bin = ifelse(DISCAPACIDAD == "Sí", 1, 0),
    TLMEETT_bin = ifelse(TLMEETT == "Cumple", 1, 0)
  )

# 3. Función para calcular POR, IC y precisión
calc_por <- function(var){
  tab <- table(base$SO, base[[var]])
  or <- oddsratio(tab)$measure[2,1]
  lci <- oddsratio(tab)$measure[2,2]
  lcs <- oddsratio(tab)$measure[2,3]
  se  <- (log(lcs) - log(lci)) / (2*1.96)
  precision <- lcs / lci
  p <- chisq.test(tab)$p.value
  data.frame(Variable = var, POR = or, LI = lci, LS = lcs, SE = se, Precisión = precision, p_value = p)
}

# 4. Aplicar a todas las variables
vars <- c("EDAD_25", "SEXO_bin", "ETNIA_bin", "SISBEN_bin", "ESCOLARIDAD_bin", "OCUPACION_bin", "DISCAP_bin", "TLMEETT_bin")
tabla_por <- do.call(rbind, lapply(vars, calc_por))

# 5. Exportar
write.csv(tabla_por, "Resultados_Taller3R.csv", row.names = FALSE)
tabla_por
