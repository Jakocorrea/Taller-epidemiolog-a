## =========================================================
## TALLER 2 - Epidemiología | Medidas de ocurrencia y asociación
## Complemento del Taller 1 (ENSIN 2010)
## - Autoría base (Taller 1): Jacobo Correa | Adaptado para Taller 2
## - Este script genera los outputs numéricos necesarios para el Taller 2:
##   * Prevalencias (global y estratificadas)
##   * Tablas de contingencia con proporciones (celda, fila, columna)
##   * Tabla 7 consolidada
## =========================================================

## ====== [0] Utilidades ======
message_line <- function(txt) {
  cat("\n", paste0(rep("=", 55), collapse = ""),
      "\n", txt, "\n",
      paste0(rep("=", 55), collapse = ""), "\n", sep = "")
}

safe_install <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
}

fix_string_NA <- function(x) {
  if (is.character(x) || is.factor(x)) {
    has_string_NA <- any(x == "NA", na.rm = TRUE)
    if (has_string_NA) {
      x <- as.character(x)
      x[x == "NA"] <- NA
      x <- factor(x)
    }
  }
  x
}

## (Opcional) Usar la carpeta donde está el script como working directory
USE_SCRIPT_DIR <- FALSE
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  i <- grep("--file=", args)
  if (length(i)) return(dirname(normalizePath(sub("--file=", "", args[i]))))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(dirname(p))
  }
  getwd()
}
if (isTRUE(USE_SCRIPT_DIR)) {
  try(setwd(get_script_dir()), silent = TRUE)
}

## ====== [1] Librerías ======
safe_install(c("epiDisplay","car","plyr","psych","modeest"))
library(epiDisplay)
library(car)
library(plyr)
library(psych)
library(modeest)

## ====== [2] Archivos de salida ======
if (!dir.exists("out_taller2")) dir.create("out_taller2")
log_file <- file.path("out_taller2", "output_taller2.txt")
sink(log_file, split = TRUE)
on.exit({ sink(NULL) }, add = TRUE)

message_line("INICIO TALLER 2 - LOG")
cat("Working dir:\n", getwd(), "\n")

## ====== [3] Cargar base ======
DATA_CSV <- "Taller 2/Data_2025.csv"  # ajusta si tu archivo tiene otro nombre

if (!file.exists(DATA_CSV)) {
  stop("No se encontró el archivo de datos: ", DATA_CSV,
       "\nUbícalo en el working directory o ajusta la variable DATA_CSV.")
}

base <- read.csv(DATA_CSV, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

## Normalizar nombres de columnas (robusto, sin errores de escape)
nm <- names(base)
nm <- gsub("[[:space:]]+", "_", nm)  # espacios/tabulaciones → "_"
nm <- gsub("\\.", "_", nm)            # punto literal → "_"
names(base) <- toupper(nm)

cat("\nColumnas (normalizadas):\n"); print(names(base))

## ====== [4] Convertir a factor (variables originales clave) ======
factores_orig <- intersect(c("SEXO","ETNIA","ESCOLARIDAD","OCUPACION","SISBEN","IMCNEW","TLMEETT","QH42"),
                           names(base))
for (v in factores_orig) base[[v]] <- as.factor(base[[v]])

## ====== [5] (Re)codificaciones NUEVAS (consistentes con Taller 1) ======
## 5.a) EDADNEW: 1 = 18–25, 2 = >25
if ("EDAD" %in% names(base)) {
  base$EDADNEW <- cut(base$EDAD, breaks = c(18, 25, 64),
                      labels = c("1","2"), include.lowest = TRUE)
}

## 5.b) SEXO_NEW: 0=Hombre; 1=Mujer
if ("SEXO" %in% names(base)) {
  base$SEXO_NEW <- recode(as.character(base$SEXO), "c('1')='0'; c('2')='1'; else=NA")
  base$SEXO_NEW <- factor(base$SEXO_NEW, levels = c("0","1"), labels = c("Hombre","Mujer"))
}

## 5.c) ETNIA_NEW (binaria, ejemplo)
##     1=minorías (Raizal, Palenquero, Negro/Mulato/Afrocol) -> '1'
##     0=Indígena, Gitanos, Otros -> '0'
if ("ETNIA" %in% names(base)) {
  base$ETNIA_NEW <- recode(as.character(base$ETNIA),
                           "c('3','4','5')='1'; c('1','2','6')='0'; else=NA")
  base$ETNIA_NEW <- factor(base$ETNIA_NEW, levels = c("0","1"),
                           labels = c("No minoría","Minoría étnica"))
}

## 5.d) EXCESO_PESO_NEW: 1 = sobrepeso u obesidad (IMCNEW ∈ {2,3})
if ("IMCNEW" %in% names(base)) {
  base$IMCNEW <- fix_string_NA(base$IMCNEW)
  base$EXCESO_PESO_NEW <- recode(as.character(base$IMCNEW),
                                 "c('2','3')='1'; c('0','1')='0'; else=NA")
  base$EXCESO_PESO_NEW <- factor(base$EXCESO_PESO_NEW, levels = c("0","1"), labels = c("No","Sí"))
}

## 5.e) SISBEN_NEW: 0=Niveles 1–3; 1=Niveles 4–6
if ("SISBEN" %in% names(base)) {
  base$SISBEN_NEW <- recode(as.character(base$SISBEN),
                            "c('1','2','3')='0'; c('4','5','6')='1'; else=NA")
  base$SISBEN_NEW <- factor(base$SISBEN_NEW, levels = c("0","1"),
                            labels = c("Sisbén 1–3","Sisbén 4–6"))
}

## 5.f) ESCOLARIDAD_NEW: 0=Básica/Media; 1=Téc/Univ/Pos
if ("ESCOLARIDAD" %in% names(base)) {
  base$ESCOLARIDAD_NEW <- recode(as.character(base$ESCOLARIDAD),
                                 "c('0','1','2','3')='0'; c('4','5','6','7')='1'; else=NA")
  base$ESCOLARIDAD_NEW <- factor(base$ESCOLARIDAD_NEW, levels = c("0","1"),
                                 labels = c("Básica/Media","Téc/Univ/Pos"))
}

## 5.g) OCUPACION_NEW: 1=Activo; 0=Inactivo
if ("OCUPACION" %in% names(base)) {
  base$OCUPACION_NEW <- recode(as.character(base$OCUPACION),
                               "c('1','2')='1'; c('3','4','5','6','7','8','96','98')='0'; else=NA")
  base$OCUPACION_NEW <- factor(base$OCUPACION_NEW, levels = c("0","1"),
                               labels = c("Inactivo","Activo"))
}

## 5.h) DISCAPACIDAD_NEW: 1=Sí; 0=No
if ("QH42" %in% names(base)) {
  base$DISCAPACIDAD_NEW <- recode(as.character(base$QH42), "c('1')='1'; c('2')='0'; else=NA")
  base$DISCAPACIDAD_NEW <- factor(base$DISCAPACIDAD_NEW, levels = c("0","1"),
                                  labels = c("No","Sí"))
}

## 5.i) TLMEETT_NEW: 1=Cumple; 0=No cumple
if ("TLMEETT" %in% names(base)) {
  base$TLMEETT_NEW <- recode(as.character(base$TLMEETT), "c('1')='1'; c('0')='0'; else=NA")
  base$TLMEETT_NEW <- factor(base$TLMEETT_NEW, levels = c("0","1"),
                             labels = c("No cumple","Cumple"))
}

## 5.j) Grupo etario alterno (18–29 vs 30–64) para ejemplo del taller
if ("EDAD" %in% names(base)) {
  base$EDAD_2GRUPOS <- cut(base$EDAD, breaks = c(18, 29, 64),
                           labels = c("18–29","30–64"), include.lowest = TRUE)
}

## ====== [6] Variables de resultado ======
##  - SOBREPESO_BIN: 1 = sólo sobrepeso (IMCNEW == 2)
##  - IMCNEW2: 1 = sobrepeso u obesidad (IMCNEW ∈ {2,3}); 0 = delgadez/normal
if ("IMCNEW" %in% names(base)) {
  base$IMCNEW <- fix_string_NA(base$IMCNEW)

  base$SOBREPESO_BIN <- ifelse(as.character(base$IMCNEW) == "2", 1,
                               ifelse(as.character(base$IMCNEW) %in% c("0","1","3"), 0, NA))
  base$SOBREPESO_BIN <- factor(base$SOBREPESO_BIN, levels = c(0,1), labels = c("No","Sí"))

  base$IMCNEW2 <- ifelse(as.character(base$IMCNEW) %in% c("2","3"), 1,
                         ifelse(as.character(base$IMCNEW) %in% c("0","1"), 0, NA))
  base$IMCNEW2 <- factor(base$IMCNEW2, levels = c(0,1), labels = c("No","Sí"))
}

## ====== [7] Prevalencias globales ======
message_line("Prevalencias globales")

# Sobrepeso SOLO
if ("SOBREPESO_BIN" %in% names(base)) {
  tb_sp <- table(base$SOBREPESO_BIN, useNA = "ifany")
  print(tb_sp)
  prev_sp <- prop.table(tb_sp)
  cat("\nPrevalencia de sobrepeso (solo IMC=Sobrepeso):\n")
  print(round(100 * prev_sp, 2))
}

# Sobrepeso + Obesidad
if ("IMCNEW2" %in% names(base)) {
  tb_so <- table(base$IMCNEW2, useNA = "ifany")
  print(tb_so)
  prev_so <- prop.table(tb_so)
  cat("\nPrevalencia de sobrepeso+obesidad (IMC>=25):\n")
  print(round(100 * prev_so, 2))
}

# Guardado tolerante a ausencia de niveles
write.csv(
  data.frame(
    indicador = c("sobrepeso_solo_No","sobrepeso_solo_Si",
                  "sobrepeso_obesidad_No","sobrepeso_obesidad_Si"),
    porcentaje = c(
      if (exists("prev_sp")) 100 * as.numeric(prev_sp["No"]) else NA,
      if (exists("prev_sp")) 100 * as.numeric(prev_sp["Sí"]) else NA,
      if (exists("prev_so")) 100 * as.numeric(prev_so["No"]) else NA,
      if (exists("prev_so")) 100 * as.numeric(prev_so["Sí"]) else NA
    )
  ),
  file.path("out_taller2", "prevalencias_globales.csv"),
  row.names = FALSE
)

## ====== [8] Prevalencia SO estratificada (IMCNEW2) ======
message_line("Prevalencia de sobrepeso+obesidad estratificada (IMCNEW2)")

new_vars <- intersect(c("EDADNEW","EDAD_2GRUPOS","SEXO_NEW","ETNIA_NEW","SISBEN_NEW",
                        "ESCOLARIDAD_NEW","OCUPACION_NEW","DISCAPACIDAD_NEW","TLMEETT_NEW"),
                      names(base))

out_dir_strat <- file.path("out_taller2", "tablas_estratificadas")
if (!dir.exists(out_dir_strat)) dir.create(out_dir_strat)

strat_summary <- list()

for (v in new_vars) {
  cat("\n-- ", v, " --\n", sep = "")
  tb <- table(base[[v]], base$IMCNEW2, useNA = "no")  # sin NA
  print(tb)

  # Proporciones
  prop_all <- round(100 * prop.table(tb), 2)
  prop_row <- round(100 * prop.table(tb, 1), 2)
  prop_col <- round(100 * prop.table(tb, 2), 2)

  # Exportar
  write.csv(as.data.frame.matrix(tb),
            file.path(out_dir_strat, paste0("freq_", v, "_x_IMCNEW2.csv")))
  write.csv(as.data.frame.matrix(prop_all),
            file.path(out_dir_strat, paste0("propCELDA_", v, "_x_IMCNEW2.csv")))
  write.csv(as.data.frame.matrix(prop_row),
            file.path(out_dir_strat, paste0("propFILA_", v, "_x_IMCNEW2.csv")))
  write.csv(as.data.frame.matrix(prop_col),
            file.path(out_dir_strat, paste0("propCOLUMNA_", v, "_x_IMCNEW2.csv")))

  # Resumen de prevalencia por categoría (solo si ambas columnas existen)
  if (nrow(tb) > 0 && ncol(tb) == 2) {
    numer <- tb[, "Sí"]
    denom <- rowSums(tb)
    prev_cat <- round(100 * (numer / denom), 2)
    strat_summary[[v]] <- data.frame(
      variable = v,
      categoria = names(prev_cat),
      n = as.integer(denom),
      prevalencia_SO_pct = as.numeric(prev_cat),
      stringsAsFactors = FALSE
    )
  }
}

if (length(strat_summary)) {
  df_strat <- do.call(rbind, strat_summary)
  write.csv(df_strat,
            file.path("out_taller2", "prevalencia_SO_por_variable.csv"),
            row.names = FALSE)
  cat("\n[OK] Exportado: out_taller2/prevalencia_SO_por_variable.csv\n")
}

## ====== [9] TABLA 7 (Variable, Categoría, Frecuencia, %, Prevalencia SO) ======
message_line("Tabla 7 (para manuscrito): Frecuencia, % y Prevalencia SO por categoría")

vars_tabla7 <- intersect(c("EDADNEW","EDAD_2GRUPOS","SEXO_NEW","ETNIA_NEW","SISBEN_NEW",
                           "ESCOLARIDAD_NEW","OCUPACION_NEW","DISCAPACIDAD_NEW","TLMEETT_NEW"),
                         names(base))

arma_tabla7 <- function(var, outcome = "IMCNEW2") {
  x <- base[[var]]
  y <- base[[outcome]]
  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) {
    return(data.frame(
      Variable = character(0), Categoria = character(0),
      Frecuencia = integer(0), Porcentaje = numeric(0),
      Prevalencia_SO_pct = numeric(0)
    ))
  }

  x <- droplevels(as.factor(x[ok]))
  y <- droplevels(as.factor(y[ok]))

  # Mantener categorías ANTES de convertir a integer (evita perder nombres)
  tab_x <- table(x)
  cats  <- names(tab_x)      # categorías del estrato
  n_cat <- as.integer(tab_x) # frecuencias por categoría
  pct_cat <- round(100 * (n_cat / sum(n_cat)), 2)

  # Cruce como matriz (evita dropeo raro)
  tb <- as.matrix(table(x, y))

  # Numerador seguro: si no existe la columna "Sí", usar 0s
  numer <- if (!is.null(colnames(tb)) && "Sí" %in% colnames(tb)) tb[, "Sí"] else rep(0L, length(cats))
  denom <- rowSums(tb)
  prev_cat <- round(100 * ifelse(denom > 0, numer / denom, NA), 2)

  data.frame(
    Variable = var,
    Categoria = cats,
    Frecuencia = n_cat,
    Porcentaje = pct_cat,
    Prevalencia_SO_pct = as.numeric(prev_cat),
    stringsAsFactors = FALSE
  )
}

tabla7_list <- lapply(vars_tabla7, arma_tabla7)
tabla7 <- do.call(rbind, tabla7_list)
row.names(tabla7) <- NULL
print(tabla7)

write.csv(tabla7, file.path("out_taller2", "taller2_tabla7_pub.csv"), row.names = FALSE)
cat("\n[OK] Exportado: out_taller2/taller2_tabla7_pub.csv\n")

## ====== [10] Checklist rápido ======
message_line("Checklist de outputs")
cat("Generado en out_taller2/ :\n",
    "- output_taller2.txt (este log)\n",
    "- prevalencias_globales.csv\n",
    "- tablas_estratificadas/ (frecuencias y proporciones)\n",
    "- prevalencia_SO_por_variable.csv\n",
    "- taller2_tabla7_pub.csv (Tabla 7 final)\n", sep = "")

message_line("FIN TALLER 2 - LOG")
