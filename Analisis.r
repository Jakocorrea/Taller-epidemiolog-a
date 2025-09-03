## =========================================================
## TALLER 1 - Epidemiología | Solucion completa
## - Realizado por: Jacobo Correa
## =========================================================

## ----- [setwd] Ruta de trabajo -----
setwd("/Users/jakosmacbook/Downloads/Silvi")   # <-- Ajusta a tu carpeta

## ----- Exporta OUTPUT en output_taller.txt -----
sink("output_taller.txt", split = TRUE)  

## ----- [9] Instalar librerías -----
install.packages(c("epiDisplay", "car", "plyr", "psych", "modeest"),
                 repos = "https://cloud.r-project.org")

## ----- [10] Cargar librerías -----
library(epiDisplay)
library(car)
library(plyr)
library(psych)
library(modeest)

## ----- [11] Cargar la base -----
base <- read.csv("Data_2025.csv", header = TRUE,
                 stringsAsFactors = FALSE, check.names = FALSE)

## ----- Normalizar nombres (parche clave) -----
names(base) <- toupper(gsub("\\.", "_", gsub("\\s+", "_", names(base))))
cat("\nColumnas (normalizadas):\n"); print(names(base))

## ----- [12] Verificar diccionario / contenido -----
cat("\nResumen general del data set:\n")
print(summary(base))
it12 <- TRUE

## ----- [13] Convertir variables necesarias a factor -----
factores <- intersect(c("SEXO","ETNIA","ESCOLARIDAD","OCUPACION",
                        "SISBEN","IMCNEW","TLMEETT","QH42"),
                      names(base))
for (v in factores) base[[v]] <- as.factor(base[[v]])
it13 <- all(sapply(factores, \(v) is.factor(base[[v]])))

## ----- [14] Re-codificaciones (Tabla #1) -----

# 14.a) Numérica -> categórica con cut(): EDADNEW (1=18–25; 2=>25)
if ("EDAD" %in% names(base)) {
  base$EDADNEW <- cut(base$EDAD, breaks = c(18, 25, 64),
                      labels = c("1","2"), include.lowest = TRUE)
}

# 14.b) Categóricas con car::recode()
if ("SEXO" %in% names(base)) {
  base$SEXO_NEW <- recode(as.character(base$SEXO),
                          "c('1')='0'; c('2')='1'; else=NA")
  base$SEXO_NEW <- factor(base$SEXO_NEW, levels = c("0", "1"),
                          labels=c("Hombre", "Mujer"))
  cat("\n[OK] SEXO_NEW creada\n")
  print(table(base$SEXO, base$SEXO_NEW, useNA = "always"))
}
if ("IMCNEW" %in% names(base)) {
  base$EXCESO_PESO_NEW <- recode(as.character(base$IMCNEW),
                                 "c('2','3')='1'; c('0','1')='0'; else=NA")
  base$EXCESO_PESO_NEW <- factor(base$EXCESO_PESO_NEW, levels=c("0","1"),
                                 labels=c("No","Sí"))
  cat("\n[OK] EXCESO_PESO_NEW creada\n")
  print(table(base$IMCNEW, base$EXCESO_PESO_NEW, useNA="always"))
}
if ("SISBEN" %in% names(base)) {
  base$SISBEN_NEW <- recode(as.character(base$SISBEN),
                            "c('1','2','3')='0'; c('4','5','6')='1'; else=NA")
  base$SISBEN_NEW <- factor(base$SISBEN_NEW, levels=c("0","1"),
                            labels=c("Niveles 1–3","Niveles 4–6"))
}
if ("ESCOLARIDAD" %in% names(base)) {
  base$ESCOLARIDAD_NEW <- recode(as.character(base$ESCOLARIDAD),
                                 "c('0','1','2','3')='0'; c('4','5','6','7')='1'; else=NA")
  base$ESCOLARIDAD_NEW <- factor(base$ESCOLARIDAD_NEW, levels=c("0","1"),
                                 labels=c("Básica/Media","Téc/Univ/Pos"))
}
if ("OCUPACION" %in% names(base)) {
  base$OCUPACION_NEW <- recode(as.character(base$OCUPACION),
                               "c('1','2')='1'; c('3','4','5','6')='0'; else=NA")
  base$OCUPACION_NEW <- factor(base$OCUPACION_NEW, levels=c("0","1"),
                               labels=c("Inactivo","Activo"))
}
if ("QH42" %in% names(base)) {
  base$DISCAPACIDAD_NEW <- recode(as.character(base$QH42),
                                  "c('1')='1'; c('2')='0'; else=NA")
  base$DISCAPACIDAD_NEW <- factor(base$DISCAPACIDAD_NEW, levels=c("0","1"),
                                  labels=c("No","Sí"))
}
if ("TLMEETT" %in% names(base)) {
  base$TLMEETT_NEW <- recode(as.character(base$TLMEETT),
                             "c('1')='1'; c('0')='0'; else=NA")
  base$TLMEETT_NEW <- factor(base$TLMEETT_NEW, levels=c("0","1"),
                             labels=c("No cumple","Cumple"))
}

vars_new <- intersect(c("EDADNEW","SEXO_NEW","EXCESO_PESO_NEW","SISBEN_NEW",
                        "ESCOLARIDAD_NEW","OCUPACION_NEW","DISCAPACIDAD_NEW","TLMEETT_NEW"),
                      names(base))
it14 <- length(vars_new) > 0

## ----- [15] Verificación original vs nueva -----
cat("\n=== Ítem 15: Verificación original vs nueva ===\n")
ver_ok <- c()
ver_pair <- function(orig, neu) {
  if (all(c(orig, neu) %in% names(base))) {
    cat("\nTabla", orig, "vs", neu, "(con NA):\n")
    print(table(base[[orig]], base[[neu]], useNA="always"))
    cat("summary(", orig, ")\n"); print(summary(base[[orig]]))
    cat("summary(", neu, ")\n");  print(summary(base[[neu]]))
    return(TRUE)
  }
  FALSE
}
ver_ok <- c(ver_ok, ver_pair("SEXO", "SEXO_NEW"))
ver_ok <- c(ver_ok, ver_pair("IMCNEW", "EXCESO_PESO_NEW"))
ver_ok <- c(ver_ok, ver_pair("SISBEN", "SISBEN_NEW"))
ver_ok <- c(ver_ok, ver_pair("ESCOLARIDAD", "ESCOLARIDAD_NEW"))
ver_ok <- c(ver_ok, ver_pair("OCUPACION", "OCUPACION_NEW"))
ver_ok <- c(ver_ok, ver_pair("QH42", "DISCAPACIDAD_NEW"))
if ("EDADNEW" %in% names(base)) {
  cat("\nsummary(EDAD)\n"); print(summary(base$EDAD))
  cat("summary(EDADNEW)\n"); print(summary(base$EDADNEW))
  ver_ok <- c(ver_ok, TRUE)
}
it15 <- any(ver_ok)

## ----- [16] Frecuencias con NA (ROBUSTO) -----
cat("\n=== ÍTEM 16: Frecuencias con NA (table(..., useNA='always')) ===\n")
# función para corregir "NA" como texto
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
freq_all16 <- data.frame(variable = character(), categoria = character(),
                         n = integer(), stringsAsFactors = FALSE)
for (v in vars_new) {
  cat("\n-- ", v, " --\n", sep = "")
  base[[v]] <- fix_string_NA(base[[v]])
  tb <- table(base[[v]], useNA = "always")
  print(tb)
  lvls <- rownames(tb)
  lvls[is.na(lvls)] <- "<NA>"
  freq_all16 <- rbind(freq_all16,
                      data.frame(variable = v,
                                 categoria = lvls,
                                 n = as.integer(tb),
                                 stringsAsFactors = FALSE))
}
write.csv(freq_all16, "frecuencias_item16_con_NA.csv", row.names = FALSE)
cat("\n[OK] Ítem 16 exportado a frecuencias_item16_con_NA.csv\n")
it16 <- nrow(freq_all16) > 0

## ----- [17] Frecuencias sin NA (ROBUSTO) -----
cat("\n=== ÍTEM 17: Frecuencias SIN NA (table(...)) ===\n")
freq_all17 <- data.frame(variable = character(), categoria = character(),
                         n = integer(), stringsAsFactors = FALSE)
for (v in vars_new) {
  cat("\n-- ", v, " --\n", sep = "")
  tb <- table(base[[v]])  # excluye NA por defecto
  print(tb)
  if (length(tb) > 0) {
    freq_all17 <- rbind(freq_all17,
                        data.frame(variable = v,
                                   categoria = names(tb),
                                   n = as.integer(tb),
                                   stringsAsFactors = FALSE))
  } else {
    freq_all17 <- rbind(freq_all17,
                        data.frame(variable = v, categoria = "(sin categorías)",
                                   n = 0, stringsAsFactors = FALSE))
  }
}
write.csv(freq_all17, "frecuencias_item17_sin_NA.csv", row.names = FALSE)
cat("\n[OK] Ítem 17 exportado a frecuencias_item17_sin_NA.csv\n")
it17 <- nrow(freq_all17) > 0

## ----- [18] Histograma de EDAD (guardado) -----
it18 <- FALSE
if ("EDAD" %in% names(base)) {
  png("hist_EDAD.png", width=900, height=600, res=120)
  hist(base$EDAD, breaks=30, main="Distribución de la Edad (18–64)",
       xlab="Edad (años)")
  dev.off()
  cat("\n[OK] Histograma de EDAD guardado en hist_EDAD.png\n")
  it18 <- TRUE
}

## ----- [19] Descriptivos de EDAD -----
it19 <- FALSE
if ("EDAD" %in% names(base)) {
  cat("\n==== Descriptivos de EDAD ====\n")
  print(psych::describe(base$EDAD))
  cat("\nMedia: ", mean(base$EDAD, na.rm=TRUE))
  cat("\nDE: ", sd(base$EDAD, na.rm=TRUE))
  cat("\nMediana: ", median(base$EDAD, na.rm=TRUE))
  cat("\nModa (mlv): ", as.numeric(mlv(base$EDAD, method="mfv")))
  cat("\nPercentiles 5 y 95:\n"); print(quantile(base$EDAD, c(0.05,0.95), na.rm=TRUE))
  it19 <- TRUE
}

## ----- CHECKLIST 12–19 -----
.ok <- function(cond) if (isTRUE(cond)) "✔ PASÓ" else "✖ NO PASÓ"
.sep <- function(t) cat("\n", paste0(rep("=",12), collapse=""), " ", t, " ",
                        paste0(rep("=",12), collapse=""), "\n", sep = "")
.sep("RESUMEN CHECKLIST")
cat("12 summary(base):     ", .ok(it12), "\n")
cat("13 as.factor():       ", .ok(it13), "\n")
cat("14 cut/recode NEW:    ", .ok(it14), "\n")
cat("15 table/summary:     ", .ok(it15), "\n")
cat("16 table(..., NA):    ", .ok(it16), "\n")
cat("17 table(...) sin NA: ", .ok(it17), "\n")
cat("18 hist():            ", .ok(it18), "\n")
cat("19 describe/med/etc.: ", .ok(it19), "\n")
cat("\nArchivos generados:\n",
    "- output_taller.txt (toda la consola)\n",
    "- frecuencias_item16_con_NA.csv (todas las NEW con NA)\n",
    "- frecuencias_item17_sin_NA.csv (todas las NEW sin NA)\n",
    "- hist_EDAD.png (gráfico)\n", sep="")

## ----- Cerrar sink (MK MUUUUYYYYYYY importante) -----
sink()
