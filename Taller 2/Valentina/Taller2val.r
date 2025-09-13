setwd("/Users/jakosmacbook/Documents/GitHub/Taller-epidemiolog-a/Taller 2/Valentina") 

# instalar librerias
install.packages(c("epiDisplay","car","plyr","psych","modeest"))	

# cargar librerias
library(epiDisplay) #Esta librería contiene funciones necesarias para análisis epidemiológicos
library(car) #Esta librería contiene funciones necesarias para análisis estadísticos 
library(plyr) #Esta librería contiene funciones para transformación de datos 
library(psych) #Esta librería contiene funciones análisis de variables continuas
library(modeest) #Esta librería contiene funciones para estimar la moda

# cargar base de datos
base<-read.csv("Data_2025.csv")

# convertir variables a factores
base$ETNIA<-as.factor(base$ETNIA)
base$OCUPACION<-as.factor(base$OCUPACION)
base$QH42<-as.factor(base$QH42)
base$SEXO<-as.factor(base$SEXO)
base$ESCOLARIDAD<-as.factor(base$ESCOLARIDAD)
base$SISBEN<-as.factor(base$SISBEN)
base$imcnew<-as.factor(base$imcnew)
base$Tlmeett<-as.factor(base$Tlmeett)
# RECATEGORIZACION DE vatiables catecoricas
base$ETNIA2<-recode(base$ETNIA,"c('1', '2','6')='0';c('3','4','5') = '1';else=NA")
base$ESCOLARIDADnew<-recode(base$ESCOLARIDAD,"c('0', '1','2','3')='0';c('4','5','6') = '1';else=NA")
base$OCUPACIONnew<-recode(base$OCUPACION,"c('1', '2')='0';c('3','4','5','6','7') = '1';else=NA")
base$QH42NEW<-recode(base$QH42,"c('2')='0';c('1') = '1';else=NA")
base$SEXONEW<-recode(base$SEXO,"c('1')='0';c('2') = '1';else=NA")
base$SISBENNEW<-recode(base$SISBEN,"c('1', '2','3')='0';c('4') = '1';else=NA")
base$TlmeettNEW<-recode(base$Tlmeett,"c('0')='0';c('1') = '1';else=NA")
base$imcnewNEW<-recode(base$imcnew,"c('0', '1')='0';c('2','3') = '1';else=NA")

# recategorizacion de variables numericas
base$EDADnew<-cut(base$EDAD,breaks = c(18,25,64), labels = c("1","2"),include.lowest = TRUE)

# Analisis univariado de variables categoricas frecuencias
table(base$ETNIA2,useNA = "always")
table(base$ESCOLARIDADnew,useNA = "always")
table(base$OCUPACIONnew,useNA = "always")
table(base$QH42NEW,useNA = "always")
table(base$SEXONEW,useNA = "always")
table(base$SISBENNEW,useNA = "always")
table(base$EDADnew,useNA = "always")
table(base$TlmeettNEW,useNA = "always")
table(base$imcnewNEW,useNA = "always")

# tabule las frecuencias, excluyendo los valores missing de todas las variables nuevas.
table(base$ETNIA2)
table(base$ESCOLARIDADnew)
table(base$OCUPACIONnew)
table(base$QH42NEW)
table(base$SEXONEW)
table(base$SISBENNEW)
table(base$EDADnew)
table(base$TlmeettNEW)
table(base$imcnewNEW)

# comparacion de tablas con las variables viejas
table(base$ETNIA,base$ETNIA2,useNA = "always")
table(base$ESCOLARIDAD,base$ESCOLARIDADnew,useNA = "always")
table(base$OCUPACION,base$OCUPACIONnew,useNA = "always")
table(base$QH42,base$QH42NEW,useNA = "always")
table(base$SEXO,base$SEXONEW,useNA = "always")
table(base$SISBEN,base$SISBENNEW,useNA = "always")
table(base$EDAD,base$EDADnew,useNA = "always")
table(base$Tlmeett,base$TlmeettNEW,useNA = "always")
table(base$imcnew,base$imcnewNEW,useNA = "always")

# Analísis univariado de las variables continuas 
hist(base$EDAD)
describe(base$EDAD)
mean(base$EDAD) #media 
sd(base$EDAD) # desviación estandar
median(base$EDAD) # mediana
mlv(base$EDAD, method = "mfv") # moda
quantile(base$EDAD,c(0.05, .95)) #percentiles 5 y 95

# Taller R 2
# ============================
# Prevalencia de sobrepeso/obesidad (IMC)
# con todas las variables nuevas
# Puntos 4,5,7

# ETNIA
table(base$imcnewNEW, base$ETNIA2)                                  # Frecuencias
prop.table(table(base$imcnewNEW, base$ETNIA2))                       # % de cada celda
prop.table(table(base$imcnewNEW, base$ETNIA2), 1)                    # % por fila
prop.table(table(base$imcnewNEW, base$ETNIA2), 2)                    # % por columna

# ESCOLARIDAD
table(base$imcnewNEW, base$ESCOLARIDADnew)
prop.table(table(base$imcnewNEW, base$ESCOLARIDADnew))
prop.table(table(base$imcnewNEW, base$ESCOLARIDADnew), 1)
prop.table(table(base$imcnewNEW, base$ESCOLARIDADnew), 2)

# OCUPACION
table(base$imcnewNEW, base$OCUPACIONnew)
prop.table(table(base$imcnewNEW, base$OCUPACIONnew))
prop.table(table(base$imcnewNEW, base$OCUPACIONnew), 1)
prop.table(table(base$imcnewNEW, base$OCUPACIONnew), 2)

# QH42 (hábitos de consumo)
table(base$imcnewNEW, base$QH42NEW)
prop.table(table(base$imcnewNEW, base$QH42NEW))
prop.table(table(base$imcnewNEW, base$QH42NEW), 1)
prop.table(table(base$imcnewNEW, base$QH42NEW), 2)

# SEXO
table(base$imcnewNEW, base$SEXONEW)
prop.table(table(base$imcnewNEW, base$SEXONEW))
prop.table(table(base$imcnewNEW, base$SEXONEW), 1)
prop.table(table(base$imcnewNEW, base$SEXONEW), 2)

# SISBEN
table(base$imcnewNEW, base$SISBENNEW)
prop.table(table(base$imcnewNEW, base$SISBENNEW))
prop.table(table(base$imcnewNEW, base$SISBENNEW), 1)
prop.table(table(base$imcnewNEW, base$SISBENNEW), 2)

# EDAD
table(base$imcnewNEW, base$EDADnew)
prop.table(table(base$imcnewNEW, base$EDADnew))
prop.table(table(base$imcnewNEW, base$EDADnew), 1)
prop.table(table(base$imcnewNEW, base$EDADnew), 2)

# Tiempo frente a pantalla (Tlmeett)
table(base$imcnewNEW, base$TlmeettNEW)
prop.table(table(base$imcnewNEW, base$TlmeettNEW))
prop.table(table(base$imcnewNEW, base$TlmeettNEW), 1)
prop.table(table(base$imcnewNEW, base$TlmeettNEW), 2)


# ============================
# PUNTO 7: TABLA CONSOLIDADA
# Variable | Categoria | Frecuencia | Porcentaje | Prevalencia_SO_pct
# ============================

# Asegura que el desenlace esté como factor binario con niveles "0","1"
if (!("imcnewNEW" %in% names(base))) {
  stop("No existe 'imcnewNEW' en 'base'. Verifica la recodificación del IMC.")
}
base$imcnewNEW <- as.factor(base$imcnewNEW)
if (!all(c("0","1") %in% levels(base$imcnewNEW))) {
  base$imcnewNEW <- factor(base$imcnewNEW, levels = c("0","1"))
}

# Variables nuevas a incluir en la tabla (ajusta si usas otros nombres)
vars_tabla7 <- c("EDADnew","SEXONEW","ETNIA2","SISBENNEW",
                 "ESCOLARIDADnew","OCUPACIONnew","QH42NEW","TlmeettNEW")

# Función para armar la fila por variable :D
arma_tabla7 <- function(var, outcome = "imcnewNEW") {
  if (!(var %in% names(base))) {
    warning(sprintf("La variable '%s' no existe en 'base'. Se omite.", var))
    return(data.frame(Variable = character(0), Categoria = character(0),
                      Frecuencia = integer(0), Porcentaje = numeric(0),
                      Prevalencia_SO_pct = numeric(0)))
  }
  x <- base[[var]]
  y <- base[[outcome]]

  # Excluir NA en variable y desenlace
  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) {
    warning(sprintf("La variable '%s' no tiene datos válidos junto con '%s'.", var, outcome))
    return(data.frame(Variable = character(0), Categoria = character(0),
                      Frecuencia = integer(0), Porcentaje = numeric(0),
                      Prevalencia_SO_pct = numeric(0)))
  }

  # Factores sin niveles vacíos
  x <- droplevels(as.factor(x[ok]))
  y <- droplevels(as.factor(y[ok]))

  # Conteos por categoría (guardar nombres antes de convertir a integer)
  tab_x <- table(x)
  cats  <- names(tab_x)
  n_cat <- as.integer(tab_x)
  pct_cat <- round(100 * (n_cat / sum(n_cat)), 2)

  # Cruce como matriz para evitar dropeos raros cuz yknows R
  tb <- as.matrix(table(x, y))

  # Numerador = casos "1" (si no existe la columna, usar 0s)
  col_pos <- if (!is.null(colnames(tb)) && "1" %in% colnames(tb)) "1" else NA
  numer <- if (!is.na(col_pos)) tb[, col_pos] else rep(0L, length(cats))
  denom <- rowSums(tb)
  prev_cat <- round(100 * ifelse(denom > 0, numer / denom, NA), 2)

  out <- data.frame(
    Variable = var,
    Categoria = cats,
    Frecuencia = n_cat,
    Porcentaje = pct_cat,
    Prevalencia_SO_pct = as.numeric(prev_cat),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# Building tablaaaaaaa
tabla7_list <- lapply(vars_tabla7, arma_tabla7)
tabla7_p7 <- do.call(rbind, tabla7_list)

# show output to be sure bro
cat("\n================ TABLA PUNTO 7 (con NA excluidos) ================\n")
print(tabla7_p7)

# CSV pa verlo lendo 
write.csv(tabla7_p7, file = "tabla_punto7.csv", row.names = FALSE)
cat("\n[OK] Exportado: tabla_punto7.csv\n")
