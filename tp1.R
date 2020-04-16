# Análisis univariado:

# Primer variable a analizar: ALTURA

tablasAltura <- function(datos) {
  # Tabla de frecuencias
  frecAbs <- table(cut(datos$Altura, seq(0, 35, 5), right = "TRUE"))
  frecRel <- round(frecAbs/length(datos$Altura), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(datos$Altura)), digits = 4)
  tabla_altura <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  # Histograma
  tit <- "ALTURA DE LOS ÁRBOLES"
  b <- seq(0, 35, 5)
  hist(datos$Altura, main = tit, xlab = "Altura (en metros)",
       ylab = "Frecuencia absoluta", xlim = c(0, 35), ylim = c(0, 100),
       col = "green", breaks = b, xaxt = "n", yaxt = "n", right = TRUE,
       font.lab = 2)
  axis(side = 1, at = b)
  axis(side = 2, at = seq(0, 100, 10))
  
  # Polígono acumulativo
  plot(c(0, frecRelAcum), type = "l", main = tit, xlab = "Altura (en metros)", 
       ylab = "Frecuencia relativa acumulada", xaxt = "n", font.lab = 2,
       xlim = c(1, 8))
  axis(side = 1, at = (1:8), labels = seq(0, 35, 5))
  abline(h = seq(0, 1, 0.1), lty = 3)
}

# Segunda variable a analizar: DIÁMETRO

tablasDiametro <- function(datos){
  
  # Tabla de frecuencias
  frecAbs <- table(cut(datos$Diametro, seq(0, 140, 20), right = "TRUE"))
  frecRel <- round(frecAbs/length(datos$Diametro), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(datos$Diametro)), digits = 4)
  tabla_diametro <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  # Histograma
  tit <- "DIÁMETRO DE LOS ÁRBOLES"
  b <- seq(0, 140, 20)
  hist(datos$Diametro, main = tit, xlab = "Diámetro (en centímetros)",
       ylab = "Frecuencia absoluta", xlim = c(0, 140), ylim = c(0, 150),
       col = "lightblue", breaks = b, xaxt = "n", yaxt = "n", right = TRUE,
       font.lab = 2)
  axis(side = 1, at = b)
  axis(side = 2, at = seq(0, 150, 10))
  
  # Polígono acumulativo.
  plot(c(0, frecRelAcum), type = "l", main = tit,
       xlab = "Diámetro (en centímetros)",
       ylab = "Frecuencia relativa acumulada", xaxt = "n", font.lab = 2)
  axis(side = 1, at = (1:8), labels = seq(0, 140, 20))
  abline(h = seq(0, 1, 0.1), lty = 3)
}


# Tercer variable a analizar: Inclinación
tablasInclinacion <- function(datos) {
  frecAbs <- table(cut(datos$Inclinacion, seq(0, 42, 7), right = "FALSE"))
  frecRel <- round(frecAbs/length(datos$Inclinacion), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(datos$Inclinacion)), digits = 4)
  tabla_inclinacion <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  # Boxplot
  tit <- "INCLINACIÓN DE LOS ÁRBOLES"
  boxplot(datos$Inclinacion, main = tit, col = "orange", ylim = c(0, 45))
  title(ylab = "Inclinación (en grados)", font.lab = 2)
}

# Cuarta variable a analizar: Especie

tablasEspecie <- function(datos) {
   frecAbs <- table(datos$Especie)
   
   tit <- "ÁRBOLES SEGÚN SU ESPECIE"
   # Grafico de barras.
   barplot(frecAbs, ylim = c(0, 70), col = "green", main = tit,
           xlab = "Especie", ylab = "Frecuencia absoluta", font.lab = 2)
}

# Quinta variable a analizar: Origen.

tablasOrigen <- function(datos) {
  frecAbs <- table(datos$Origen)
  porcentajes <- round((frecAbs / length(datos$Origen)) * 100, digits = 2)
  tabla_origen <<- cbind(frecAbs, porcentajes)
  
  lbls <- c("Exótico", "Nativo/Autóctono")
  lbls <- paste(lbls, porcentajes)
  lbls <- paste(lbls, "%")
  
  tit <- "ÁRBOLES SEGÚN SU ORIGEN"
  # Pie chart
  pie(frecAbs, labels = lbls, main = tit, col = c("purple", "cyan"), font = 2)
}

# Sexta variable a analizar: N° Brotes

tablasBrotes <- function(datos) {
  frecAbs <- table(datos$Brotes)
  frecRel <- round(frecAbs/length(datos$Brotes), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(datos$Brotes)), digits = 4)
  tabla_brotes <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  tit <- "BROTES CRECIDOS POR ÁRBOL\nEN EL ÚLTIMO AÑO"
  # Gráfico de bastones
  plot(frecAbs, type = "h", main = tit, xlab = "Cantidad de brotes", 
       ylab = "Frecuencia absoluta", font.lab = 2, ylim = c(0, 105))
  # Gráfico escalonado
  plot(frecRelAcum, type = "s", main = tit, xlab = "Cantidad de brotes", 
       ylab = "Frecuencia relativa acumulada", font.lab = 2, 
       xaxt = "n", ylim = c(0, 1))
  axis(side = 1, at = (1:9), labels = seq(0, 8, 1))
  abline(h = seq(0, 1, 0.1), lty = 3)
  
}

#----------------------------------------------------------------------------#

# Análisis bivariado:

tablasAlturaEspecie <- function(datos){
  grupo <- group_by(datos, Especie)
  tabla_alturaEspecie <<- summarise(grupo, min = min(Altura), max = max(Altura),
                                    mean = round(mean(Altura), digits = 4),
                                    median = median(Altura),
                                    fstQuan = quantile(Altura, probs = 0.25),
                                    trdQuan = quantile(Altura, probs = 0.75))
  
  tit <- "ALTURA DE LOS ÁRBOLES SEGÚN SU ESPECIE"
  boxplot(datos$Altura~datos$Especie, main = tit, col = "orange", ylim = c(0, 40),
           xlab = "", ylab = "")  
  title(xlab = "Especie", ylab = "Altura (en metros)", font.lab = 2)
}

tablasDiametroEspecie <- function(datos){
  grupo <- group_by(datos, Especie)
  tabla_diametroEspecie <<- summarise(grupo, min = min(Diametro), max = max(Diametro),
                                      mean = round(mean(Diametro), digits = 4),
                                      median = median(Diametro),
                                      fstQuan = quantile(Diametro, probs = 0.25),
                                      trdQuan = quantile(Diametro, probs = 0.75))
  
  tit <- "DIÁMETRO DE LOS ÁRBOLES SEGÚN SU ESPECIE"
  boxplot(datos$Diametro~datos$Especie, main = tit, col = "orange", ylim = c(0,140),
          xlab = "", ylab = "")  
  title(xlab = "Especie", ylab = "Diámetro (en centímetros)", font.lab = 2)
}

tablasInclinacionEspecie <- function(datos){
  grupo <- group_by(datos, Especie)
  tabla_inclinacionEspecie <<- summarise(grupo, min = min(Inclinacion), max = max(Inclinacion),
                                      mean = round(mean(Inclinacion), digits = 4),
                                      median = median(Inclinacion),
                                      fstQuan = quantile(Inclinacion, probs = 0.25),
                                      trdQuan = quantile(Inclinacion, probs = 0.75))
  
  tit <- "INCLINACIÓN DE LOS ÁRBOLES SEGÚN SU ESPECIE"
  boxplot(datos$Inclinacion~datos$Especie, main = tit, col = "orange", ylim = c(0,45),
          xlab = "", ylab = "")  
  title(xlab = "Especie", ylab = "Inclinación (en grados)", font.lab = 2)
}

tablasOrigenEspecie <- function(datos) {
  frecAbs <- table(datos$Origen, datos$Especie)
  tabla_origenEspecie <<- cbind(table(datos$Especie, datos$Origen), table(datos$Especie))
  
  tit <- "ORIGEN DE LOS ÁRBOLES\nSEGÚN SU ESPECIE"
  barplot(frecAbs, beside = TRUE, ylim = c(0, 70), col = c("purple", "cyan"),
          main = tit, xlab = "Especie", ylab = "Frecuencia absoluta",
          font.lab = 2)
  legend("topright", legend = c("Exótico", "Nativo/Autóctono"),
        fill = c("purple", "cyan"))
}

tablasBrotesEspecie <- function(datos) {
  frecAbs <- table(datos$Brotes, datos$Especie)
  totalPorEspecie <- c(1:9)
  for (i in seq(1:length(levels(datos$Especie)))) {
    totalPorEspecie[i] = sum(table(datos$Brotes, datos$Especie)[, i] * c(0:8))
  }
  tabla_brotesEspecie <<- cbind(levels(datos$Especie), totalPorEspecie)
  
  tit <- "CANTIDAD DE BROTES POR ESPECIE\nEN EL ÚLTIMO AÑO"
  barplot(totalPorEspecie, names.arg = levels(datos$Especie), col = "purple",
          ylim = c(0, 250), main = tit, xlab = "Especie",
          ylab = "Frecuencia absoluta", font.lab = 2)
}

#-----------------------------------------------------------------------------

leerDatos <- function() {
  #setwd("~/TP1PyE")
  datos <<- read.table(file = "base1.txt", header = TRUE, sep = "\t", 
                      col.names = c("ID", "Altura", "Diametro",
                      "Inclinacion", "Especie", "Origen", "Brotes"))
  tablasAltura(datos)
  tablasDiametro(datos)
  tablasInclinacion(datos)
  tablasEspecie(datos)
  tablasOrigen(datos)
  tablasBrotes(datos)
  tablasAlturaEspecie(datos)
  tablasDiametroEspecie(datos)
  tablasInclinacionEspecie(datos)
  tablasOrigenEspecie(datos)
  tablasBrotesEspecie(datos)
}

library(dplyr)
leerDatos()

