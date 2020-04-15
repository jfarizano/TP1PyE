# Primer variable a analizar: ALTURA

tablasAltura <- function(Altura) {
  # Tabla de frecuencias
  frecAbs <- table(cut(Altura, seq(0, 35, 5), right = "TRUE"))
  frecRel <- round(frecAbs/length(Altura), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(Altura)), digits = 4)
  tabla_altura <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  # Histograma
  tit <- "ALTURA DE LOS ÁRBOLES"
  b <- seq(0, 35, 5)
  hist(Altura, main = tit, xlab = "Altura (en metros)", ylab = "Frecuencia absoluta", 
       xlim = c(0, 35), ylim = c(0, 100), col = "green", 
       breaks = b, xaxt = "n", yaxt = "n", right = TRUE, font.lab = 2)
  axis(side = 1, at = b)
  axis(side = 2, at = seq(0, 100, 10))
  
  # Polígono acumulativo
  plot(c(0, frecRelAcum), type = "l", main = tit, xlab = "Altura (en metros)", 
       ylab = "Frecuencia relativa acumulada", xaxt = "n", font.lab = 2, xlim = c(1, 8))
  axis(side = 1, at = (1:8), labels = seq(0, 35, 5))
  abline(h = seq(0, 1, 0.1), lty = 3)
}

# Segunda variable a analizar: DIÁMETRO

tablasDiametro <- function(Diametro){
  
  # Tabla de frecuencias
  frecAbs <- table(cut(Diametro, seq(0, 140, 20), right = "TRUE"))
  frecRel <- round(frecAbs/length(Diametro), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(Diametro)), digits = 4)
  tabla_diametro <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  # Histograma
  tit <- "DIÁMETRO DE LOS ÁRBOLES"
  b <- seq(0, 140, 20)
  hist(Diametro, main = tit, xlab = "Diámetro (en cm)", ylab = "Frecuencia absoluta", 
       xlim = c(0, 140), ylim = c(0, 150), col = "lightblue", 
       breaks = b, xaxt = "n", yaxt = "n", right = TRUE, font.lab = 2)
  axis(side = 1, at = b)
  axis(side = 2, at = seq(0, 150, 10))
  
  # Polígono acumulativo.
  plot(c(0, frecRelAcum), type = "l", main = tit, xlab = "Diámetro (en cm)", 
       ylab = "Frecuencia relativa acumulada", xaxt = "n", font.lab = 2)
  axis(side = 1, at = (1:8), labels = seq(0, 140, 20))
  abline(h = seq(0, 1, 0.1), lty = 3)
}

tablasInclinacion <- function(Inclinacion) {
  frecAbs <- table(cut(Inclinacion, seq(0, 42, 7), right = "FALSE"))
  frecRel <- round(frecAbs/length(Inclinacion), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(Inclinacion)), digits = 4)
  tabla_inclinacion <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  tit <- "INCLINACIÓN DE LOS ÁRBOLES"
  boxplot(Inclinacion, main = tit, col = "orange", ylab = "Inclinación (en grados)", font = 2, ylim = c(0, 45))
}

tablasEspecie <- function(Especie) {
   frecAbs <- table(Especie)
   
   tit <- "ÁRBOLES SEGÚN SU ESPECIE"
   barplot(frecAbs, ylim = c(0, 70), col = "green", main = tit, xlab = "Especie", 
           ylab = "Frecuencia absoluta", font.lab = 2)
}

tablasOrigen <- function(Origen) {
  frecAbs <- table(Origen)
  porcentajes <- round((frecAbs / length(Origen)) * 100, digits = 2)
  tabla_origen <<- cbind(frecAbs, porcentajes)
  
  lbls <- c("Exótico", "Nativo/Autóctono")
  lbls <- paste(lbls, porcentajes)
  lbls <- paste(lbls, "%")
  
  tit <- "ÁRBOLES SEGÚN SU ORIGEN"
  pie(frecAbs, labels = lbls, main = tit, col = c("purple", "cyan"))
}

tablasBrotes <- function(Brotes) {
  frecAbs <- table(Brotes)
  frecRel <- round(frecAbs/length(Brotes), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(Brotes)), digits = 4)
  tabla_brotes <<- cbind(frecAbs, frecAbsAcum, frecRel, frecRelAcum)
  
  tit <- "BROTES CRECIDOS POR ÁRBOL\nEN EL ÚLTIMO AÑO"
  plot(frecAbs, type = "h", main = tit, xlab = "Cantidad de brotes", 
       ylab = "Frecuencia absoluta", font.lab = 2, ylim = c(0, 105))
  
  plot(frecRelAcum, type = "s", main = tit, xlab = "Cantidad de brotes", 
       ylab = "Frecuencia relativa acumulada", font.lab = 2, 
       xaxt = "n", ylim = c(0, 1))
  axis(side = 1, at = (1:9), labels = seq(0, 8, 1))
  abline(h = seq(0, 1, 0.1), lty = 3)
  
}

tablasEspecieOrigen <- function(Especie, Origen) {
  frecAbs <- table(Origen, Especie)
  tabla_especieOrigen <<- cbind(table(Especie, Origen), table(Especie))
  
  tit <- "ORIGEN DE LOS ÁRBOLES\nSEGÚN SU ESPECIE"
  barplot(frecAbs, beside = TRUE, ylim = c(0, 70), col = c("purple", "cyan"), main = tit, xlab = "Especie", 
          ylab = "Frecuencia absoluta", font.lab = 2)
  legend("topright", legend = c("Exótico", "Nativo/Autóctono"), fill = c("purple", "cyan"))
}

tablasEspecieBrotes <- function(Especie, Brotes) {
  frecAbs <- table(Brotes, Especie)
  totalPorEspecie <- c(1:9)
  for (i in seq(1:length(levels(Especie)))) {
    totalPorEspecie[i] = sum(table(Brotes, Especie)[, i] * c(0:8))
  }
  tabla_especieBrotes <<- cbind(levels(Especie), totalPorEspecie)
  
  tit <- "CANTIDAD DE BROTES POR ESPECIE\nEN EL ÚLTIMO AÑO"
  barplot(totalPorEspecie, names.arg = levels(Especie), col = "purple", ylim = c(0, 250), main = tit, 
          xlab = "Especie", ylab = "Frecuencia absoluta", font.lab = 2)
}

leerDatos <- function() {
  #setwd("~/TP1PyE")
  datos <- read.table(file = "base1.txt", header = TRUE, sep = "\t", 
                      col.names = c("ID", "Altura", "Diametro", "Inclinacion", "Especie", "Origen", "Brotes"))
  attach(datos)
  fuente <- "Fuente: Censo Forestal Urbano Público, Ciudad de Buenos Aires, año 2011"
  tablasAltura(Altura)
  tablasDiametro(Diametro)
  tablasInclinacion(Inclinacion)
  tablasEspecie(Especie)
  tablasOrigen(Origen)
  tablasBrotes(Brotes)
  tablasEspecieOrigen(Especie, Origen)
  tablasEspecieBrotes(Especie, Brotes)
}

leerDatos()