# Primer variable a analizar: ALTURA

tablasAltura <- function(Altura, fuente) {
  # Tabla de frecuencias
  frecAbs <- table(cut(Altura, seq(0, 35, 5), right = "TRUE"))
  frecRel <- round(frecAbs/length(Altura), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(Altura)), digits = 4)
  tabla_altura <<- cbind(frecAbs, frecRel, frecAbs, frecAbsAcum, frecRelAcum)
  
  # Histograma (FALTA MEJORAR TITULOS ETC)
  tit <- "ALTURA DE LOS ÁRBOLES EN EL SUR DE BUENOS AIRES"
  b <- seq(0, 35, 5)
  hist(Altura, main = tit, xlab = "Altura (en metros)", ylab = "Frecuencia absoluta", 
       xlim = c(0, 35), ylim = c(0, 100), sub = fuente, col = "green", 
       breaks = b, xaxt = "n", yaxt = "n", right = TRUE, font.lab = 2)
  axis(side = 1, at = b)
  axis(side = 2, at = seq(0, 100, 10))
  
  # Polígono acumulativo (FALTA QUE APAREZCA LE NRO 35 EN EL EJE X)
  plot(frecRelAcum, type = "l", main = tit, sub = fuente, xlab = "Altura (en metros)", ylab = "Frecuencia relativa acumulada", xaxt = "n", font.lab = 2)
  axis(side = 1, at = (1:8), labels = seq(0, 35, 5))
  abline(h = seq(0, 1, 0.2), lty = 3)
}

# Segunda variable a analizar: DIÁMETRO

tablasDiametro <- function(Diametro, fuente){
  
  # Tabla de frecuencias
  frecAbs <- table(cut(Diametro, seq(0, 140, 20), right = "TRUE"))
  frecRel <- round(frecAbs/length(Diametro), digits = 4)
  frecAbsAcum = cumsum(frecAbs)
  frecRelAcum = round(cumsum(frecAbs/length(Diametro)), digits = 4)
  tabla_diametro <<- cbind(frecAbs, frecRel, frecAbs, frecAbsAcum, frecRelAcum)
  
  # Histograma (FALTA MEJORAR TITULOS ETC)
  tit <- "DIÁMETRO DE LOS ÁRBOLES"
  b <- seq(0, 140, 20)
  hist(Diametro, main = tit, xlab = "Diámetro (en cm)", ylab = "Frecuencia absoluta", 
       xlim = c(0, 140), ylim = c(0, 150), sub = fuente, col = "lightblue", 
       breaks = b, xaxt = "n", yaxt = "n", right = TRUE, font.lab = 2)
  axis(side = 1, at = b)
  axis(side = 2, at = seq(0, 150, 10))
  
  # Polígono acumulativo.
  plot(frecRelAcum, type = "l", main = tit, sub = fuente, xlab = "Diámetro (en cm)", ylab = "Frecuencia relativa acumulada", xaxt = "n", font.lab = 2)
  axis(side = 1, at = (1:8), labels = seq(0, 140, 20))
  abline(h = seq(0, 1, 0.2), lty = 3)
}

# tablasEspecie <- function(Especie, fuente) {
#   frecAbs <- table(Especie)
#   barplot(frecAbs, ylim = c(0, 70), col = "green")
# }

# Variable a analizar: 

leerDatos <- function() {
  #setwd("~/TP1PyE")
  datos <- read.table(file = "base1.txt", header = TRUE, sep = "\t", 
                      col.names = c("ID", "Altura", "Diametro", "Inclinacion", "Especie", "Origen", "Brotes"))
  attach(datos)
  fuente <- "Fuente: Censo Forestal Urbano Público, Ciudad de Buenos Aires, año 2011"
  tablasAltura(Altura, fuente)
  tablasDiametro(Diametro, fuente)
  # tablasEspecie(Especie, fuente)
}

leerDatos()