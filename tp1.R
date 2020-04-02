leerDatos <- function() {
  setwd("~/TP1PyE")
  datos <- read.table(file = "base1.txt", header = TRUE, sep = "\t", 
                      col.names = c("ID", "Altura", "Diametro", "Inclinacion", "Especie", "Origen", "Brotes"))
  attach(datos)
  fuente <- "Fuente: Censo Forestal Urbano Público, Ciudad de Buenos Aires, año 2011"
  tablasAltura(Altura, fuente)
  tablasEspecie(Especie, fuente)
}

tablasAltura <- function(Altura, fuente) {
  frecAbs <- table(cut(Altura, seq(0, 35, 5), right = "FALSE"))
  titulo <- "ALTURA DE LOS ÁRBOLES"
  barplot(frecAbs, ylim = c(0, 100), xlab = "Altura", ylab = "Frecuencia absoluta", font.lab = 2, col = "green", main = titulo, 
          sub = fuente, font.sub = 2)
  
  frecAbs <- table(Altura, Especie)
  titulo <- "ALTURA PROMEDIO DE LOS ÁRBOLES\nSEGÚN SU ESPECIE"
  barplot(frecAbs, beside = TRUE, main = titulo)
}

tablasEspecie <- function(Especie, fuente) {
  frecAbs <- table(Especie)
  barplot(frecAbs, ylim = c(0, 70), col = "green")
}
