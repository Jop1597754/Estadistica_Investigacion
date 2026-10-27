# Jop Rubén Escobedo Cruz
# Matrícula
# Fecha de entrega - 09/09/2026

# EJERCICIO 3.1 ----
# H0: mu = 16.5 ; H1: diferente a la media bace


# Importar datos----

# URL de los datos
url <- paste0(
  "https://raw.githubusercontent.com/mgtagle/",
  "Estadistica_Investigacion_2026/refs/heads/main/",
  "Datos/Crudos/cuadro1.csv")

# Importar los datos directamente desde GitHub
cuadro1 <- read.csv(url, header = TRUE)

# Revisar los datos
head(cuadro1)
str(cuadro1)

# Graficar ----

hist(cuadro1$Diametro,
     main = "",
     xlab = "Diámetro (cm)",
     ylab = "Frecuencia",
     ylim = c(0,15),
     col = "lightblue")

# Normalidad datos y estadisticos descriptivos ----

shapiro.test(cuadro1$Diametro)

mean(cuadro1$Diametro)
sd(cuadro1$Diametro)
var(cuadro1$Diametro)
fivenum(cuadro1$Diametro)

boxplot(cuadro1$Diametro,
        col = "red",
        ylab = "Diámetro (cm)")

# Prueba de una sola muestra

boxplot(cuadro1$Diametro)
t.test(cuadro1$Diametro, mu = 16.5, alternative = "two.sided")


library(rmarkdown)


