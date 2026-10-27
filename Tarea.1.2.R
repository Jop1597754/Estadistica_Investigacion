# Jop Rubén Escobedo Cruz
# Matrícula
# Fecha de entrega- 09/09/2026

# EJERCICIO 3.2----

# Hipótesis:

# H0: μ = 80 kg — el peso neto medio de los costales es igual al contenido declarado.
# H1: μ < 80 kg — el peso neto medio de los costales es menor que el contenido declarado.


# Ingresar datos ----

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94,
            80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89,
            82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01,
            80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29,
            75.52, 77.21, 75.99, 81.94, 80.41, 77.7)

# Graficar ----

hist(costal,
     main = "",
     xlab = "Peso neto (kg)",
     ylab = "Frecuencia",
     ylim = c(0,12),
     col = "lightblue")

# Normalidad datos y estadisticos descriptivos ----

shapiro.test(costal)

mean(costal)
sd(costal)
var(costal)
fivenum(costal)

boxplot(costal,
        col = "red",
        ylab = "Peso neto (kg)")

# Prueba t de una sola muestra

t.test(costal, mu = 80, alternative = "less")






