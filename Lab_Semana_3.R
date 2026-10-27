# Jop Rubén Escobedo Cruz
# Matrícula 1597754
# Fecha de entrega ---- 17/09/2026

# Objetivos de la práctica
# 1--- Inicair una sesión en Rstudio
# 2--- Realziar una descripción estadística de un conjunto de datos.
# 3--- Comprender el diseño del panel de RStudio.
# 4--- Conozca la sintaxis markdown.

# Parte 1: Importar datos ----

# Importar desde archivos csv ----
# Función read.csv importa datos de excel a R

getwd()

trees <- read.csv("DBH_1.csv", header=TRUE)
head(trees)

# Ingresar datos directo en la consola ----

dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3,
         9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)

# Datos de URL no seguras (http) ----

url <- "https://www.agricultura.gob.mx/sites/default/files/sagarpa/document/2025/07/11/2245/14072025-actividades-inspeccion-movilizacion.csv"
inspeccion <- read.csv(url)
head(inspeccion)

prof_url_2 <- paste0("https://www.agricultura.gob.mx/sites/default/files/sagarpa/",
                     "document/2025/07/11/2245/",
                     "14072025-actividades-inspeccion-movilizacion.csv")

senasica <- read.csv(prof_url_2)
head(senasica)

# Datos de URL seguras (https): Dropbox ----

library(repmis) # No olvidar cargar la paquetería

conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
head(conjunto) # muestra las primeras seis filas de la BD

# Datos de URL seguras (https): Github ----

library(readr)

file <- paste0("https://raw.githubusercontent.com/mgtagle/",
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")

inventario <- read_csv(file)
head(inventario)

# Parte 2: Operaciones con la base de datos ----

mean(trees$dbh) # El signo de $ informa que necesitamos la columna dbh
sd(trees$dbh)

# Selección mediante restricciones ----

# Indica la sumatoria de los individuos en el objeto tree con un dbh < a 10
sum(trees$dbh < 10)

which(trees$dbh < 10)

# Excluir los diámetros de la parcela 2. El símbolo ! indica NO
trees.13 <- trees[!(trees$parcela=="2"),]
trees.13

# Selección de una submuestra ----

trees.1 <- subset(trees, dbh <= 10)
head(trees.1)

mean(trees$dbh)
mean(trees.1$dbh)

# Histogramas de la muestra y la submuestra (Figura 5) ----
# Argumentos de hist y etiquetas vistos en clase (Revisar_conjunto_datos.R)

hist(trees$dbh,
     main = "Muestra original trees",
     xlab = "DBH",
     ylab = "Frecuencia")

hist(trees.1$dbh,
     main = "dbh < 10 cm. trees.1",
     xlab = "DBH",
     ylab = "Frecuencia")

# Parte 3: Representación gráfica ----

# Histogramas ----

mamiferos <- read.csv("https://www.openintro.org/data/csv/mammals.csv")

hist(mamiferos$total_sleep)

hist(mamiferos$total_sleep, # Datos
     xlim = c(0,20), ylim = c(0,14), # Cambiar los límites de x & y
     main = "Total de horas sueño de las 39 especies", # Cambiar el título
     xlab = "Horas sueño", # Cambiar eje de las x
     ylab = "Frecuencia", # Cambiar eje de las y
     las = 1, # Cambiar orientación de y
     col = "navajowhite") # Cambiar color de las barras

# Barplot o gráfico de barras ----

data("chickwts")
head(chickwts[c(1:2,42:43, 62:64), ])

feeds <- table(chickwts$feed) # Tabla de frecuencias por tipo de alimento
feeds

barplot(feeds)
barplot(feeds[order(feeds, decreasing = TRUE)])


# Figura 10: barplot horizontal ----
# Ejercicio del laboratorio: "Ahora te toca a tí"

barplot(feeds[order(feeds)], # Orden creciente: la mayor queda arriba
        horiz = TRUE, # Barras horizontales
        las = 1, # Etiquetas horizontales
        main = "Frecuencias por tipos de\n alimentación", # Título en dos líneas
        xlab = "Número de Pollos", # Eje de las x
        col = c("#FFCC00", "#FFD11A", "#FFD633", "#FFDB4D", "#FFE066", "#FFE680")) # Gradiente amarillo


