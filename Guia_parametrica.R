# Jop Rubén Escobedo Cruz
# Matrícula 1597754
# 27/10/2026

# Caso de estudio y datos reproducibles ----

datos <- data.frame(
  Parcela = sprintf("P%02d", 1:30),
  Tratamiento = factor(
    rep(c("Testigo", "Ligero", "Moderado"), each = 10),
    levels = c("Testigo", "Ligero", "Moderado")
  ),
  Area_basal_inicial = c(
    24, 29, 22, 33, 27, 31, 25, 35, 28, 30,
    25, 30, 23, 32, 28, 34, 26, 35, 29, 31,
    23, 28, 24, 34, 27, 32, 26, 35, 29, 31
  ),
  Incremento = c(
    1.05, 1.28, 1.11, 1.38, 1.13, 1.42, 1.24, 1.39, 1.17, 1.33,
    1.32, 1.61, 1.38, 1.57, 1.47, 1.76, 1.49, 1.68, 1.53, 1.59,
    1.59, 1.82, 1.62, 1.96, 1.72, 1.91, 1.74, 2.03, 1.79, 1.92
  )
)

datos # Datos simulados del caso de estudio

# Revisar ----
# Revisar solo una porción de datos
head(datos) # Primeras filas de la BD datos
tail(datos) # Últimas filas de la BD datos
str(datos)  # Estructura y tipo de cada variable

# Estadística descriptiva ----
# Función tapply aplica cada estadístico por tratamiento
tabla <- data.frame(
  n = tapply(datos$Incremento, datos$Tratamiento, length),
  Media = tapply(datos$Incremento, datos$Tratamiento, mean),
  Mediana = tapply(datos$Incremento, datos$Tratamiento, median),
  DE = tapply(datos$Incremento, datos$Tratamiento, sd),
  Varianza = tapply(datos$Incremento, datos$Tratamiento, var),
  Minimo = tapply(datos$Incremento, datos$Tratamiento, min),
  Maximo = tapply(datos$Incremento, datos$Tratamiento, max)
)
tabla$CV_pct <- 100 * tabla$DE / tabla$Media # CV (%)
round(tabla, 3) # Estadísticos descriptivos del incremento por tratamiento

# Comparación visual de tratamientos ----
# Guardar la configuración gráfica
op <- par(no.readonly = TRUE)

# Márgenes: inferior, izquierdo, superior y derecho
par(mar = c(5, 7, 3, 2))

boxplot(
  Incremento ~ Tratamiento,
  data = datos,
  col = c("#D9D9D9", "#A6CEE3", "#B2DF8A"),
  xlab = "Tratamiento",
  ylab = expression(
    "Incremento de área basal (" *
      plain(m)^2 ~ plain(ha)^{-1} ~ "año"^{-1} * ")"
  ),
  outline = FALSE
)
set.seed(123)

stripchart(
  Incremento ~ Tratamiento,
  data = datos,
  vertical = TRUE,
  method = "jitter",
  jitter = 0.10,
  pch = 16,
  col = "#333333",
  add = TRUE
)

# Restaurar la configuración
par(op)

# Correlación de Pearson ----
# Subconjuntos para los análisis
testigo <- subset(datos, Tratamiento == "Testigo")
dos_grupos <- droplevels(
  subset(datos, Tratamiento %in% c("Testigo", "Moderado"))
)
table(testigo$Tratamiento)
table(dos_grupos$Tratamiento)

# Hipótesis:
# H0: rho = 0 - No existe una correlación lineal estadísticamente significativa
#     entre el área basal inicial y el incremento en las parcelas testigo.
# H1: rho != 0 - Existe una correlación lineal estadísticamente significativa
#     entre el área basal inicial y el incremento en las parcelas testigo.

resultado_cor <- cor.test(
  testigo$Area_basal_inicial, testigo$Incremento,
  method = "pearson", alternative = "two.sided"
)
resultado_cor

# Regresión lineal simple ----
# Hipótesis:
# H0: beta1 = 0 - No existe una relación lineal estadísticamente significativa
#     entre el área basal inicial y el incremento en las parcelas testigo.
# H1: beta1 != 0 - Existe una relación lineal estadísticamente significativa
#     entre el área basal inicial y el incremento en las parcelas testigo.

modelo_reg <- lm(Incremento ~ Area_basal_inicial, data = testigo)
summary(modelo_reg)

# Intervalos de confianza de los coeficientes
confint(modelo_reg)

# Diagrama de dispersión con recta de regresión
plot(
  testigo$Area_basal_inicial, testigo$Incremento,
  pch = 16, col = "#2166AC",
  xlab = "Área basal inicial (m² ha⁻¹)",
  ylab = "Incremento de área basal (m² ha⁻¹ año⁻¹)"
)
abline(modelo_reg, col = "#B2182B", lwd = 2)

# Estimación dentro del intervalo observado
predict(
  modelo_reg,
  newdata = data.frame(Area_basal_inicial = 30),
  interval = "confidence", level = 0.95
)

# Comparación de dos muestras independientes ----
# Hipótesis:
# H0: muT = muM - No existe una diferencia estadísticamente significativa en el
#     incremento medio entre el testigo y el aclareo moderado.
# H1: muT != muM - Existe una diferencia estadísticamente significativa en el
#     incremento medio entre el testigo y el aclareo moderado.

resultado_t <- t.test(
  Incremento ~ Tratamiento, data = dos_grupos,
  alternative = "two.sided", var.equal = TRUE,
  conf.level = 0.95
)
resultado_t

# Comparación de tres muestras independientes ----
# Hipótesis:
# H0: muT = muL = muM - No existe una diferencia estadísticamente significativa
#     en el incremento medio entre tratamientos.
# H1: No todas las medias son iguales - Existe una diferencia estadísticamente
#     significativa en el incremento medio de al menos un tratamiento.

modelo_anova <- aov(Incremento ~ Tratamiento, data = datos)
summary(modelo_anova)

# Evaluación de supuestos ----
# Diagnóstico de la regresión
par(mfrow = c(1, 2))
plot(modelo_reg, which = 1)
plot(modelo_reg, which = 2)
par(mfrow = c(1, 1))

# Diagnóstico del ANOVA
par(mfrow = c(1, 2))
plot(modelo_anova, which = 1)
plot(modelo_anova, which = 2)
par(mfrow = c(1, 1))

# Preguntas de autocomprobación ----
# P1. Asociación lineal sin ecuación predictiva: correlación de Pearson
#     y diagrama de dispersión (H0: rho = 0).
# P2. r = -0.70, p = 0.01: asociación lineal negativa significativa.
#     No demuestra causalidad ni significa una disminución del 70 %.
# P3. Pendiente: +0.028 m2 ha-1 año-1 por cada unidad de área basal inicial.
0.46 + 0.028 * 30 # Estimación para 30 m2 ha-1
#     R2 = 0.75 no garantiza por sí solo un ajuste adecuado.
# P4. p = 0.18: no se rechaza H0. No demuestra igualdad de medias;
#     revisar la diferencia estimada y su intervalo de confianza.
# P5. ANOVA con p = 0.004: al menos una media difiere. No implica que
#     los tres tratamientos difieran entre sí.