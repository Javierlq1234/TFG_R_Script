
install.packages("dplyr")
install.packages("e1071")
install.packages("ggplot2")
library(dplyr)
library(e1071)
library(ggplot2)

#-----------------------------------------
# SIMULAR DATOS
#-----------------------------------------

library(dplyr)
library(truncnorm)
set.seed(123)

# 1. SIMULAR CONOCIMIENTOS PREVIOS (0–10)
n <- 100
conoc_previos <- sample(0:10, n, replace = TRUE)

# 2. CLASIFICAR EN BLOQUES: bajo (0–3), medio (4–6), alto (7–10)
nivel_conocimiento <- cut(conoc_previos,
                          breaks = c(-1, 3, 6, 10),
                          labels = c("bajo", "medio", "alto"))

# 3. BLOQUEO
df <- data.frame(id = 1:n, conoc_previos, nivel_conocimiento) %>%
  group_by(nivel_conocimiento) %>%
  mutate(grupo = sample(rep(c(0,1), length.out = n()))) %>%
  ungroup()

# 4. SIMULAR EXPERIENCIA EN DEBATE (5–20)
df$experiencia_debate <- round(pmin(pmax(rnorm(n, mean = 12, sd = 3), 5), 20))

# 5. SIMULAR NASA-TLX 
df$demanda_mental <- ifelse(df$grupo == 0,
                            round(rtruncnorm(n, 0, 100, mean = 70, sd = 15)),
                            round(rtruncnorm(n, 0, 100, mean = 50, sd = 15)))

df$demanda_temporal <- ifelse(df$grupo == 0,
                              round(rtruncnorm(n, 0, 100, mean = 60, sd = 15)),
                              round(rtruncnorm(n, 0, 100, mean = 45, sd = 15)))

df$esfuerzo <- ifelse(df$grupo == 0,
                      round(rtruncnorm(n, 0, 100, mean = 65, sd = 15)),
                      round(rtruncnorm(n, 0, 100, mean = 40, sd = 15)))

df$frustracion <- ifelse(df$grupo == 0,
                         round(rtruncnorm(n, 0, 100, mean = 35, sd = 15)),
                         round(rtruncnorm(n, 0, 100, mean = 60, sd = 15)))

df$rendimiento <- ifelse(df$grupo == 0,
                         round(rtruncnorm(n, 0, 100, mean = 70, sd = 15)),
                         round(rtruncnorm(n, 0, 100, mean = 50, sd = 15)))

# 6. CALCULAR NASA-TLX TOTAL (media sin demanda física)
df$NASA_total <- rowMeans(df[, c("demanda_mental", "demanda_temporal", "esfuerzo", "frustracion", "rendimiento")])

# 7. SIMULAR RESULTADOS (calidad del texto y número de temas)
df$calidad_texto <- round(ifelse(df$grupo == 0,
                                 rtruncnorm(n, a=0, b=20, mean=15, sd=2.5),
                                 rtruncnorm(n, a=0, b=20, mean=12, sd=2.5)))

df$n_temas <- ifelse(df$grupo == 0,
                     round(rtruncnorm(n, a = 0, b = 6, mean = 4, sd = 1)),  # Google
                     round(rtruncnorm(n, a = 0, b = 6, mean = 2, sd = 1)))  # ChatGPT

# Resultado final
df_final <- df %>% select(id, grupo, conoc_previos, nivel_conocimiento, experiencia_debate,
                          demanda_mental, demanda_temporal, esfuerzo,
                          frustracion, rendimiento, NASA_total,
                          calidad_texto, n_temas)

head(df_final)

#-------------------------------------
#LIMPIAR DATOS
#-------------------------------------

df_limpio <- df_final %>%
  filter(experiencia_debate < 16)

#-------------------------------------------
#ESTADÍSTICOS DESCRIPTIVOS
#-------------------------------------------


# 9. ESTADÍSTICOS DESCRIPTIVOS POR GRUPO

variables <- c("NASA_total", "demanda_mental", "demanda_temporal", "esfuerzo",
               "frustracion", "rendimiento", "calidad_texto", "n_temas", "conoc_previos")

resumen <- df_limpio %>%
  group_by(grupo) %>%
  summarise(across(all_of(variables), list(
    media = ~mean(., na.rm = TRUE),
    mediana = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    C5_C95 = ~quantile(., 0.95, na.rm = TRUE) - quantile(., 0.05, na.rm = TRUE),
    asimetria = ~skewness(., na.rm = TRUE),
    curtosis = ~kurtosis(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

print(resumen)


# 10. DIAGRAMAS DE CAJA POR VARIABLE

for (var in variables) {
  print(
    ggplot(df_limpio, aes(x = factor(grupo, labels = c("Google", "ChatGPT")), y = .data[[var]])) +
      geom_boxplot() +
      labs(title = paste("Distribución de", var), x = "Grupo", y = var) +
      theme_minimal()
  )
}

#------------------------------
#ESTADÍSTICA INFERENCIAL
#------------------------------


library(effectsize)


variables_ttest <- c(
  "NASA_total",
  "demanda_mental",
  "demanda_temporal",
  "esfuerzo",
  "frustracion",
  "rendimiento",
  "calidad_texto",
  "n_temas"  
)


resultados_t <- list()


for (var in variables_ttest) {
  cat("\n=== Variable (t-test):", var, "===\n")
  
  t_test <- t.test(df_limpio[[var]] ~ df_limpio$grupo, var.equal = TRUE)
  print(t_test)
  
  d <- cohens_d(df_limpio[[var]] ~ df_limpio$grupo)
  print(d)
  
  resultados_t[[var]] <- list(
    t_test = t_test,
    cohen_d = d
  )
}

# Crear tabla resumen
tabla_resumen <- data.frame(
  Variable = character(),
  Prueba = character(),
  Estadistico = character(),
  p = numeric(),
  Efecto = numeric(),
  CI95_inf = numeric(),
  CI95_sup = numeric(),
  stringsAsFactors = FALSE
)

# Añadir resultados t-test a la tabla
for (var in variables_ttest) {
  t_valor <- resultados_t[[var]]$t_test$statistic
  p_valor <- resultados_t[[var]]$t_test$p.value
  d_info <- resultados_t[[var]]$cohen_d
  
  d_valor <- d_info$Cohens_d
  ci_inf <- d_info$CI_low
  ci_sup <- d_info$CI_high
  
  tabla_resumen <- rbind(tabla_resumen, data.frame(
    Variable = var,
    Prueba = "t-test",
    Estadistico = paste0("t = ", round(t_valor, 2)),
    p = round(p_valor, 4),
    Efecto = round(d_valor, 2),
    CI95_inf = round(ci_inf, 2),
    CI95_sup = round(ci_sup, 2)
  ))
}

# Mostrar tabla final
print(tabla_resumen)

#----------------------------------------------------------

# Seleccionar las variables de interés
subescalas_nasa <- c("demanda_mental", "demanda_temporal", "esfuerzo", "frustracion", "rendimiento")
resultados <- c("calidad_texto", "n_temas")

# Crear tabla vacía
correlaciones <- data.frame(
  Subescala = character(),
  Variable_resultado = character(),
  Correlacion = numeric(),
  p_valor = numeric(),
  stringsAsFactors = FALSE
)

# Bucle para calcular correlaciones
for (s in subescalas_nasa) {
  for (r in resultados) {
    test <- cor.test(df_limpio[[s]], df_limpio[[r]], method = "pearson")
    correlaciones <- rbind(correlaciones, data.frame(
      Subescala = s,
      Variable_resultado = r,
      Correlacion = round(test$estimate, 3),
      p_valor = round(test$p.value, 4)
    ))
  }
}

# Mostrar tabla de correlaciones
print(correlaciones)


# Ejecutar las dos regresiones
modelo_calidad <- lm(calidad_texto ~ esfuerzo, data = df_limpio)
modelo_temas <- lm(n_temas ~ esfuerzo, data = df_limpio)

# Extraer resultados
tabla_regresion <- data.frame(
  Variable_dependiente = c("calidad_texto", "n_temas"),
  Beta = c(
    round(coef(modelo_calidad)[2], 3),
    round(coef(modelo_temas)[2], 3)
  ),
  R2 = c(
    round(summary(modelo_calidad)$r.squared, 3),
    round(summary(modelo_temas)$r.squared, 3)
  ),
  p_valor = c(
    round(summary(modelo_calidad)$coefficients[2, 4], 4),
    round(summary(modelo_temas)$coefficients[2, 4], 4)
  ),
  stringsAsFactors = FALSE
)

# Mostrar tabla
print(tabla_regresion)


# Instalar y cargar paquete necesario
install.packages("psych")  # Solo si no lo tienes
library(psych)

# Seleccionar subescalas del NASA-TLX
nasa_items <- df_limpio[, c("demanda_mental", "demanda_temporal", "esfuerzo", "frustracion", "rendimiento")]
cor_nasa <- cor(nasa_items, method = "pearson")
print(round(cor_nasa, 3))

# Calcular alfa de Cronbach
alpha_nasa <- psych::alpha(nasa_items)

# Mostrar resultado principal
print(alpha_nasa$total$raw_alpha)


# Subescalas sin frustración
nasa_sin_frustracion <- df_limpio[, c("demanda_mental", "demanda_temporal", "esfuerzo", "rendimiento")]

# Calcular alfa de Cronbach sin frustración
alpha_sin_frustracion <- psych::alpha(nasa_sin_frustracion)

# Mostrar el alfa
cat("Alfa de Cronbach sin frustración:", round(alpha_sin_frustracion$total$raw_alpha, 3), "\n")















