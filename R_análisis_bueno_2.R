install.packages("dplyr")
install.packages("e1071")
install.packages("ggplot2")
install.packages("truncnorm")
install.packages("psych")
install.packages("effectsize")

library(dplyr)
library(e1071)
library(ggplot2)
library(truncnorm)
library(psych)
library(effectsize)

#-------------------------------------
# SIMULAR DATOS
#-------------------------------------
set.seed(123)
n <- 126  # 63 por grupo

conoc_previos <- sample(0:10, n, replace = TRUE)
nivel_conocimiento <- cut(conoc_previos,
                          breaks = c(-1, 3, 6, 10),
                          labels = c("bajo", "medio", "alto"))

df <- data.frame(id = 1:n, conoc_previos, nivel_conocimiento) %>%
  group_by(nivel_conocimiento) %>%
  mutate(grupo = rep(c(0,1), length.out = n())) %>%
  ungroup()

df$experiencia_debate <- round(pmin(pmax(rnorm(n, mean = 12, sd = 3), 5), 20))

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
                         round(rtruncnorm(n, 0, 100, mean = 60, sd = 15)),
                         round(rtruncnorm(n, 0, 100, mean = 35, sd = 15)))

df$rendimiento <- ifelse(df$grupo == 0,
                         round(rtruncnorm(n, 0, 100, mean = 70, sd = 15)),
                         round(rtruncnorm(n, 0, 100, mean = 50, sd = 15)))

df$NASA_total <- rowMeans(df[, c("demanda_mental", "demanda_temporal", "esfuerzo", "frustracion", "rendimiento")])

df$calidad_texto <- round(ifelse(df$grupo == 0,
                                 rtruncnorm(n, a=0, b=20, mean=15, sd=2.5),
                                 rtruncnorm(n, a=0, b=20, mean=12, sd=2.5)))

df$n_temas <- ifelse(df$grupo == 0,
                     round(rtruncnorm(n, a = 0, b = 6, mean = 4, sd = 1)),
                     round(rtruncnorm(n, a = 0, b = 6, mean = 2, sd = 1)))

df_final <- df %>%
  select(id, grupo, conoc_previos, nivel_conocimiento, experiencia_debate,
         demanda_mental, demanda_temporal, esfuerzo, frustracion, rendimiento,
         NASA_total, calidad_texto, n_temas)

#-------------------------------------
# LIMPIAR DATOS
#-------------------------------------
df_limpio <- df_final %>%
  filter(experiencia_debate < 16)

#-------------------------------------
# DESCRIPTIVOS
#-------------------------------------
variables <- c("NASA_total", "demanda_mental", "demanda_temporal", "esfuerzo",
               "frustracion", "rendimiento", "calidad_texto", "n_temas", "conoc_previos")

resumen <- df_limpio %>%
  group_by(grupo) %>%
  summarise(across(all_of(variables), list(
    media = ~mean(., na.rm = TRUE),
    mediana = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    IQR = ~IQR(., na.rm = TRUE),
    asimetria = ~skewness(., na.rm = TRUE),
    curtosis = ~kurtosis(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}"))

print(resumen)

#-------------------------------------
# T-TESTS + D de Cohen
#-------------------------------------
variables_ttest <- c("NASA_total", "demanda_mental", "demanda_temporal", "esfuerzo",
                     "frustracion", "rendimiento", "calidad_texto", "n_temas")

resultados_t <- list()
tabla_resumen <- data.frame(
  Variable = character(), Prueba = character(),
  Estadistico = character(), p = numeric(),
  Efecto = numeric(), CI95_inf = numeric(), CI95_sup = numeric(),
  stringsAsFactors = FALSE
)

for (var in variables_ttest) {
  t_test <- t.test(df_limpio[[var]] ~ df_limpio$grupo, var.equal = TRUE)
  d <- cohens_d(df_limpio[[var]] ~ df_limpio$grupo)
  
  tabla_resumen <- rbind(tabla_resumen, data.frame(
    Variable = var, Prueba = "t-test",
    Estadistico = paste0("t = ", round(t_test$statistic, 2)),
    p = round(t_test$p.value, 4),
    Efecto = round(d$Cohens_d, 2),
    CI95_inf = round(d$CI_low, 2),
    CI95_sup = round(d$CI_high, 2)
  ))
}

print(tabla_resumen)

#-------------------------------------
# REGRESIONES
#-------------------------------------

modelo_calidad <- lm(calidad_texto ~ esfuerzo, data = df_limpio)
modelo_temas <- lm(n_temas ~ esfuerzo, data = df_limpio)

beta_calidad <- standardize_parameters(modelo_calidad)
beta_temas <- standardize_parameters(modelo_temas)

tabla_regresion <- data.frame(
  Variable_dependiente = c("calidad_texto", "n_temas"),
  B_no_estandarizado = c(
    round(coef(modelo_calidad)[2], 3),
    round(coef(modelo_temas)[2], 3)
  ),
  Beta_estandarizado = c(
    round(beta_calidad$Std_Coefficient[2], 3),
    round(beta_temas$Std_Coefficient[2], 3)
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

print(tabla_regresion)

#-------------------------------------
# ALFA DE CRONBACH
#-------------------------------------

nasa_items <- df_limpio[, c("demanda_mental", "demanda_temporal", "esfuerzo", "frustracion", "rendimiento")]
print(round(cor(nasa_items, method = "pearson"), 3))
print(psych::alpha(nasa_items)$total$raw_alpha)

nasa_sin_frustracion <- df_limpio[, c("demanda_mental", "demanda_temporal", "esfuerzo", "rendimiento")]
print(psych::alpha(nasa_sin_frustracion)$total$raw_alpha)

