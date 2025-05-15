install.packages("pwr")  # Solo si no lo tienes ya
library(pwr)

# Cálculo para muestras independientes, contraste bilateral
muestra <- pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.80,
                      alternative = "two.sided", type = "two.sample")

muestra$n
