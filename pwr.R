install.packages("pwr")
library(pwr)

muestra <- pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.80, 
                        alternative = "greater", type = "two.sample")

muestra$n  
