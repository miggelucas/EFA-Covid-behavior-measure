library(psych)

df <- read.csv("database.csv")

# Medidas de adequação para EFA

result_bartlett <- cortest.bartlett(df)
print(result_bartlett$p)
## Significativo para bartlett

result_kmo <- KMO(df)
print(result_kmo$MSA)
## KMO = 0.855

# Inciando EFA
efa <- fa(df, nfactors = 2, rotate = "oblimin", fm="minres", cor= "poly")

# Scree plot
autovalores <- efa$values
print(autovalores)
num_fatores <- seq_along(autovalores)
plot(num_fatores, autovalores, type="b", xlab="Número de fatores", ylab="Autovalores")
## tanto os autovalores quanto o scree plot indicam 2 fatores

summary(efa)
## RMSR = 0.06 indica um bom valor de ajuste para o modelo

fa.diagram(efa)

plot(efa)

