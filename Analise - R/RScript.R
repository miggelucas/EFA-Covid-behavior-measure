library(psych)
library(readr)


df <- read_csv("Base de Dados/database.csv")


# Medidas de adequação para EFA

result_bartlett <- cortest.bartlett(df)
print(result_bartlett$p)
## Significativo para bartlett

result_kmo <- KMO(df)
print(result_kmo$MSA)
## KMO = 0.855



# Inciando EFA
efa <- fa(df, nfactors = 2, rotate = "oblimin", fm="minres", cor= "poly")



# Scree plot e autovalores
autovalores <- efa$values
print(autovalores)
num_fatores <- seq_along(autovalores)
plot(num_fatores, autovalores, type="b", xlab="Número de fatores", ylab="Autovalores")
## tanto os autovalores quanto o scree plot indicam 2 fatores

summary(efa)
## RMSR = 0.06 indica um bom valor de ajuste para o modelo

# plot de diagrama
fa.diagram(efa)
efa$loadings

## itens que compoem o primeiro fator
itens_fa1 <- names(efa$loadings[,1][abs(efa$loadings[,1]) > 0.3])
itens_fa1

## itens que compoem o segundor fator
itens_fa2 <- names(efa$loadings[,2][abs(efa$loadings[,2]) > 0.323])
itens_fa2




# Medindo precisão


plot(efa)

df_fa1 <- df[c(itens_fa1)]
df_fa2 <- df[c(itens_fa2)]

alpha_fa1 <- alpha(df_fa1)
omega_fa1 <- omega(df_fa1, nfactors = 1,poly = TRUE)
print(alpha_fa1)
print(omega_fa1)
## alpha_fa1 = 0.82
## omega_fa1 = 0.89
## o drop de itens não aumenta o coeficiente

alpha_fa2 <- alpha(df_fa2)
omega_fa2 <- omega(df_fa2, nfactors = 1,poly = TRUE)
print(alpha_fa2)
print(omega_fa2)
## alpha_fa2 = 0.79
## omega_fa2 = 0.87 
## retirar o item "Uso.de.luvas.ao.sair.de.casa" aumenta o alpha para 0.80
## "Cobrir.a.boca.quando.tossir.ou.espirrar" faz sentido que esteja dentro do fator de distanciamento?

