# Regresión Cuantil usando Copulas

## Cómo instalar la paqueteria

```
devtools::install_github("BesitosDeBaba/deerVineReg")
```

## Ejemplo de uso
```
library(deerVineReg)

data <- mtcars
attach(data)

#Ajuste del modelo
model <- constCop(formula = mpg ~ wt + drat + hp, data, estima = TRUE)
testCop(model)

#Prediccion
pred <- regresionPred(model, model$data, 0.25)

#Visualizacion del modelo
diagonalGraf(model)

plot_effects(model, 'hp', c(0.1, 0.5, 0.9))
alleffect(model, alphas = c(0.1, 0.5, 0.9))

#Ver la forma especifica de una copula
heatsCopula(model, 'C12')
showCopula(model, 'C12')

```
