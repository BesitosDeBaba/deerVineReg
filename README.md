

# Quantile Regression using D-vine Copulas
### Brenda Paola Quintana Silva

This library was developed to perform a Quantile Regression with the D-vine model. This technique consists of decomposing the distribution function into pairs of copulas, and a graph (which is a path) determines this factorization. The following article explains how to build it [D-vine copula based quantile regression](https://www.sciencedirect.com/science/article/abs/pii/S0167947316303073). Additionally, the ideas of the copulas visualizations implemented in the library are taken from the following article [Copula-based statistical dependence visualizations](https://arxiv.org/abs/2204.00265).

## How to install the package
You have to run the following code in the terminal.

```
devtools::install_github("BesitosDeBaba/deerVineReg")
```

## Example of using
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
