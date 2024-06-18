
# Quantile Regression using D-vine Copulas
### Developer: Brenda Paola Quintana Silva

This library was developed to perform a Quantile Regression with the D-vine model. This technique consists of decomposing the distribution function into pairs of copulas, and a graph (which is a path) determines this factorization. The following article explains how to build it [D-vine copula based quantile regression](https://www.sciencedirect.com/science/article/abs/pii/S0167947316303073). Additionally, the ideas of the copulas visualizations implemented in the library are taken from the following article [Copula-based statistical dependence visualizations](https://arxiv.org/abs/2204.00265).

## How to install the package
You have to run the following code in the terminal.

```
devtools::install_github("BesitosDeBaba/deerVineReg")
```

## Example of using

Import libraries
```
library(deerVineReg)
```

Import Dataset
```
data <- mtcars
attach(data)
```

### Model Fits
*constCop* - Function that fits the D-vine model. Introduce formula and data, 'estima' is True if you want to execute an algorithm that builds a path like the initial tree, maximizing Schweizer and Wolff sigma correlation.

*testCop* - This function prints the p-value test of independent of each copula.
```
#Ajuste del modelo
model <- constCop(formula = mpg ~ wt + drat + hp, data, estima = TRUE)
testCop(model)
```

### Model Prediction
*regresionPred* - This function makes the prediction
```
#Prediccion
pred <- regresionPred(model, model$data, 0.25)
```

### Visual Exploratory Analysis
```
#Visualizacion del modelo
diagonalGraf(model)
```

<img src="diagmodel.png" alt="diag" width="50%"/>


##
```
plot_effects(model, 'hp', c(0.1, 0.5, 0.9))
```

<img src="efectohp.png" alt="efecto" width="50%"/>


```
alleffect(model, alphas = c(0.1, 0.5, 0.9))
```

*HeatCopula* function that makes heat maps of a specific Copula. They are computed using the following formulas:

$$
\mathscr{H}_\sigma = 12\left|C_n\left( \frac{i}{n}, \frac{j}{n}\right)-\frac{i j}{n^2}\right|: i, j \in\{1, \ldots, n-1\},
$$

$$
\mathscr{H}_\rho = 12\left[C_n\left(\frac{i}{n}, \frac{j}{n}\right)-\frac{i j}{n^2}\right]: i, j \in\{1, \ldots, n-1\},
$$

$$
\mathscr{H} =  D_n\left( \frac{i}{n}, \frac{j}{n}\right) : i, j \in\{1, \ldots, n-1\},
$$
```
#Ver la forma especifica de una copula
heatsCopula(model, 'C12')
```

<img src="sigmaC12.png" alt="sigma" width="50%"/>

<img src="RhoC12.png" alt="Rho" width="50%"/>

<img src="difC12.png" alt="dif" width="50%"/>

```
showCopula(model, 'C12')
```
