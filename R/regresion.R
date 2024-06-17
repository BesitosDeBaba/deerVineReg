##########################################################
#Aqui se encuentras las funciones base para correr la regresión
#cuantil. Todo se debe de correr antes de ejucutar el main.

#' Función que saca la correlacion de Spearman.
#' Recibe como argumento:
#' @param datos - un data frame con todas las variables que se
#' les quiere sacar la correlacion de Spearman.
#' @return Retorna la matriz de correlacion.
#' @export
corSpear <- function(datos){
  n = ncol(datos)
  i <- 1
  j <- 1
  nombres <- colnames(datos)
  corr <- matrix(rep(0, n*n), nrow = n, ncol = n)

  for (i in 1:n){
    for (j in i:n){
      corr[i, j] <- stats::cor(datos[i], datos[j], method ="spearman")
      corr[j, i] <- corr[i, j]
    }
  }
  colnames(corr) <- nombres
  rownames(corr) <- nombres
  return(corr)
}

cargam3 <- function(i,n){
  r=floor(i/n)+1
  c=i-(r-1)*n+1
  r*c/n^2
}

#Funcion que calcula la correlacion sigma de wolf-schviezer xd
#Recibe como argumento un data frame con todas las variables que se
#les quiere sacar la correlacion sigma de Wolf and Schewiezer.
#Retorna la matriz de correlacion.
s.w.e<-function(datos){
  cp=cee(datos)
  n=length(cp[,1])-1
  12*sum(abs(cp[2:(n+1),2:(n+1)]-
               matrix(sapply(0:(n*n-1),cargam3,n),ncol=n)))/(n^2-1)
}


#' Función que saca la correlacion wolf con la funcion que R tiene
#' implementada en el paquete copBasic. Para hacer el arbol o el camino
#' base de la D-vine.Recibe como argumento:
#' @param datos un data frame con todas las variables que se
#' les quiere sacar la correlacion de igma de Wolf and Schewiezer.
#' @return Retorna la matriz de correlacion.
#' @export
corSWEC <- function(datos){
  n = ncol(datos)
  i <- 1
  j <- 1
  nombres <- colnames(datos)
  corr <- matrix(rep(0, n*n), nrow = n, ncol = n)

  #Obtenemos la matriz
  for (i in 1:n){
    for (j in (i):n){
      muestra <- datos[c(i, j)]
      corr[i, j] <- s.w.e(muestra)
      corr[j, i] <- corr[i, j]
    }
  }

  colnames(corr) <- nombres
  rownames(corr) <- nombres
  #Asignamos los nombres
  return(corr)
}


#Función que saca la correlacion wolf (no saca las la correlacion en la
#diagonal).
#Recibe como argumento un data frame
#Retorna una matriz
corSWE <- function(datos){
  n = ncol(datos)
  i <- 1
  j <- 1
  nombres <- colnames(datos)
  corr <- matrix(rep(0, n*n), nrow = n, ncol = n)

  #Obtenemos la matriz
  for (i in 1:n){
    for (j in (i):n){
      if (i != j){
        muestra <- datos[c(i, j)]
        corr[i, j] <- copBasic::wolfCOP(para=muestra, as.sample=TRUE)
        corr[j, i] <- corr[i, j]
      }
    }
  }

  colnames(corr) <- nombres
  rownames(corr) <- nombres
  #Asignamos los nombres
  return(corr)
}

#Funcion que hace el nuevo orden dada la matriz de correlacion
#Recibe como argumento:
#formula - la formula con las variables deseadas
#corr - la matriz de correlacion
#Retorna un vector con el nuevo orden
ordenW <- function(formulaVars, corr){
  val <- formulaVars[1]
  ordenF <- c(val)

  for (i in 1:(length(formulaVars)-1)){
    indice <- which(colnames(corr) == val)
    res <- which(max(corr[val,]) == corr[val,])


    #Remove la columna y renglon
    if (i != (length(formulaVars)-1)){
      corr <- corr[-indice, ]
      corr <- corr[, -indice]
    }

    ordenF[i+1] <- names(res)
    val <- names(res)
  }

  return(ordenF)
}



# Funcion construye el primer árbol que define toda la D-vine.
# Se le puede poner le orden con el que se desea hacer la regresión
# o se hace en árbol incial maximizando la correlación de wolf
# Recibe como argumentos:
#formula - objetivo tipo formula
#data - el data set
#estima - boolean que indica si hacer el arbol maximizando la correlación o no

arbolInicial <- function(formula, data, estima = FALSE){
  if (estima == TRUE){
    corr <- corSWE(model.frame(formula, data))
    orden <- ordenW(colnames(corr), corr)
    return(data[orden])
  }else{
    #Devolvemos el orden deseado
    return(model.frame(formula, data))
  }
}

#------------------- C O P U L A S -------------------------
#' Funcion que convierte variables a su escala U. Recibe como argumentos:
#' @param datos - dataframe donde cada columna corresponde a una variable
#' @return Retorna el dataframe con los datos en escala u
#' @export
escalaU <- function(datos){
  uescala <- datos

  #Se convierte a escala u cada variable
  for (i in 1:ncol(datos)){
    uescala[i] <- ecdf(datos[,i])(datos[,i])
  }
  return(uescala)
}

#Funcion que hace los nombres de copulas. De acuerdo, a los
#índices introduccidos. Esta función recibe como parámetros:
#indice = un vector con los indices de las copula
#Retorna un string con el nombre de la cópula.
nombreC <- function(indice){
  if (length(indice) == 2){
    return(paste0('C', paste0(indice, collapse = "")))
  }else{
    primer <- paste0('C', paste0(indice[1:2], collapse = ""))
    segundo <- paste0('|', paste0(indice[3:length(indice)], collapse = ""))
    return(paste0(primer, segundo))
  }
}

#Funcion que saca todas los nombres de las C para generar que
#se van a requerir en el siguiente nivel. Esta función recibe como
#argumentos:
#nCopNivel - el número de copulas actuales
#
cparaH<- function(nCopNivel, paraNivel, T = nNodos){
  cop <- c()
  secuencia <- c(1, rep(2, (nCopNivel-2)), 1)

  if (paraNivel == 2){
    l <- 1; j <- 2
    for (i in secuencia) {
      cop <- c(cop, rep(paste0('C', l, j), i))
      l <- l + 1; j <- j + 1
    }
  }else{
    for (i in 1:length(secuencia)){
      ind <- c(i:(i + (paraNivel-1)) )
      segundo <- paste(ind[2:(length(ind)-1)], collapse = '')
      primer <- paste0('C', ind[1], max(ind), '|', segundo)
      cop <- c(cop, rep(primer, secuencia[i]))
    }
  }
  return(cop)
}


#Funcion que hace los nombres de las funciones H
#Vamos a seguir la Nomclatura del articulo para no tener confusiones
#sobre la varaible con respecto a la que se tiene que derivar
nombreH <- function(indice){
  #Para el nivel base
  if (length(indice) == 2){
    return(paste0('h', indice[1], '|', indice[2]))
  } # Para niveles más rpfundos
  else{
    primer <- paste0('h', indice[1], '|', indice[2])
    segundo <- paste0(indice[3:length(indice)], collapse = "")
    return(paste0(primer, segundo))
  }
}


#Funcion que estima todas las Copulas y funciones h
#Recibe como argumento:
#datosU <- dataframe con las variables en escala U y ordenadas
#de acuerdo al orden de importancia
#Regresa: una lista de copulas
forward <- function(datosU, criterio = 'BIC'){
  nNodos <- ncol(datosU)

  #Ciclo que va sobre cada 'arbol'
  for (nivel in 1:(nNodos-1)){
    if (nivel == 1){
      #Obtenemos las copulas del nivel 1
      for (i in 1:(nNodos-nivel)){
        #Obtenemos las copulas seleccionadas de forma parametrica
        copulas_select[[nombreC(c(i, i + nivel))]] <<- VineCopula::BiCopSelect(datosU[,i],
                                                                   datosU[,i+1],
                                                                   selectioncrit = criterio)
      }

      #Obtenemos las funciones h para las Copulas de este nivel
      cond <- 2
      cops <- cparaH(nNodos-1, 2)

      #Sacamos las h en orden las de las copulas
      for (i in 1:(2*(nNodos-cond))){
        nomH <- nombreH(c(cond + (-1)^i, cond))
        h_select[[nomH]] <<- VineCopula::BiCopHfunc2(datosU[, (cond + (-1)^i)], datosU[, cond],
                                         copulas_select[[cops[i]]])
        if ((i %% 2) == 0) cond <- cond + 1
      }
      #Para niveles más profundos
    }else{
      cond <- c(2:nivel)

      for (i in 1:(nNodos-(nivel))){
        #Calculamos los indices correspondientes a la copulas
        nomC <- nombreC(c(i, i + nivel, c((i+1): (i + (nivel -1))) ) )
        copulas_select[[nomC]] <<- VineCopula::BiCopSelect(h_select[[nombreH(c(i, cond))]],
                                               h_select[[nombreH(c(i + nivel, cond))]],
                                               selectioncrit = criterio)
        cond <- cond + 1
      }

      #Calculamos las h's correspondientes a este nivel

      if ((nNodos-(nivel + 2)) >= 0){
        cops <- cparaH(nNodos-nivel, nivel + 1, T = nNodos)

        #Sacamos las h en orden las de las copulas
        cond <- c(1:(1 + (nivel)+1) )

        for (i in 1:length(cops)){
          primer <- abs(max(cond*((-1)^i)))
          condicional <- cond[2:(length(cond)-1)]
          nomH <- nombreH(c(primer, condicional))

          #--- Sacamos las H anidadas

          #Obtemos los nombres de las h
          if (i %% 2 == 1){
            h1 <- nombreH(c(primer, condicional[1:(length(condicional)-1)]))
            h2 <- nombreH(c(condicional[length(condicional)],
                            condicional[1:(length(condicional)-1)]))
          }else{
            h1 <- nombreH(c(primer, condicional[2:length(condicional)]))
            h2 <- nombreH(c(condicional[1],
                            condicional[2:length(condicional)]))
          }

          h_select[[nomH]] <<- VineCopula::BiCopHfunc2(h_select[[h1]],
                                           h_select[[h2]],
                                           copulas_select[[cops[i]]])
          if ((i %% 2) == 0) cond <- cond + 1
        }
      }
    }
  }
  return(copulas_select)
}

#Funcion que se encarga de sacar las inversas de cada copula
#la propagación hacia atrás. Recibe como argumento:
#datosU <- dataframe con las datos ordenados en su escala U
#alpha <- quantile que se quiere obenter
quantileReg <- function(datosUs, alpha){
  nNodosAr <- ncol(datosUs)
  alphas <- rep(alpha, nrow(datosUs))

  for (i in (1:(nNodosAr-1))){
    indCop <- c(1:nNodosAr)
    #Regresamos para niveles condicionales

    #Seleccionamos la copulas y la funcion h que se requieren
    if (length(indCop) > 2){
      copula <- nombreC(c(min(indCop), max(indCop), indCop[2:(length(indCop) -1)]) )
      funH <- h_select[[nombreH(c( max(indCop), indCop[2:(length(indCop) -1)]) )]]

      nNodosAr <- nNodosAr -1
    }else{
      copula <- nombreC(indCop)
      funH <- datosUs[, 2]
    }

    #Aplicamos la inversa
    alphas <- VineCopula::BiCopHinv2(alphas, funH, copulas_select[[copula]])
  }
  return(alphas)
}

#Solo falta regresarlo a la escala normal la variable respuesta
#Recibe como argumento:
#regU - vector con la regresión en escala U
#y_real - vector o dataframe con los datos reales en la variable real.
#Retorna el vector en su escala normal.
final <- function(regU, y_real){
  res <- quantile(y_real, regU)
  return(res)
}

################ LA PRINCIPAL ##################
#' Funcion que contruye el ajuste de copulas. Recibe como parametros:
#' @param formula - la formula a ajustar
#' @param data - dataframe con los datos a ajustes (no tienen que estar en
#'       escala u)
#' @param estima - boolean que indica, si se ajusta el modelo con la formula (FALSE)
#'        o se contruye el primer árbol maximizando la correlacion de wolff (TRUE)
#' @param criterio - El criterio con el que se ajustan las copulas 'BIC', 'AIC' o 'loglikehood'
#' @return Retorna un objeto del tipo model Dvine
#' @export
constCop <- function(formula, data, estima = FALSE, criterio = 'BIC'){
  #Orden de las funciones

  #Hacemos el orden de lal árbol base
  t1 <- arbolInicial(formula, data = data, estima)
  datosU <<- escalaU(t1)

  #Variales globales para guardar las copulas Copulas y las h
  copulas_select <<- list()
  h_select <<- list()

  #Funcion que obtiene las C y las H requeridas
  copulas <- forward(datosU, criterio)
  model <- list(copulasModel = copulas_select, hmodel = h_select,
                data = t1, dataU = datosU,
                reponse = t1[,1], responseU = datosU[,1],
                call = match.call())
  class(model) <- 'Dvine'

  rm(list = deparse(substitute(copulas_select)), envir = globalenv())
  rm(list = deparse(substitute(h_select)), envir = globalenv())
  rm(list = deparse(substitute(datosU)), envir = globalenv())
  return(model)
}

#------ FUNCIONES EXTRAS PARA VER EL PERFORMANCE EN CADA NIVEL ---------

#' Funcion que imprime los test de independencia de cada copula.
#' Recibe como parametro:
#' @param model - Un objetivo del tipo model Dvine
#' @return None
#' @export
testCop <- function(model){
  nombres <- names(model$copulasModel)
  i<- 1

  #Se imprimen las cosas necesarias
  for (cop in model$copulasModel){
    infoCop <- cbind(
      copula = nombres[i],
      test = model$copulasModel[[nombres[i]]]$p.value.indeptest
    )
    i <- i + 1
    print(infoCop)
  }
}

#Funcion que genera la copula empirica
cee <- function(datos){
  F<-function(i,k){
    n=length(k)
    datos=rep(0,(n+1))
    datos[k[1:i]]=rep(1/n,i)
    datos=cumsum(datos)
    datos
  }
  n=length(datos[,1])
  copula<-matrix(nrow=(n+1),ncol=(n+1))
  ii<-order(datos[,2])
  h<-rbind(datos[,1],datos[,2])[,ii]
  k<-order(h[1,])+1
  r=k
  copula[1,]=rep(0,(n+1))
  copula[2:(n+1),]=t(sapply(c(1:n),F,k=r))
  copula
}

