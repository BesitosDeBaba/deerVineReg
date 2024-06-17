#Funcion que estima las funciones h pero ya con las copulas ajustadas
#Recibe como argumento:
#model - objeto del tipo Dvine
#dataU - datos para ahcer la predicción en escala U
#Regresa: una lista de copulas
forwardPred <- function(model, dataU){
  #datosU, copulas
  nNodos <- ncol(model$dataU)
  h_selectPred <- list()

  #Ciclo que va sobre cada 'arbol'
  for (nivel in 1:(nNodos-1)){
    if (nivel == 1){
      #Obtenemos las funciones h para las Copulas de este nivel
      cond <- 2
      cops <- cparaH(nNodos-1, 2)

      #Sacamos las h en orden las de las copulas
      for (i in 1:(2*(nNodos-cond))){
        nomH <- nombreH(c(cond + (-1)^i, cond))
        h_selectPred[[nomH]] <- VineCopula::BiCopHfunc2(model$dataU[, (cond + (-1)^i)],
                                             model$dataU[, cond],
                                             model$copulasModel[[cops[i]]])
        if ((i %% 2) == 0) cond <- cond + 1
      }
      #Para niveles más profundos
    }else{
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

          h_selectPred[[nomH]] <- VineCopula::BiCopHfunc2(h_selectPred[[h1]],
                                               h_selectPred[[h2]],
                                              model$copulasModel[[cops[i]]])
          if ((i %% 2) == 0) cond <- cond + 1
        }
      }
    }
  }
  return(h_selectPred)
}

#Funcion que se encarga de sacar las inversas de cada copula
#la propagación hacia atrás. Recibe como argumento:
#datosU <- dataframe con las datos ordenados en su escala U
#alpha <- quantile que se quiere obtener
quantileRegPred <- function(model, datosUs, alpha, h_selectPred){
  nNodosAr <- ncol(datosUs)
  alphas <- rep(alpha, nrow(datosUs))

  for (i in (1:(nNodosAr-1))){
    indCop <- c(1:nNodosAr)
    #Regresamos para niveles condicionales

    #Seleccionamos la copulas y la funcion h que se requieren
    if (length(indCop) > 2){
      copula <- nombreC(c(min(indCop), max(indCop),
                          indCop[2:(length(indCop) -1)]) )
      funH <- h_selectPred[[nombreH(c( max(indCop),
                          indCop[2:(length(indCop) -1)]) )]]

      nNodosAr <- nNodosAr -1
    }else{
      copula <- nombreC(indCop)
      funH <- datosUs[, 2]
    }

    #Aplicamos la inversa
    alphas <- VineCopula::BiCopHinv2(alphas, funH, model$copulasModel[[copula]])
  }
  return(alphas)
}


#Funcion que calcula los cuantiles o transforma los valores datos a
#escala de acuerdo a las variables que forman la base original. Para seguir
#la misma distribución.
#Recibe como argumento:
#data - un data frame con los datos Orginales.
#dataPred - un dataframe con las nuevas observaciones
#Retorna ObserU - un dataframe con las observaciones en escala U
UdataPred <- function(data, dataPred){
  variables <- names(dataPred)
  predU <- dataPred
  count <- 1

  #Sacamos las F de acuerdo a la distribucion de cada variable
  for (i in variables){
    predU[, count] <-  ecdf(data[,i])(dataPred[,count])
    count <- count + 1
  }
  return(predU)
}

#' Funcion que hace la regresion de la variable obser, usando
#' las copulas generadas. Recibe como argumento:
#' @param model - un objeto del tipo de Dvine
#' @param dataPred - data frame con las observaciones que se les quiere
#'        hacer prediccion, no tiene que estar en escala U.
#' @param alpha - nivel al cual se va a hacer la regresión
#' NOTA: Obser deben tener los mismos nombres que en las variables en escala U
#' @export
regresionPred <- function(model, dataPred, alpha){
  dataUpred <- UdataPred(model$data, dataPred)
  h_selectPred <- forwardPred(model, dataUpred)

  #Hacemos las predicciones
  regA1 <- quantileRegPred(model, dataUpred, alpha, h_selectPred)
  yA1 <- quantile(model$reponse, regA1)

  return(yA1)
}

#####################################################################

#Funcion que le mueve ligeramente para encontrar la Raiz
regresionSemaforo <- function(datos, obser, corte, y_real, alpha, copulas){
  h_selectPred <<- list()
  dataU <- Uexternos(datos, obser)
  hs <- forwardPred(dataU, copulas)

  #Hacemos las predicciones
  regA1 <- quantileRegPred(dataU, alpha, copulas)
  yA1 <- quantile(y_real, regA1)

  return(yA1-corte)
}

