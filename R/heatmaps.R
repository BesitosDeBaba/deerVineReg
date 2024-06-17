
#' Funcion que calcula la copula inferior (Bidimensional)
#' Recibe como parametro:
#' @param  U - data.frame dimensional
#' @return Retorma un dataframe unidimensional
#' @export
W <- function(U){
  return(max(U[1]+U[2]-1, 0))
}

#' Funcion que calcula la copula superior (Bidimensional)
#' Recibe como parametro:
#' @param U - data.frame dimensional
#' @return Retorna un dataframe unidimensional
#' @export
M <- function(U){
  return(min(U[1], U[2]))
}

#Funcion para heatmap
D <- function(info){
  C <- info[1]; UIJ <- info[2]; M <- info[3]; W <- info[4]
  if (C >= UIJ){
    res <- (C -UIJ)/(M - UIJ)
  }else{
    res <- -(UIJ - C)/(UIJ - W)
  }
  return(res)
}


#Funcion que obtiene los heatmaps que se necesitan para
#vizualizar copulas a pares. Recibe como argumentos:
#X - data frame de nx2 con las datos expiricos de las covariables
#n - en tamaño de resolulacion del grid
heats <- function(UX, copNombre, n = 100){
  #Variables para sacar la copulas empirica
  u <- seq(from = 0, to = 1, by = 1/(n-1))
  n <- length(u)
  #Matriz para hacer e grid
  matrizU <- data.frame(u1 = rep(0, n**2), u2 = rep(0, n**2))
  uj <- rep(0, n**2)

  #Hacemos en Grid
  cuenta <- 1
  for (i in 1:n){
    matrizU[cuenta:(i*n), 1] <- rep(u[i], n)
    matrizU[cuenta:(i*n), 2] <- u
    uj[cuenta:(i*n)] <- rep(u[i], n)*u

    cuenta <- cuenta + n
  }

  matrizU <- as.matrix(matrizU)

  #La copula empirica
  ec <- copula::C.n(matrizU, X = UX)

  #Hacemos las matrices
  ecF <- matrix(ec, ncol = n, nrow = n)
  UIJ <- matrix(uj, ncol = n, nrow = n)

  # Los heatmaps
  rho <- 12*(ecF - UIJ)
  sigma <- 12*abs(ecF - UIJ)

  breaksrho <- seq(-1, 1, by = 0.02)
  breaksigma <- seq(0, 1, by = 0.01)

  rho <- pheatmap::pheatmap(rho, breaks = breaksrho, border_color = NA,
                  cluster_cols = FALSE,
                  cluster_rows = FALSE, main = paste0('rho ', copNombre))
  print(rho)
  readline(prompt="Press [enter] to continue")
  #ggsave(rho, file = paste0(id, 'rho', nombre, '.png'), width = 4.5, height = 4)

  sigma <- pheatmap::pheatmap(sigma, breaks = breaksigma, border_color = NA,
                    color = rev(paletteer::paletteer_c("grDevices::PuBuGn", 200)),
                    cluster_cols = FALSE,
                    cluster_rows = FALSE, main = paste0('Sigma ', copNombre))
  print(sigma)
  readline(prompt="Press [enter] to continue")
  #ggsave(sigma, file = paste0(id, 'sigma', nombre, '.png'), width = 4.5, height = 4)

  #Heatmap de diferencias normalizadas
  info <- data.frame(copemp = ec, UVemp = uj,
                     Memp = apply(matrizU, MARGIN = 1, M),
                     Wemp = apply(matrizU, MARGIN = 1, W))
  H_melt <- data.frame(X1 = matrizU[,1], X2 = matrizU[,2],
                       value = apply(X = info, MARGIN = 1, FUN = D))

  #Corregimos algunos valores
  H_melt[is.infinite(H_melt$value), 3] <- sign(H_melt[is.infinite(H_melt$value), 3])*1
  H_melt[is.na(H_melt$value), 3] <- 0
  H_melt[abs(H_melt$value) > 1, 3] <- sign(H_melt[abs(H_melt$value) > 1, 3])*1

  heat <- ggplot2::ggplot(H_melt, ggplot2::aes(X1, X2)) + ggplot2::geom_tile(ggplot2::aes(fill = value))  +
    ggplot2::scale_fill_gradientn(colours = grDevices::hcl.colors(100, palette = "spectral", rev = TRUE),
                         limits = c(-1, 1)) + ggplot2::ylim(1, 0)

  H <- heat + ggplot2::geom_point(data = UX,
                                  ggplot2::aes(x = UX[,1], y = UX[,2]),
                         color = "black", size = 1.5, alpha = 0.5) +
    ggplot2::ggtitle(paste('Diferencias Normalizadas ', copNombre))
  suppressWarnings(print(H))
  readline(prompt="Press [enter] to continue")
  #ggsave(H, file = paste0(id, 'H', nombre, '.png'), width = 4.5, height = 4)
}


#' Funcion que hace la grafica de cualquier copula. La
#' diferencia con la funcion anterior la selección del data frame
#' de entrada. Pues esta depende del nivel de la copula.
#' Recibe como argumento:
#' @param model - un objetivo del tipo D-vine
#' @param copName - string que tiene el nombre de la copula, ejemplo 'C12'
#' @export
heatsCopula <- function(model, copName){
  i <- as.integer(substring(copName, 2, 2)); j <- as.integer(substring(copName, 3, 3))
  if (nchar(copName) == 3){
    heats(model$dataU[,c(i, j)], copName, n = 100)
  }else{
    cond <- substring(copName, 4, nchar(copName))
    h1 <- paste0('h', i, cond)
    h2 <- paste0('h', j, cond)

    X <- data.frame(h1 = model$hmodel[[h1]],
                    h2 = model$hmodel[[h2]])
    heats(X, copName, n = 100)
  }
}

############################################################
############################################################

#Funcion que obtiene los nombres de cada
#Copula de acuerdo a el nivel. Recibe como parametros:
#nCopNivel - el número de copulas que contiene ese nivel
#paraNivel - el nivel para el cual se quiere sacar los nombres de las
#           copulas.
#Retona: cop - array con los nombres de las copulas
copNivel <- function(nCopNivel, paraNivel){
  cop <- c()
  secuencia <- rep(0, nCopNivel)

  if (paraNivel == 1){
    l <- 1; j <- 2
    for (i in secuencia) {
      cop <- c(cop, paste0('C', l, j))
      l <- l + 1; j <- j + 1
    }
  }else{
    for (i in 1:length(secuencia)){
      ind <- c(i:(i + (paraNivel)) )
      segundo <- paste(ind[2:(length(ind)-1)], collapse = '')
      primer <- paste0('C', ind[1], max(ind), '|', segundo)
      cop <- c(cop, primer)
    }
  }
  return(cop)
}

#' Funcion que hace el graficos de todo el modelo de la D-vine.
#' Recibe como parametros:
#' @param model - un objeto del tipo del D-vine
#' @return none
#' @export
heatVine <- function(model){
  nodos <- length(colnames(model$dataU))

  for (nivel in 1:(nodos -1)){

    if (nivel == 1){
      #Nivel 1 -Copulas sencillas
      cops <- copNivel(nodos-1, nivel)
      for (copName in cops){
        heats(model$dataU[c(i, i+1)], copNombre = copName)
      }
    }else{
      cops <- copNivel(nodos-nivel, nivel)

      #Para niveles profundos - Con h's
      for (copName in cops){
        i <- as.integer(substring(copName, 2, 2)); j <- as.integer(substring(copName, 3, 3))

        cond <- substring(copName, 4, nchar(copName))
        h1 <- paste0('h', i, cond)
        h2 <- paste0('h', j, cond)

        X <- data.frame(h1 = model$hmodel[[h1]],
                        h2 = model$hmodel[[h2]])
        heats(X, copNombre = copName)
      }
    }
  }
}

