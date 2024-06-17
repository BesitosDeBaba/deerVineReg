#' Funcion que hace la grafica de effectos de la variable (solo una)
#' deseadas. Recibe como argumentos:
#' @param model - un objeto del tipo Dvine
#' @param variable - string con la variable de cual se quiere ver los efectos
#' @param alpha - vector de double, el nivel alpha con el cual se va a hacer
#'        la regresión cuantil
#' @return Retorna un 1, si la función tiene éxito.
#' @export
plot_effects <- function(model, variable, alpha){
  data <- model$data
  medias <- colMeans(model$data)
  variablefija <- c(names(medias)[1], variable)

  #Se crea un data.frame auxilar con las medias de las otras variables
  #que no son la respuesta y la desada
  for (var in colnames(model$data)) {
    if (!(var %in% variablefija)){
      data[var] <- medias[var]
    }
  }

  #Se hace la regresión pasra distintos niveles de alpha
  datosAlphas <- data.frame(matrix(ncol = length(alpha), nrow = nrow(data)))
  colnames(datosAlphas) <- as.character(alpha)

  for (i in 1:length(alpha)){
    datosAlphas[,i] <- regresionPred(model, model$data, alpha[i])
  }
  datosAlphas$x <- model$data[[variablefija[2]]]

  #Se un pivote para poder graficar los efectos
  dfgraf <- tidyr::pivot_longer(datosAlphas, !x, names_to = "Alphas", values_to = "Pred")
    #datosAlphas %>% pivot_longer(!x, names_to = "Alphas", values_to = "Pred")

  #Se hace la grafica
  graf <- ggplot2::ggplot(dfgraf, ggplot2::aes(x = x, y = Pred, colour = Alphas)) +
    ggplot2::geom_point() + #geom_smooth(se = FALSE) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE) +
    ggplot2::ylab('Q(a)') + ggplot2::xlab(paste0(variable)) +
    ggplot2::ggtitle(paste0('Graf. de efectos de la variable ', variable))
  #ggsave(graf, file = paste0(nombre, '.png'), width = 4.5, height = 4)
  return(graf)
}

#' Función que hace la grafica de efectos de todas las variables de un modelo.
#' Recide como parametros:
#' @param model - objeto del tipo Dvine
#' @param alphas - vector de double. Vector que contiene los niveles con los que
#'      se va a hacer la regresión cuantil.
#' @return None
#' @export
alleffect <- function(model, alphas){
  for (var in colnames(model$data[-c(1)])){
    print(var)
    print(plot_effects(model, var, alphas))
    readline(prompt="Press [enter] to continue")
  }
}

############################################################
######### FUNCIONES QUE MUESTRAN LAS DEPEDENCIAS ###########
############################################################

#' Funcion que grafica las copulas. La primera grafica
#' las curvas de nivel, la segunda es la grafica de la diagonal.
#' Recibe como parametros:
#' @param model - un objetivo del D-vine
#' @param copName - string, con el nombre de la copula, ejemplo 'C12'
#' @param col - vector de colores para la curva de nivel
#' @export
showCopula <- function(model, copName, col = viridis::inferno(20)){
  i <- as.integer(substring(copName, 2, 2)); j <- as.integer(substring(copName, 3, 3))

  if (nchar(copName) == 3){

    #Grafica de curvas de nivel
    graphics::contour(model$copulasModel[[copName]], margins = "unif",
            col = col[1], lwd = 3,
            main = paste0('Curvas de Nivel ', copName))
    graphics::points(model$dataU[,i], model$dataU[,j], pch = 20, col = rgb(0, 0, 0, alpha = 0.5))

    readline(prompt="Press [enter] to continue")

    #Grafica de las diagonales
    graf <- diagonal(model$dataU[c(i, j)]) + ggplot2::ggtitle(paste0('Diagonal ', copName))
    print(graf)
    readline(prompt="Press [enter] to continue")

    #Heatmaps
  }else{
    cond <- substring(copName, 4, nchar(copName))
    h1 <- paste0('h', i, cond)
    h2 <- paste0('h', j, cond)

    X <- data.frame(h1 = model$hmodel[[h1]],
                    h2 = model$hmodel[[h2]])

    #Grafica de curvas de nivel
    graphics::contour(model$copulasModel[[copName]], margins = "unif",
            col = col[1], lwd = 3,
            main = paste0('Curvas de Nivel ', copName))
    graphics::points(X[,1], X[,2], pch = 20, col = rgb(0, 0, 0, alpha = 0.5))

    readline(prompt="Press [enter] to continue")

    #Grafica de las diagonales
    graf <- diagonal(X) + ggplot2::ggtitle(paste0('Diagonal ', copName))
    print(graf)
    readline(prompt="Press [enter] to continue")
  }
}


#' Función que hace las grafias de showCopula para todo
#' el modelo Dvine. Recibe como parametros:
#' @param model - un objeto de tipo D-vine
#' @param col - gamma de colores para las curvas de nivel
#' @return None
#' @export
ShowVine <- function(model, col = inferno(20)){
  nodos <- length(model$dataU)
  ncolo <- 1

  for (nivel in 1:(nodos-1) ){
    if (nivel == 1){
      cops <- copNivel(nodos-1, nivel)
      #Nivel 1 -Copulas sencillas
      for (copName in cops){
        showCopula(model, copName, col = inferno(20))
        ncolo <- ncolo + 1
      }
    }else{
      #Para niveles profundos - Con h's
      cops <- copNivel(nodos-nivel, nivel)

      #Para niveles profundos - Con h's
      for (copName in cops){
        i <- as.integer(substring(copName, 2, 2)); j <- as.integer(substring(copName, 3, 3))

        cond <- substring(copName, 4, nchar(copName))
        h1 <- paste0('h', i, cond)
        h2 <- paste0('h', j, cond)

        X <- data.frame(h1 = model$hmodel[[h1]],
                        h2 = model$hmodel[[h2]])

        showCopula(model, copName, col = inferno(20))
      }
    }
  }
}





