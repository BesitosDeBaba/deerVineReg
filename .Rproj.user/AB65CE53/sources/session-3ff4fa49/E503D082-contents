
#Funcion que graficas la diagonal de la copulas
#Recibe como argumentos:
#datos - un data frame con los datos a los cuales se les quiere
#modelar con copulas
diagonal <- function(datos){
  copula <- cee(datos)
  d <- dim(copula)
  diag <- vector(mode="numeric",length=d[1])

  for(i in 1:d[1]){
    diag[i] <- copula[i,i]
  }

  #Las funciones
  Md<-function(u) u
  Pd<-function(u) u^2
  Wd<-function(u) max(2*u-1,0)

  w=sapply(seq(0,1,1/(d[1]-1)), Wd)
  p=sapply(seq(0,1,1/(d[1]-1)), Pd)
  m=sapply(seq(0,1,1/(d[1]-1)), Md)

  val <- data.frame('x' = seq(0,1,1/(d[1]-1)), 'Diag' = diag,
                    'Superior' = m, 'Pi' = p, 'Inferior' = w)

  new <- tidyr::pivot_longer(val, !x, names_to = "curva", values_to = "valores")

  #Grafica
  graf <- ggplot2::ggplot(new, ggplot2::aes(x=x, y=valores, group = curva, color = curva)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c('black', "darkred", "steelblue", 'greenyellow')) +
    ggplot2::geom_line()

  return(graf)
}

#' Funcion que hace graficos de dispersion
#' Hacemos las graficas de las diagonales en la
#' Con las tablas de dispersion en la triangular superior.
#' Recibe como parametro:
#' @param model - un objeto del tipo D-vine
#' @return None
#' @export
diagonalGraf <- function(model){
  data <- model$dataU
  graficas <- list()
  n <- ncol(data)
  cuenta <- 1

  for (i in 1:n) {
    for (j in 1:n){
      if (j < i){
        #Graficas de diagonal
        graf <- diagonal(data[c(i, j)])
        Mylegend <- cowplot::get_legend(graf)
        graf <- graf + ggplot2::theme(legend.position="none")

      }else if(j > i){
        #Graficos de dispersion
        df <- data[c(j, i)]; colnames(df) <- c('x', 'y')
        graf <- ggplot2::ggplot(data = df, ggplot2::aes(x=x, y=y)) +
          ggplot2::geom_point() +
          ggplot2::geom_rug(col="steelblue", alpha=0.1, linewidth=1.5)
      }else{
        graf <- ggplot2::ggplot() +                      # Draw ggplot2 plot with text only
          ggplot2::annotate("text",
                   x = 1,
                   y = 1,
                   size = 8,
                   label = paste0(names(data[i]))) +
          ggplot2::theme_void()
      }
      #Anadimos la grafica a la lista
      graficas[[cuenta]] <- graf
      cuenta <- cuenta + 1
    }
  }
  gridExtra::grid.arrange(grobs = graficas, Mylegend, nrow = n,
               top = 'Graficos de dispersión')
}

#Funcion que se encarga de graficar los intervalos
#Recibe como argumento:
#alpha1 - float con en primer cuantil
#alpha2 - float con el segundo cuantil
#Retorna un grafica
grafInter <- function(alpha1, alpha2, y_real, datosU){
  #Hacemos las predicciones
  regA1 <- quantileReg(datosU, alpha1)
  yA1 <- final(regA1, y_real)

  regA2 <- quantileReg(datosU, alpha2)
  yA2 <- final(regA2, y_real)

  #Dataframe para graficas
  visual <- data.frame(y_real, yA1, yA2)
  visual <- visual[order(visual$y_real), ]; visual$x <- c(1:nrow(visual))
  #
  vis <- tidyr::pivot_longer(visual, !x, names_to = "Regresion", values_to = "valores")

  graf <- ggplot2::ggplot(vis, ggplot2::aes(x=x, y= valores, group=Regresion, color= Regresion)) +
    ggplot2::geom_line() +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::ggtitle(paste0("Regresión cuantil, con alpha_! = ", alpha1, " alpha_2 = " , alpha2) ) +
    ggplot2::ylab("Q(a|x)")

  print(paste0('Longitud media del Intervalo: ', mean(yA2 - yA1)))
  return(graf)
}
