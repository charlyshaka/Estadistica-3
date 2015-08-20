generaAR.R <- function(n,phi){
  # This code generates n observations
  # from an AR(1) process with autoregressive
  # parameter phi and white noise variance = 1.  
  # The program burns mburn = 200 observations
  # before the process gets to stationary conditions.
  
  # Se abre la libreria ggplot2
  library("ggplot2")
  
  mburn <- 200
  
  m <- n + mburn
  
  y <- {1:m}
  tiempo <- {1:n}
  
  # generar ruido blanco
  eps <- rnorm(m)
  
  y[1] <- eps[1]
  
  
  # generar proceso AR(1) en vec y[]
  for(i in {2:m}){
    
    y[i] <- phi*y[i-1] + eps[i]
    
  }
  
  # tirar las primeras mburn observaciones
  y <- y[(mburn+1):m]
  
  dat<- data.frame(tiempo,y)
  
  # graficar la serie de tiempo
  bp<- ggplot(data=dat, aes(x=tiempo, y=y, group=1)) + 
    geom_line(colour="orange", linetype="solid", size=1.5) + 
    geom_point(colour="red", size=4, shape=21, fill="white")
   
  bp + ggtitle("Grafica Ruido Blanco\n AR(1)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  # fin de la funcion
}
