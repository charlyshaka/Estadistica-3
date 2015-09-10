generaAR4.R <- function(n,phi1,phi2,phi3,phi4){
  # This code generates n observations
  # from an AR(4) process with autoregressive
  # parameter phi and white noise variance = 1.  
  # The program burns mburn = 300 observations
  # before the process gets to stationary conditions.
  
  # Se abre la libreria ggplot2
  library("ggplot2")
  
  mburn <- 300
  
  m <- n + mburn
  
  y <- {1:m}
  tiempo <- {1:n}
  
  # generar ruido blanco
  eps <- rnorm(m)
  
  y[1] <- eps[1]
  y[2] <- eps[2]
  y[3] <- eps[3]
  y[4] <- eps[4]
  
  # generar proceso AR(1) en vec y[]
  for(i in {5:m}){
    
    y[i] <- phi1*y[i-1] +phi2*y[i-2]+phi3*y[i-3]+phi4*y[i-4] + eps[i]}
  
  # tirar las primeras mburn observaciones
  y <- y[(mburn+1):m]
  
  dat<- data.frame(tiempo,y)
  
  # graficar la serie de tiempo
  bp<- ggplot(data=dat, aes(x=tiempo, y=y, group=1)) + 
    geom_line(colour="blue", linetype="solid", size=1.5) + 
    geom_point(colour="black", size=3, shape=21, fill="white")
  
  bp + ggtitle("METODO AUTOREGRESIVO\n AR(4)") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  # fin de la funcion
}
