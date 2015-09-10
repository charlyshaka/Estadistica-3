ar2simA2.R <- function(phi,n1,sigma){
  #
  #  genera procesos AR(2) con parametros  phi = (phi1,phi2)
  #  Al correr se debe asignar phi = c(0.1 ,0.2) n1=100, sigma = 1,  p. ej.
  #  esta funcion  puede requerir cargar librerias "tseries" y "timeSeries"
  #  Esta version (ACTIVIDAD 2) de funcion es para observar la sucesion
  #  de autocorrelacion 
  
  arcoeffs <- c(1,-phi)
  
  raices <- polyroot(arcoeffs)
  
  minMod <- min(Mod(raices))
  
  if(minMod <= 1){print("proceso no estacionario")
                  print(raices)
  }
  
  ts.sim <- arima.sim(n = n1, list(ar = phi),
                      sd = sigma)
  
  frame()
  par(mfrow=c(1,2))
  acf(ts.sim)
  
  
  ts.plot(ts.sim)
  
  
}