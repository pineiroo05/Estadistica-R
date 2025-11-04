data(Davis, package="carData")
str(Davis)

CV(Davis$height)
cv<-(sd(Davis$height)/mean(Davis$height))
cv

#Si se pone con cv en minusculas no saca el valor, no esta llamando a la funcion de R CV.
calcularResumenVariablesContinuas(data=Davis["height"], statistics=c("mean", "sd", "se(mean)", "IQR", "quantiles", 
  "cv", "skewness", "kurtosis"), quantiles=c(0,0.25,0.5,0.75,1), type="2", tablaFrecuencia=FALSE, cortes=NULL)

#Asi si va
calcularResumenVariablesContinuas(data=Davis["height"], statistics=c("mean", "sd", "se(mean)", "IQR", "quantiles", 
  "CV", "skewness", "kurtosis"), quantiles=c(0,0.25,0.5,0.75,1), type="2", tablaFrecuencia=FALSE, cortes=NULL)

