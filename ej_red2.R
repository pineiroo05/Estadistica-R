datos <- iris
vector_minimos <- apply(datos[,1:4], 2, min)
vector_maximos <- apply(datos[,1:4], 2, max)
vector_rango <- vector_maximos - vector_minimos

#Normalizacion -> sweep - actúa como resta, sweep / como division
iris_norm <- sweep(datos[,1:4], 2, vector_minimos, "-")
iris_norm <- sweep(iris_norm, 2, vector_rango, "/")
iris_norm
iris_norm$Species <- iris$Species
iris_norm$esSetosa <- ifelse(iris_norm$Species == "setosa", 1, 0)

#La red no entiende de variables ni dataframes, hay que pasarlo todo a matrices
entradas <- as.matrix(iris_norm[,1:4])
salidas <- as.matrix(iris_norm$esSetosa)

#Funciones de activacion de las neuronas
#sigmoide me sirve para hacer la activacion de las neuronas -> convierte a 0 o 1 los valores
sigmoide <- function(x)1/(1+exp(-x))
#derivada sigmoide me permite conocer el error en la estimacion de la sigmoide
derivada_sigmoide <- function(x)x*(1-x)

#Neuronas. AQUI ESTAN LOS PESOS
set.seed(42)
capa1 <- matrix(runif(4*3, -1, 1), nrow=4, ncol=3) #las 4 columnas de entrada se relacionan con 3 neuronas cada una
capa2 <- matrix(runif(3, -1, 1), nrow=3, ncol=1) #las 3 neuronas de la capa anterior se relacionan con 1 columna (esSetosa)

#R lo que hace por detras seria valor neurona=(variable1*peso)+(variable2*peso)+(variable3*peso)+(variable4*peso)

#Aprendizaje
for(i in 1:100000){ #Se necesita valor grande para q pueda aprender, con valor pequeño no cambian los valores
    #Forward
    primera_capa <- sigmoide(entradas %*% capa1)
    salida <- sigmoide(primera_capa %*% capa2)
    #Error y back
    error <- salidas-salida
    derivada_salida <- error*derivada_sigmoide(salida)
    error_primera_capa <- derivada_salida %*% t(capa2)
    derivada_primera_capa <- error_primera_capa*derivada_sigmoide(primera_capa)
    capa2 <- capa2 + (t(primera_capa) %*% derivada_salida) * 0.1
    capa1 <- capa1 + (t(entradas) %*% derivada_primera_capa) * 0.1
}

prediccion <- ifelse(salida > 0.5, 1, 0)
print(table(Real = iris_norm$esSetosa, Prediccion = prediccion))

View(iris)
View(iris_norm)

"
Forward propagation: coge cada variable/dato de la flor los multiplica por los pesos, los suma, y aplica sigmoide
Entrada de datos -> primera capa de neuronas -> salida

Backpropagation: es donde aprende, calcula la diferencia entre el valor real y el estimado, aplica la derivada vuelve a la entrada y ajusta los pesos en base al error medido.
Entrada -> primera capa -> salida -> error
"

#VER DE MEJORARLA AÑADIENDO EL SESGO