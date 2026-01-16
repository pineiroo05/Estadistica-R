#Clasifica flores si son o no setosas
library(neuralnet)
set.seed(42)
datos <- iris
vector_minimos <- apply(datos[,1:4], 2, min)
vector_maximos <- apply(datos[,1:4], 2, max)
vector_rango <- vector_maximos - vector_minimos

#Normalizacion: sweep - actua como resta, sweep / como division
iris_norm <- sweep(datos[,1:4], 2, vector_minimos, "-")
iris_norm <- sweep(iris_norm, 2, vector_rango, "/")

iris_norm$Species <- iris$Species
iris_norm$esSetosa <- ifelse(iris_norm$Species=="setosa", 1, 0)

#Para saber que tipo de flor es debe mirar en las variables del dataframe.
#neuralnet sirve para entrenar a la mini-red
#neuralnet(formula, data, hidden = 1, threshold = 0.01, stepmax = 1e+05, rep = 1, startweights = NULL, learningrate.limit = NULL, learningrate.factor = list(minus = 0.5, plus = 1.2), learningrate = NULL, lifesign = "none", lifesign.step = 1000, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic", linear.output = TRUE, exclude = NULL, constant.weights = NULL, likelihood = FALSE)
red <- neuralnet(esSetosa~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
            data=iris_norm, 
            hidden=c(2,3), 
            linear.output = FALSE)
plot(red)

output <- compute(red, iris_norm[1:4])
predicciones <- output$net.result
resultado <- ifelse(predicciones[,1] > 0.5, 1, 0)

print(resultado[1:150])
tabla <- table(Real = iris_norm$esSetosa, Prediccion = resultado)
print(tabla)
View(iris)
View(iris_norm)

#[1] -> accede a las filas // [,1] -> accede a las columnas