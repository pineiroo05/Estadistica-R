load("UniformSamples.RData")

obtenerPuntuacion <- function(jugador, probAc, UniformSamples){
    #Balones normales (Del 1º al 4º, 1 punto)
    normales <- lapply(1:5, function(i){ #aplica la funcion, pero devuelve una lista. la tengo que convertir en matriz para luego pegarla a la original
        vars_norm <- paste0(jugador, i, "B", 1:4)
        mapply(
            function(v) ifelse(UniformSamples[[v]]<=probAc[i], 0, 1),
            v = vars_norm
        )
    })
    normales <- do.call(cbind, normales)
    colnames(normales) <- paste0(jugador, "x", rep(1:5, each=4), "B", 1:4)

    #Balones especiales (El 5º, 2 puntos)
    especiales <- lapply(1:5, function(i){
        vars_esp <- paste0(jugador, i, "B", 5)
        mapply(
            function(v) ifelse(UniformSamples[[v]]<=probAc[i], 0, 2),
            v = vars_esp
        )
    })
    especiales <- do.call(cbind, especiales)
    colnames(especiales) <- paste0(jugador, "x", 1:5, "B5")

    #cbind -> combina matrices por columnas // rbind -> combina matrices por filas
    puntuacion_total <- cbind(normales, especiales)
    return (puntuacion_total)
}

#LARRY BIRD
var_lb <- paste0("LB", 1:5, "B", 1:5)
probs_lb <- c(0.356, 0.376, 0.396, 0.376, 0.356)

puntuacion_LB <- obtenerPuntuacion("LB", probs_lb, UniformSamples)
UniformSamples <- cbind(UniformSamples, puntuacion_LB)

#CRAIG HODGES
var_ch <- paste0("CH", 1:5, "B", 1:5)
probs_ch <- c(0.364, 0.4, 0.405, 0.4, 0.364)

puntuacion_CH <- obtenerPuntuacion("CH", probs_ch, UniformSamples)
UniformSamples <- cbind(UniformSamples, puntuacion_CH)

#DEVIN BOOKER
var_db <- paste0("DB", 1:5, "B", 1:5)
probs_db <- c(0.329, 0.357, 0.385, 0.357, 0.329)

puntuacion_DB<-obtenerPuntuacion("DB", probs_db, UniformSamples)
UniformSamples <- cbind(UniformSamples, puntuacion_DB)

#Accede al valor de la columna
cols_lb <- colnames(puntuacion_LB)
cols_ch <- colnames(puntuacion_CH)
cols_db <- colnames(puntuacion_DB)

UniformSamples$Puntos_LB <- rowSums(UniformSamples[cols_lb])
UniformSamples$Puntos_CH <- rowSums(UniformSamples[cols_ch])
UniformSamples$Puntos_DB <- rowSums(UniformSamples[cols_db])

z1 <- UniformSamples$Puntos_LB
z2 <- UniformSamples$Puntos_CH
z3 <- UniformSamples$Puntos_DB

UniformSamples$Gana_LB <- ifelse(z1>z2 & z1>z3, TRUE, FALSE);
UniformSamples$Gana_CH <- ifelse(z2>z1 & z2>z3, TRUE, FALSE);
UniformSamples$Gana_DB <- ifelse(z3>z1 & z3>z2, TRUE, FALSE);

UniformSamples$Empate <- ifelse(z1==z2 | z2==z3 | z1==z3, TRUE, FALSE);

View(UniformSamples)