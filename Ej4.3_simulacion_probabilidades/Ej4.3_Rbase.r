load("UniformSamples.RData")

#LARRY BIRD
#var lbx -> la x indica la posicion
var_lb1 <- paste0("LB1B", 1:5)
var_lb2 <- paste0("LB2B", 1:5)
var_lb3 <- paste0("LB3B", 1:5)
var_lb4 <- paste0("LB4B", 1:5)
var_lb5 <- paste0("LB5B", 1:5)
#Son las probabilidades de acierto desde cada posicion
probs_lb <- c(0.356, 0.376, 0.396, 0.376, 0.356)

#BALONES NORMALES
for (i in 1:5){
    UniformSamples[paste0("LBx", i, "B", 1:4)] <- mapply(
        function(v, p) ifelse(UniformSamples[[v]] <= p, 0, 1),
        v = get(paste0("var_lb", i)),
        p = probs_lb[i]
    )
}

#BALONES ESPECIALES
UniformSamples[paste0("LBx", 1:5, "B5")] <- mapply(
	function(v, p) ifelse(UniformSamples[[v]] <= p, 0, 2),
	v = var_lb5,
    p = probs_lb[1:5]
)

#CRAIG HODGES
var_ch1 <- paste0("CH1B", 1:5)
var_ch2 <- paste0("CH2B", 1:5)
var_ch3 <- paste0("CH3B", 1:5)
var_ch4 <- paste0("CH4B", 1:5)
var_ch5 <- paste0("CH5B", 1:5)
probs_ch <- c(0.364, 0.4, 0.405, 0.4, 0.364)

#BALONES NORMALES
for (i in 1:5){
    UniformSamples[paste0("CHx", i, "B", 1:4)] <- mapply(
        function(v, p) ifelse (UniformSamples[[v]] <= p, 0, 1),
        v = get(paste0("var_ch", i)),
        p = probs_ch[i]
    )
}
#BALONES ESPECIALES
UniformSamples[paste0("CHx", 1:5, "B5")]<-mapply(
	function(v, p) ifelse(UniformSamples[[v]] <= p, 0, 2),
	v = var_ch5,
    p = probs_ch[1:5]
)
#DEVIN BOOKER
var_db1 <- paste0("DB1B", 1:5)
var_db2 <- paste0("DB2B", 1:5)
var_db3 <- paste0("DB3B", 1:5)
var_db4 <- paste0("DB4B", 1:5)
var_db5 <- paste0("DB5B", 1:5)
probs_db <- c(0.329, 0.357, 0.385, 0.357, 0.329)

#BALONES NORMALES
for (i in 1:5){
    UniformSamples[paste0("DBx", i, "B", 1:4)] <- mapply(
        function(v, p) ifelse (UniformSamples[[v]] <= p, 0, 1),
        v = get(paste0("var_db", i)),
        p = probs_db[i]
    )
}

#BALONES ESPECIALES
UniformSamples[paste0("DBx", 1:5, "B5")] <- mapply(
    function(v, p) ifelse (UniformSamples[[v]] <= p, 0, 2),
    v = var_db5,
    p = probs_db[1:5]
)

#rep con each=5 lo repite 5 veces antes de pasar al sig
cols_lb <- paste0("LBx", rep(1:5, each = 5), "B", rep(1:5, times = 5))
cols_ch <- paste0("CHx", rep(1:5, each = 5), "B", rep(1:5, times = 5))
cols_db <- paste0("DBx", rep(1:5, each = 5), "B", rep(1:5, times = 5))

UniformSamples$z1 <- rowSums(UniformSamples[cols_lb])
UniformSamples$z2 <- rowSums(UniformSamples[cols_ch])
UniformSamples$z3 <- rowSums(UniformSamples[cols_db])

UniformSamples$Gana_LB <- ifelse(z1>z2 & z1>z3, TRUE, FALSE);
UniformSamples$Gana_CH <- ifelse(z2>z1 & z2>z3, TRUE, FALSE);
UniformSamples$Gana_DB <- ifelse(z3>z1 & z3>z2, TRUE, FALSE);
#Revisar la logica del empate
UniformSamples$Empate <- ifelse(z1==z2 | z2==z3 | z1==z3, TRUE, FALSE);

View(UniformSamples)