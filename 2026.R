rotar <- function(angulo){
    text(x=7,y=2,labels="¡¡¡FELIZ AÑO NUEVO!!!",cex=3,font=2,col="orange",srt=angulo)
}

dibujar_2026 <- function(angulo){
    plot(0, 0, type="n", xlim=c(0,16), ylim=c(-2,2))
    rotar(angulo)
    #Pintar primer 2
    t <- seq(0,pi,length.out=100)
    lines(cos(t)+2,sin(t)+0.5,lwd=3,col="red") #Genera el semicirculo del 2
    lines(c(3,1),c(0.5,-1.5),lwd=3,col="red") #Diagonal
    lines(c(3,1),c(-1.5,-1.5),lwd=3,col="red") #Base
    #Pintar 0
    t <- seq(0,2*pi,length.out=100)
    lines(cos(t)+6,sin(t)*1.5,lwd=3,col="red")
    #Pintar segundo 2
    t <- seq(0,pi,length.out=100)
    lines(cos(t)+10,sin(t)+0.5,lwd=3,col="red")
    lines(c(11, 9), c(0.5, -1.5), lwd = 3, col = "red")
    lines(c(9, 11), c(-1.5, -1.5), lwd = 3, col = "red")
    #Pintar 6
    t_base <- seq(0,2*pi,length.out=100)
    lines(cos(t_base)+14,sin(t_base)-0.5,lwd=3,col="red")
    t_superior <- seq(pi/2,pi,length.out=100)
    lines(cos(t_superior)*2+15,sin(t_superior)*1.5+0.25,lwd=3,col="red")
}

for(i in seq(0,360,by=5)){
    dev.hold()
    dibujar_2026(i)
    dev.flush()
    Sys.sleep(0.05)
}