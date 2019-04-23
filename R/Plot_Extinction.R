ExtProb<-function(Herd, Title = ""){       #plot extinction probability ExtProb(clawrmc, "CLAWR Pr(Extinction)")
  
  PP<-function(x){
    Y1<-1837
    x0<-seq(1,100)
    plot((x0+Y1),x[x0],type="l",lwd=3,
         xlim=c(1,length(x))+Y1,ylim=c(0,1),
         xlab = "", ylab = "",
         cex.axis = 1.2)
    x1<-seq(101,170)
    lines(x1+Y1,x[x1],col="blue",lwd=3)
    x2<-seq(171,length(x))
    lines(x2+Y1,x[x2],col="red",lwd=3)
  }
  
  
  PP(apply(Herd$Nt,2,function(x, N = 10)
    sum(x<10)/length(x)))  #Extinction Probability
  title(main = Title, xlab = "Year", ylab = "Prob. of Extinction",
        font.main = 2, font.lab = 4,
        col.main = "blue", col.lab = "blue",
        cex.lab = 1.3, cex.main = 1.4)
}
