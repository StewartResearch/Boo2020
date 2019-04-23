
pLambda<-function(Herd, Title = ""){      #plot mean Lambda  Example:  pLambda(clawrmc, "CLAWR")
  Lamb.CI <- apply(Herd$Lambda, 2, function(x){
    c(quantile(x,0.05),  mean(x), quantile(x, 0.95))
  })
  PP<-function(x = Lambda){
    Y1<-1837
    x0<-seq(1,100)
    plot(x = 0, y = 0,
         xlim=c(1,ncol(x))+Y1,ylim=range(x),
         xlab="", ylab="",
         cex.axis = 1.2)
    polygon(c(x0+Y1, rev(x0+Y1)), c(x[1,x0], rev(x[3,x0])), col = "lightgray")
    lines((x0+Y1),x[2,x0], type="l",lwd=3, col = "black")
    x1<-seq(100,170)
    polygon(c(x1+Y1, rev(x1+Y1)), c(x[1,x1], rev(x[3,x1])), col = "cornflowerblue")
    lines(x1+Y1,x[2,x1],col="darkblue",lwd=3)
    x2<-seq(170,ncol(x))
    polygon(c(x2+Y1, rev(x2+Y1)), c(x[1,x2], rev(x[3,x2])), col = "brown1")
    lines(x2+Y1,x[2,x2],col="darkred",lwd=3)
  }
  PP(Lamb.CI)
  title(main = Title, xlab = "Year", ylab = "Lambda",
        font.main = 2, font.lab = 4,
        col.main = "blue", col.lab = "blue",
        cex.lab = 1.3, cex.main = 1.4)
}

