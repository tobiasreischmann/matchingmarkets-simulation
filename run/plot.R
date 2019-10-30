daresult <- readRDS("../run/daresults/daresults.scenario-3.2700-580.3.2.1.2.rds")
m <-1
quota <- '80%'
y <- 2700

iteration <- daresult$iterations[[m]]
iterationtable <- t(as.matrix(iteration[,-1]))
complete <- sum(iterationtable[,1])

rm(daresult)
rm(iteration)
gc()

png("../run/plots/analysis.png", 480, 360)

colors=c("darkgreen","darkblue","brown")
par(mfrow=c(1, 1), mar=c(4, 4, 3, 3),xpd=TRUE)
bp <- barplot(iterationtable * y,main=paste("Matching after X iterations in comparison to full run \n(private quota=",quota,")"),
            xlab="Number of iterations", col=colors,ylim=c(0,y),yaxt='n',ylab='Places')
legend(x=bp[8]*1.03,y=y*.3, row.names(iterationtable), fill=colors, bg='white')
axis(side = 1, at = bp, tick = FALSE, labels=seq(iterationtable[1,]))
axis(2,at=c(0,450,900,1350,1800,2250,2700),labels=TRUE)
xend = (bp[11]+.7)
lines(c((bp[1]-.7),xend),rep(y*complete*0.95,2))
segments(xend,y*complete*0.95,xend,y*complete)
segments(bp[11],y*complete,xend,y*complete)
segments(xend,y*complete*.975,xend+.1,y*complete*.975)
text(bp[11]+1.2,2700*complete*0.975, '5%')

dev.off()
        
  