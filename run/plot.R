library(ggplot2)
#daresult <- readRDS("../run/daresults/daresults.scenario-3.2700-580.3.2.1.2.rds")
daresult <- readRDS("../run/daresults/daresults.scenario-6.2000-600.rds")
m <-1
quota <- '80%'
y <- 2000

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
legend(x=bp[8]*1.03,y=y*.3, row.names(iterationtable), fill=colors, bg='white', reverse=T)
axis(side = 1, at = bp, tick = FALSE, labels=seq(iterationtable[1,]))
axis(2,at=c(0,450,900,1350,1800,2250,2700),labels=TRUE)
xend = (bp[11]+.7)
lines(c((bp[1]-.7),xend),rep(y*complete*0.95,2))
segments(xend,y*complete*0.95,xend,y*complete)
segments(bp[11],y*complete,xend,y*complete)
segments(xend,y*complete*.975,xend+.1,y*complete*.975)
text(bp[11]+1.2,2700*complete*0.975, '5%')

dev.off()

iteration <- as.vector(unlist(lapply(1:11,function(x){rep(x,3)})))
type <- as.vector(rep(rownames(iterationtable),11))
values <- as.vector(iterationtable*y)
data <- data.frame(iteration,type,values)

ggplot(data=data, aes(fill=factor(type, levels=c("new","altered","unchanged")), x=iteration, y=values)) +
  geom_bar(position="stack", stat="identity", width=.8)  +
  scale_fill_grey(start = 0.45, end = 0.8, labels = c("unmatched","temporary","final")) +
  xlab("Number of iterations") +
  ylab("Places") +
  #ggtitle("Matching after X iterations in comparison to a full DA run") +
  #labs(subtitle=paste("(share of private facilities =",quota,")")) +
  guides(fill=guide_legend(title="State of assignments")) +
  theme_classic() + 
  scale_x_continuous(breaks=1:11) + 
  scale_y_continuous(breaks=c(0,400,800,1200,1600,2000)) + 
  #geom_abline(slope=0, intercept=y*complete*.95,  col = "black",lty=2) +
  geom_segment(mapping=aes(x=-1, y=y*complete*.95, xend=11.7, yend=y*complete*.95), color="black",lty="22",size=.25) +
  geom_segment(mapping=aes(x=11.7, y=y*complete, xend=11.7, yend=y*complete*.95), color="black",lty="22",size=.25) +
  geom_segment(mapping=aes(x=-1, y=y*complete-2, xend=11.7, yend=y*complete-2), color="black",lty="22",size=.25) +
  geom_segment(mapping=aes(x=11.7, y=y*complete*.975, xend=11.85, yend=y*complete*.975), color="black",lty="22",size=.25) +
  annotate("text",x=12.15,y=y*complete*.975,label="5%", size=3, fontface="plain") +
  coord_cartesian(xlim=c(0.7,11.8)) +
  theme(
    legend.position = c(.875, .1),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
ggsave("../run/plots/analysis2.pdf", scale=.85, width=7, height = 5)

