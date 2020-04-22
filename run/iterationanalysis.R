require(matchingMarkets)
require(matchingMarketsEvaluation)
require(parallel)
  
########## Config ##################  
nworkers = 1#detectCores()
nruns = 1 # Number of runs to average over

cat("Starting general iteration scenario")
  
### Small element scenario
elem1 <- list(occupancyrate = 1.2, quota = .6, nStudents = 3000, nColleges = 600, 
                areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = 0.05)
elements <- list(elem1)
rows <<- lapply(elements, function(elem) {
    lapply(c(1), function(x){
      elem
    })
  })
dimensionx <- "Iterations"
  
equaldist <- function(x) {
  runif(x)
}
  
category <- function(c) {
  function(x) {
    round(runif(x) * c + 0.5)
  }
}
  
applyresults <- calculateScenarios(rows,nruns,nworkers,123,fullresult=TRUE)
    
library(ggplot2)
m <-1
quota <- '60%'
y <- 3000

iteration <- applyresults[[1]][[1]]$iterations[[m]]
iterationtable <- t(as.matrix(iteration[,-1]))
complete <- sum(iterationtable[,1])

entries <- length(iteration[,1])

rm(applyresults)
rm(iteration)
gc()

iteration <- as.vector(unlist(lapply(1:entries,function(x){rep(x,3)})))
type <- as.vector(rep(rownames(iterationtable),entries))
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
  scale_x_continuous(breaks=1:entries) +
  scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500,3000)) +
  #geom_abline(slope=0, intercept=y*complete*.95,  col = "black",lty=2) +
  geom_segment(mapping=aes(x=-1, y=y*complete*.95, xend=entries + .7, yend=y*complete*.95), color="black",lty="22",size=.25) +
  geom_segment(mapping=aes(x=entries + .7, y=y*complete, xend=entries + .7, yend=y*complete*.95), color="black",lty="22",size=.25) +
  geom_segment(mapping=aes(x=-1, y=y*complete-2, xend=entries + .7, yend=y*complete-2), color="black",lty="22",size=.25) +
  geom_segment(mapping=aes(x=entries + .7, y=y*complete*.975, xend=entries + .85, yend=y*complete*.975), color="black",lty="22",size=.25) +
  annotate("text",x=entries + 1.15,y=y*complete*.975,label="5%", size=3, fontface="plain") +
  coord_cartesian(xlim=c(0.7,entries + .8)) +
  theme(
    legend.position = c(.875, .1),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
ggsave("./plots/iterationanalysis.pdf", scale=.85, width=7, height = 5)


