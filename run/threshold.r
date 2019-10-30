require(matchingMarkets)
require(parallel)

########## Config ##################
nworkers = detectCores()
nruns = 10 # Number of runs to average over
png("threshold.png", 480, 360)
par(xpd = FALSE) 
plot(NULL, xlim=c(1,length(thresholds)),ylim = c(0,15), xaxt = 'n', yaxt = 'n', xlab = 'Threshold', ylab = 'Iterations')
axis(side=1, at=c(1:length(thresholds)), labels = thresholds, col = NA, col.ticks = 1)
axis(side=2, at=(0:7)*2, labels = (0:7)*2, col = NA, col.ticks = 1)
abline(h=6,col='red')

for (scenario in c(2,3)) {  
  cat("Starting scenario ", scenario)
  
  ### Run 200 - 10
  if (scenario == 1) {
    occupancyrates = c(1,1.2) # Ratio between nStudents and number of places
    quotas <- 1:10/10
    nStudents <- 200 # Number of students 
    nColleges <- 10 # Number of programs over all facilities
    areasize <- 2 # Area size for horizontal distribution
    conf.s.prefs.count <- list(withTies=c(1,2,3),withoutTies=rep(1,6)) # Number/Ties of preferences of students
  }
  
  ### Run 600 - 200
  if (scenario == 2) {
    quotas <- 1:10/10
    occupancyrates = c(0.8,1,1.2,1.5,2) # Ratio between nStudents and number of places
    nStudents <- 600 # Number of students
    nColleges <- 200 # Number of programs over all facilities
    areasize <- 6 # Area size for horizontal distribution
    conf.s.prefs.count <- list(withTies=c(2,5,6,7),withoutTies=rep(1,20)) # Number/Ties of preferences of students
  }
  
  ### Run 2700 - 580
  if (scenario == 3) {
    quotas <- c(0.1,0.3,0.5,0.8,1)
    occupancyrates = c(1.2) # Ratio between nStudents and number of places
    nStudents <- 2700 # Number of students
    nColleges <- 580 # Number of programs over all facilities
    areasize <- 7 # Area size for horizontal distribution
    horizontalscenarios <- c(0)
    conf.s.prefs.count <- list(withTies=c(3,7,10,10)) # Number/Ties of preferences of students
  }
  
  ### Run 6000 - 1000
  if (scenario == 4) {
    quotas <- c(0.2,0.5,0.8)
    occupancyrates = c(1,1.2) # Ratio between nStudents and number of places
    nStudents <- 5000 # Number of students
    nColleges <- 1000 # Number of programs over all facilities
    areasize <- 100 # Area size for horizontal distribution
    conf.s.prefs.count <- list(withTies=c(3,7,10,10)) # Number/Ties of preferences of students
  }
  
  results<-array(0,dim=c(length(quotas),length(horizontalscenarios),length(conf.s.prefs.count),length(occupancyrates)),
                 dimnames = list(paste("quota ", quotas), 
                                 paste("c.prefs - Scenario", horizontalscenarios+1), 
                                 names(conf.s.prefs.count),
                                 paste("Occupancy", occupancyrates)
                 )
  )
  
  equaldist <- function(x) {
    runif(x)
  }
  
  category <- function(c) {
    function(x) {
      round(runif(x) * c + 0.5)
    }
  }
  
  thresholds <- c(0.001,0.002,0.005,0.01,0.02,0.05,0.1)
  
  ######### Run ##############
  totalruns <- length(occupancyrates) * 3 * length(conf.s.prefs.count) * length(quotas)
  starttime <- Sys.time()
  timediff <- NULL
  runscompleted <- 0
  applyresults <- mclapply(1:length(occupancyrates), function(o) { # Loop over occupancy rates
    occupancy = occupancyrates[o]
    mean <- (nStudents/nColleges)/occupancy # Mean number of places per program
    sd <- mean/2 # Standard deviation for distribution of places per program
    resultj <- mclapply(1:length(horizontalscenarios), function(j) { # Loop over horizontal preference exclusions
      j <- horizontalscenarios[j]
      resultr <- mclapply(1:length(conf.s.prefs.count), function(r) { # Loop over Ties vs No Ties
        s.prefs.count = conf.s.prefs.count[[r]]
        resulti <- mclapply(1:length(quotas), function(i) { # Loop over different private quotas
          
          ## calculate time
          if (is.null(timediff)) {
            #print(paste("Estimated finish time:", "Unkown"))
            timediff <- 0
          } else {
            #print(paste("Completed: ", runscompleted, '/', totalruns, sep=""))
            #print(paste("Estimated finish time:", Sys.time() + (totalruns - runscompleted) * timediff))
          }
          ## end calculate time
          
          quota <- quotas[i]
          capacityfun <- function(n, mean, sd=1) {
            sapply(round(rnorm(n, mean=mean, sd=sd)), function(x) max(1,x))
          }
          nSlots <- capacityfun(nColleges, mean, sd)
          
          private <- function(x) {
            runif(x) < quota
          }
          #capture.output(
          daresult <- stabsim3(m=nruns, nStudents=nStudents, nSlots=nSlots, verbose=FALSE, 
                               colleges = c("cx","cy", "firstpref", "secondpref", "quality", "cidiocat", "cidio1", "cidio2", "private"), 
                               students = c("sx", "sy", "social", "sidiocat", "idist", "iidio", "sidio1", "sidio2", "iquality"),
                               colleges_fun = c(category(areasize),category(areasize),category(3),category(2),equaldist,category(10),equaldist,equaldist,private), 
                               students_fun = c(category(areasize),category(areasize),category(100),category(10),equaldist,equaldist,equaldist,equaldist,equaldist),
                               outcome = ~ I(sqrt(((cx-sx)/areasize)**2 + ((cy-sy)/areasize)**2)), 
                               selection = c(
                                 student = ~ I((1000**(firstpref %% 3)) * ((private + j) < 2) * (abs(cx-sx)<=1) * (abs(cy-sy)<=1)) 
                                 + I((1000**((firstpref + secondpref) %% 3)) * social)
                                 + I((1000**((firstpref - secondpref) %% 3)) * private * ceiling((cidio1 + sidio1 %% 1) * 100))
                                 #+ I((1000**((firstpref - secondpref) %% 3)) * private * (cidiocat == sidiocat) )
                                 ,
                                 colleges = ~ I(-idist * 2 * sqrt(((cx-sx))**2 + ((cy-sy))**2)/areasize)
                                 + I(iquality * quality) 
                                 + I(iidio * (cidiocat == sidiocat))
                               ),
                               private_college_quota = quota,
                               count.waitinglist = function(x) {x}, s.prefs.count = s.prefs.count)
          #, file = 'NUL')
          sapply(1:nColleges, function(x){
            sum(sapply(1:nStudents, function(y){x %in% daresult$s.prefs[[1]][,y]})
            )})
          sapply(1:nColleges, function(x) {nSlots[x] - sum(daresult$OUT[[1]]$c.id == x)})
          
          sum(nSlots) - length(daresult$OUT[[1]]$s.id)
          nStudents - length(daresult$OUT[[1]]$s.id)
          curr <- array(0,length(thresholds))
          
          for (iterator in 1:length(thresholds)) {
            threshold <- thresholds[iterator]
            for (m in 1:nruns) { # Average results
              iteration <- daresult$iterations[[m]]
              iterationtable <- t(as.matrix(iteration[,-1]))
              #bp <- barplot(iterationtable, main=paste("Developme  nt of held offers over number of iterations \n(private quota=",quota,")\n(private distance=",j==0,")"),
              #            xlab="Number of iterations", col=c("darkblue","darkgreen","brown"),
              #            legend.text = row.names(iterationtable),ylim=c(0,1)) 
              #axis(side = 1, at = bp, tick = FALSE, labels=seq(iterationtable[1,]))
              complete <- sum(iterationtable[,1])
              ratio <- complete * threshold
              curr[iterator] <- curr[iterator] + sum(iteration$new+iteration$altered > ratio) + 1
            }
          }
          
          result <- curr/nruns
          varj <- j+1
          #    print(paste("Results at [",i,",",varj,",",r,",",o,"] = ", result))
          write('.', file="", append=TRUE)
          # saveRDS(daresult,paste("daresults.","scenario-",scenario,".",nStudents,"-",nColleges,".",i,".",j,".",r,".",o,".rds", sep=""))
          rm(daresult)
          gc()
          return(result)
          
          ## calculate time
          endtime <- Sys.time()
          timediff.temp <- endtime - starttime
          timediff <- ((timediff * runscompleted) + timediff.temp) / (runscompleted + 1)
          runscompleted <- runscompleted + 1
          ## end calculate time
          
        }, mc.cores=nworkers)
        return(resulti)
      }, mc.cores=nworkers)
      return(resultr)
    }, mc.cores=nworkers)
    return(resultj)
  }, mc.silent=FALSE, mc.cores=nworkers)
  
  for (o in c(1:length(applyresults))) {
    oval <- applyresults[[o]]
    for (j in c(1:length(oval))) {
      jval <- oval[[j]]
      for (r in c(1:length(jval))) {
        rval <- jval[[r]]
        for (i in c(1:length(rval))) {
          if (is.null(rval[[i]])) {
            results[i,j,r,o] <- array()
            print("Empty result occured")
          } else {
            lines(rval[[i]])
            #results[i,j,r,o] <- as.list(rval[[i]])
          }
        }
      }
    }
  }
  dev.off()
  print(results)
  saveRDS(results,paste("results.","scenario-",scenario,".",nStudents,"-",nColleges,".rds", sep=""))
}
#test <- readRDS("../run/daresults/daresults.scenario-3.2700-580.1.0.1.2.rds")
#scen4 <- readRDS("../run/results.scenario-4.5000-1000.rds")
