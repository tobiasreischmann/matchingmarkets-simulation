require(matchingMarkets)
require(parallel)
  
########## Config ##################
nworkers = detectCores()
nruns = 10 # Number of runs to average over
load <- FALSE

## Common function

percent <- function(x, digits = 0, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

for (scenario in c(5)) {  
  cat("Starting scenario ", scenario)
  
  ### Run 200 - 10
  if (scenario == 1) {
    elem1 <- list(occupancyrate = 1, quota = 1, nStudents = 2700, nColleges = 600, 
                  areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1, threshold = 0.05)
    occupancyrates = c(1,1.2) # Ratio between nStudents and number of places
    quotas <- 1:10/10
    nStudents <- 200 # Number of students 
    nColleges <- 10 # Number of programs over all facilities
    areasize <- 2 # Area size for horizontal distribution
    conf.s.prefs.count <- list(withTies=c(1,2,3),withoutTies=rep(1,6)) # Number/Ties of preferences of students
    elements = c(elem1)
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
  
  ### First element scenario
  if (scenario == 5) {
    dimensionxval <- c(0.07,0.05,0.02,0.01,0.005,0.002,0.001)
    dimensionxlabels <- percent(dimensionxval, digits = 1)
    
    elem1 <- list(occupancyrate = 1.2, quota = .6, nStudents = 2700, nColleges = 600, 
                  areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
    elem2 <- list(occupancyrate = .8, quota = .6, nStudents = 2700, nColleges = 600, 
                  areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
    elem5 <- list(occupancyrate = 1.2, quota = .6, nStudents = 600, nColleges = 200, 
                  areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
    elem3 <- list(occupancyrate = 1.2, quota = .3, nStudents = 2700, nColleges = 600, 
                  areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
    elem4 <- list(occupancyrate = .8, quota = .3, nStudents = 2700, nColleges = 600, 
                  areasize = 7, conf.s.prefs = c(3,7,10,10), horizontalscenario = 1)
    elem6 <- list(occupancyrate = .8, quota = .3, nStudents = 600, nColleges = 200, 
                  areasize = 6, conf.s.prefs = c(2,5,6,7), horizontalscenario = 1)
    elements <- list(elem1, elem2, elem3, elem4, elem5, elem6)
    rows <- lapply(elements, function(elem) {
        lapply(dimensionxval, function(x){
          elem$threshold <- x
          elem
        })
    })
    dimensionx <- "threshold"
    relevantForLegend <- c("occupancyrate", "quota", "nStudents", "nColleges")
    translationsForLegend <- list("occupancyrate"="Occupancy rate:", "quota" = "Private quota:", "nStudents" = "#Children:", "nColleges" = "#Programmes:  ")
    translationsForResults <- list("occupancyrate"=identity, "quota" = percent, "nStudents" = identity, "nColleges" = identity, "threshold" = percent)
    }
  
  equaldist <- function(x) {
    runif(x)
  }
  
  category <- function(c) {
    function(x) {
      round(runif(x) * c + 0.5)
    }
  }
  
  if (!load) {
    ######### Run ##############
    applyresults <- lapply(rows, function(elements) {
      rowresults <- mclapply(elements, function(elem) { # Loop over elements
        occupancy <- elem$occupancyrate
        nStudents <- elem$nStudents
        nColleges <- elem$nColleges
        threshold <- elem$threshold
        areasize <- elem$areasize
        mean <- (nStudents/nColleges)/occupancy # Mean number of places per program
        sd <- mean/2 # Standard deviation for distribution of places per program
        j <- elem$horizontalscenario
        s.prefs.count = elem$conf.s.prefs
        quota <- elem$quota
        capacityfun <- function(n, mean, sd=1) {
          sapply(round(rnorm(n, mean=mean, sd=sd)), function(x) max(1,x))
        }
        nSlots <- capacityfun(nColleges, mean, sd)
        
        private <- function(x) {
          runif(x) < quota
        }
    
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
        sapply(1:nColleges, function(x){
          sum(sapply(1:nStudents, function(y){x %in% daresult$s.prefs[[1]][,y]})
          )})
        sapply(1:nColleges, function(x) {nSlots[x] - sum(daresult$OUT[[1]]$c.id == x)})
        
        sum(nSlots) - length(daresult$OUT[[1]]$s.id)
        nStudents - length(daresult$OUT[[1]]$s.id)
        curr <- 0
        
        for (m in 1:nruns) { # Average results
          iteration <- daresult$iterations[[m]]
          iterationtable <- t(as.matrix(iteration[,-1]))
          complete <- sum(iterationtable[,1])
          ratio <- complete * threshold
          curr <- curr + sum(iteration$new+iteration$altered > ratio) + 1
        }
        
        result <- curr/nruns
        varj <- j+1
        #    print(paste("Results at [",i,",",varj,",",r,",",o,"] = ", result))
        write('.', file="", append=TRUE)
        # saveRDS(daresult,paste("daresults.","scenario-",scenario,".",nStudents,"-",nColleges,".",i,".",j,".",r,".",o,".rds", sep=""))
        rm(daresult)
        gc()
        return(result)
    
      }, mc.silent=FALSE, mc.cores=nworkers)
    })
    
    saveRDS(applyresults,paste("evaluations.","scenario-",scenario,".rds", sep=""))
  }
  if (load) {
    applyresults <- readRDS(paste("evaluations.","scenario-",scenario,".rds", sep=""))
  }
  
  #Initialize Plot
  pdf("threshold.pdf", width=8, height=6)
  par(mfrow=c(1, 1), mar=c(4, 4, 3, 3),xpd=FALSE)
  plot(NULL, xlim=c(1,length(dimensionxval)),ylim = c(0,15), xaxt = 'n', yaxt = 'n', xlab = dimensionx, ylab = 'Iterations')
    axis(side=1, at=c(1:length(dimensionxval)), labels = dimensionxlabels, col = NA, col.ticks = 1)
  axis(side=2, at=(0:7)*2, labels = (0:7)*2, col = NA, col.ticks = 1)
  abline(h=6,col='red')
  
  colors = colors()[c(73,74,139,116,143, 50)]
  
  
  for (i in c(1:length(elements))) {
    if (is.null(applyresults[[i]])) {
      #results[i,j,r,o] <- array()
      print("Empty result occured")
    } else {
      lines(unlist(applyresults[[i]]), pch = 1, type = "b", lty = i, col=colors[i])
    }
  }
  legendentries <- lapply(elements, function(elem) {
    entries = c()
    for (dim in relevantForLegend) {
      entry <- paste0(c(translationsForLegend[[dim]], translationsForResults[[dim]](elem[[dim]])), collapse=" ")
        entries <- cbind(entries, entry)
    }
    paste(entries, collapse=", ")
  })
  rowsrange <- 1:length(elements)
  legend('topright', legend = legendentries, col = colors[rowsrange], lty = rowsrange, pch = 1)
    
  dev.off()
  print(applyresults)
}
#test <- readRDS("../run/daresults/daresults.scenario-3.2700-580.1.0.1.2.rds")
#scen4 <- readRDS("../run/results.scenario-4.5000-1000.rds")

          