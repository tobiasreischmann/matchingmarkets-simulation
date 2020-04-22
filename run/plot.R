require(parallel)
require(matchingMarkets)
require(matchingMarketsEvaluation)
require(tikzDevice)

generate <- function(configuration, label, relevantForLegend = NULL,maxy=14) {
  cl <- call(configuration)
  eval(cl)
  data <- calculateScenarios(rows)
  for (i in 1:length(data)) {
    for (j in 1:length(data[[i]])) {
      if (!is.numeric(data[[i]][[j]]) && !is.null(data[[i]][[j]])) {
        data[[i]][j] <- list(NULL)
      }
    }
  }
  plotEvaluation(data, elements, label, dimensionxval, relevantForLegend,maxy)
}

p1 <- generate("occupancy_configuration", "Occupancy rate", c("quota", "nStudents", "nColleges"))
p2 <- generate("quota_configuration", "Share of private facilities", c("occupancyrate", "nStudents", "nColleges"),maxy=14)
p3 <- generate("tiers_configuration", "Tiers", c("occupancyrate", "quota", "nStudents", "nColleges"))
p4 <- generate("threshold_configuration", "Threshold", c("occupancyrate", "quota", "nStudents", "nColleges"),maxy=18)
p5 <- generate("programmes_configuration", "Number of applicants", c("occupancyrate", "quota", "nColleges"))
p6 <- generate("rankinglength_configuration", "Length of childrens ranking lists", c("occupancyrate", "quota", "nStudents", "nColleges"),maxy=18)
p7 <- generate("size_configuration", "Market size (no. of applications / no. of programmes)", c("occupancyrate", "quota"),maxy=18)
p8 <- generate("horizontal_configuration", "Preference Scenario", c("occupancyrate", "quota", "nStudents", "nColleges", "conf.s.prefs"),maxy=28)

pdf("plot1.pdf",width = 6, height = 4, pointsize = 14)
p1
dev.off()
pdf("plot2.pdf",width = 6, height = 4, pointsize = 14)
p2
dev.off()
pdf("plot3.pdf",width = 6, height = 4, pointsize = 14)
p3
dev.off()
pdf("plot4.pdf",width = 6, height = 4, pointsize = 14)
p4
dev.off()
pdf("plot5.pdf",width = 6, height = 4, pointsize = 14)
p5
dev.off()
pdf("plot6.pdf",width = 6, height = 4, pointsize = 14)
p6
dev.off()
pdf("plot7.pdf",width = 6, height = 4, pointsize = 14)
p7
dev.off()
pdf("plot8.pdf",width = 6, height = 4, pointsize = 14)
p8
dev.off()
