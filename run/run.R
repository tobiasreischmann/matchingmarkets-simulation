
# ----------------------------------------------------------------------------
# R-code which runs the different configurations and stores them for later use
# by the output functions. This is usefull if the expensive calculation should
# be done on a different machine than the creation of the output.
#
# Copyright (c) 2019 Tobias Reischmann
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file LICENSE
#
# ----------------------------------------------------------------------------

require(matchingMarketsEvaluation)
require(matchingMarkets)
require(parallel)

generate <- function(configuration) {
  cl <- call(configuration)
  eval(cl)
  calculateScenarios(rows)
}

generate("horizontal_configuration")
generate("occupancy_configuration")
generate("programmes_configuration")
generate("quota_configuration")
generate("threshold_configuration")
generate("size_configuration")
generate("tiers_configuration")
generate("rankinglength_configuration")
