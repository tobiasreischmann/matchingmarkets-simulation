#!/bin/sh

# Install required packages
echo "install.packages(\"rJava\", repos=\"https://cran.rstudio.com\")" | R --no-save
echo "install.packages(\"Rcpp\", repos=\"https://cran.rstudio.com\")" | R --no-save
echo "install.packages(\"RcppProgress\", repos=\"https://cran.rstudio.com\")" | R --no-save
echo "install.packages(\"lpSolve\", repos=\"https://cran.rstudio.com\")" | R --no-save
echo "install.packages(\"partitions\", repos=\"https://cran.rstudio.com\")" | R --no-save
echo "install.packages(\"RcppArmadillo\", repos=\"https://cran.rstudio.com\")" | R --no-save 
echo "install.packages(\"rlist\", repos=\"https://cran.rstudio.com\")" | R --no-save
echo "install.packages(\"digest\", repos=\"https://cran.rstudio.com\")" | R --no-save

# Install matchingMarkets
cd code
R CMD build --no-build-vignettes --no-manual .
R CMD INSTALL .

# Install matchingMarketsEvaluation
cd ../matchingMarketsEvaluation
R CMD build --no-build-vignettes --no-manual .
R CMD INSTALL .
cd ..
