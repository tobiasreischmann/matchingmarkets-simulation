## Start with the official rocker image (lightweight Debian)
FROM rocker/r-base:latest as r-java

MAINTAINER Gergely Daroczi <gergely.daroczi@card.com>

## gnupg is needed to add new key
RUN apt-get update && apt-get install -y gnupg2

## Install Java 
RUN apt-get update \
    && apt-get install -y openjdk-11-jdk \
    && update-alternatives --display java \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && R CMD javareconf

## make sure Java can be found in rApache and other daemons not looking in R ldpaths
RUN echo "/usr/lib/jvm/java-12-oracle/jre/lib/amd64/server/" > /etc/ld.so.conf.d/rJava.conf
RUN /sbin/ldconfig

## Install rJava package
RUN install2.r --error rJava \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

FROM r-java as matchingMarkets

RUN apt-get update && \
 apt install -y libgmp3-dev libblas-dev libopenblas-dev libxml2-dev && \
  export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH && \
  ln -s /usr/lib/x86_64-linux-gnu/libopenblas.so /usr/lib/x86_64-linux-gnu/libopenblas.so.3

RUN echo "install.packages(\"Rcpp\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"RcppProgress\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"lpSolve\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"partitions\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"RcppArmadillo\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"rlist\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"digest\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"ggplot2\", repos=\"https://cran.rstudio.com\")" | R --no-save

ADD /code /app 
RUN cd /app && \
  R CMD build --no-build-vignettes --no-manual . && \
  R CMD INSTALL .
ADD /matchingMarketsEvaluation /evaluation
RUN cd /evaluation && \
  R CMD build --no-build-vignettes --no-manual . && \
  R CMD INSTALL .

FROM matchingMarkets
ADD /run /opt/run
CMD R < /opt/run/run.R --vanilla
