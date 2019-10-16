FROM cardcorp/r-java as matchingMarkets

RUN apt-get update && \
 apt install -y libgmp3-dev libblas-dev libopenblas-dev libxml2-dev && \
  export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH && \
  ln -s /usr/lib/x86_64-linux-gnu/libopenblas.so /usr/lib/x86_64-linux-gnu/libopenblas.so.3

RUN echo "install.packages(\"Rcpp\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"RcppProgress\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"lpSolve\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"partitions\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"RcppArmadillo\", repos=\"https://cran.rstudio.com\")" | R --no-save && \
 echo "install.packages(\"rlist\", repos=\"https://cran.rstudio.com\")" | R --no-save

ADD /code /app 
RUN cd /app && \
  R CMD build --no-build-vignettes --no-manual . && \
  R CMD INSTALL .

FROM matchingMarkets
ADD /run /opt/run
CMD R < /opt/run/formula.r --vanilla
