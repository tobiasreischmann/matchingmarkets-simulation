# MatchingMarkets Simulation

This package includes scripts to run and reproduce multiple simulations for different childcare markets.

To build the files you can either use the possibility to install the packages on your system directly or you can use the docker container.
The project uses c++ and RJava so multiple programmes and libraries are necessary to run it.
The docker container already ships all necessary packages.
Especially, if you want to repoduce all simulations (see [stored data](#stored-data)) you should deploy the project on a machine with multiple cores.
Due to the huge amount of markets generated, the code might still run for multiple hours or days.

## Intitialize the repository

To get all data you have to clone the repository and run

`git submodule update --init`

to load all submodules correctly.

## Build directly on your machine

The project comes with a build script, which you can run using:

`sh build.sh`

It will automatically install all required R-packages, build the two requiered projects `matchingMarkets` and `matchingMarketsEvaluation` and install them on your machine.
Afterwards, you can run the scripts within the folder `run/`, which will repoduce the results.
For instance:

`Rscript run/run.R`

## Build using docker

If you want to use docker you can simply call:

`./docker-compose up`

It will download the docker image and start the R-script provided within docker-compose.yml.
If another script should be run, you can alter it within the `docker-compose.yml`.

## Stored data

Both methods will cache calculated values within the data folder.
Thus, it is not necessary to calculate the data everytime you want to generate the plots.
The data necessary for the current configurations is already included in the repository.
So if you want to recalculate these values, you have to delete the files from data.
The filenames are hashes geneareted from the actual scenario configuration.
So if you change the configurations, the new scenarios will be calculated nevertheless.

The file `run.R` can be used to calculate all data if the data folder is empty.

## Files

There are three files within `/run`.

### run.R

This file executes and calculates all scenarios necessary for the later generation of the plots.
Depending on your machine, this can run hours or days, if no cached data is available.
If all data is cached, this will do nothing.

### iterationanalysis.R

This file create the plot iterationanalysis.pdf, which illustrates the convergence of the assignments within each iteration towards the final stable matching of the IDAT.
Currently, this simulation is not cached, but it also does not take very long to be calculated.

### plot.R

This file generates all plots for the analysis which influence different characteristics of a market have on the number of iterations.
This file can take very long, if the data folder does not already containg cached results. `run.R` will genarate those.



