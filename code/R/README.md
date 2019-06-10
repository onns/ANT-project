This library inclues some of the functions we used for visualizing traces and performing the fingerprinting analysis.
Here we describe the steps for reproducing our experiments:

# Installation

Install R version > 3.3.0.

Install the release version of devtools from CRAN with `install.packages("devtools")`.

Run `R`:

`> install_github('cgvwzq/rlang-loophole')`

All dependencies will be installed automatically.

# Tuning

Download our dataset from ???. The experiment may take days when using the whole dataset and a big parameter space.

```
> library(loophole)
> generateTuningDatabases("rawTuningTraces/", "dbs/", RL=c(4000,2000,1000), RP=c(50,20,10,5), RF=c("sum"))
```

This command will generate all the timeseries with the different combination of parameters `traceDuration`, `P` and `samplingFunction`.

`> executeTuning("dbs/", logFile="tuning.log")`

This will perform a small cross-validation trying DTW with different parameters over the whole dataset:
```
pWindowTypes <- c("itakura", "sakoechiba")
pStepPatterns <- c("symmetric1", "symmetric2", "asymmetric")
pWindowSizes <- c(1, 5, 10, 30, 50, 100)
```

At the end it will generate a table with the results of the tuning phase, i.e. the average performance for each configuration.

# Cross-validation

In order to validate the performance obtained by a "specific set" of parameters we perform a 10-fold cross-validation over an independent sample set.

```
> generateTuningDatabases("rawValidationTraces/", "dbsValidation/", ...)
> runCrossValidation("dbsValidation/", ...)
```

Note: It is necessaryt o specify the specific parameters.

# Datasets
The datasets used are available at:
https://software.imdea.org/cloud/index.php/s/fn9xduGKagwDK1Y

# TODO
*The library needs some clean up (e.g.: reuse repeated functions in `executeTuning` and `runCrossvalidation`).
*Some variables are currently hardcoded into the code and should be parameterized.


