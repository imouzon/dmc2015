---
title: Clean Data Version 1
author: imouzon
course: DMC
date: April 30, 2015
---

[//]: # (R code (No Results in Document))


[//]: # (R code (No Results in Document))

```r
   library(ggplot2)
   library(lubridate)
   library(xtable)
   library(foreach)
   library(rCharts)
   library(magrittr)
   library(tidyr)
   library(dplyr)
   library(reshape2)
   library(gtools)
   library(sqldf)
```

#Reading in the data
All the columns in the training and test set are also contained in the feature matrix.
I will read from there:
[//]: # (readdata: R code (No Results in Document))

To make sure that we have captured all of the raw data columns, I will 
also read in the raw training and classification sets:
[//]: # (readdata: R code (No Results in Document))


I want to add some general purpose columns to this dataset.
I am adding:
[//]: # (addCols: R code (No Results in Document))

We would like to keep an ordering that resembles the original dataset:
[//]: # (ordcols: R code (No Results in Document))

and we can save the results:
[//]: # (: R code (No Results in Document))

