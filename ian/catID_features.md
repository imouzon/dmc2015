---
title: Getting Category IDs Under Control
author: none
course: DMC
date: April 29, 2015
---

[//]: # (R code (No Results in Document))


I am using the following packages:
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
and my working directory is set to \verb!dmc2015/ian!.

# Reading the Data
I am working from the current feature matrix:
<!---  readFeatMat: R code (Code in Document) -->

```r
   featMat = readRDS("~/dmc2015/data/featureMatrix/featMat_v2.0.rds")
   trn = featMat$train
   cls = featMat$class

   #Also reading the melted train and test sets
   trn.m = read.csv("~/dmc2015/data/clean_data/melted_train_simple_name.csv")
   cls.m = read.csv("~/dmc2015/data/clean_data/melted_test_simple_name.csv")

   stack.trn = trn.m
   stack.trn$dsn = "trn"

   stack.cls = cls.m
   stack.cls$dsn = "cls"

   stack.m = rbind(stack.trn,stack.cls)

   stack.m$dsn = factor(stack.m$dsn,levels=c('trn','cls'))
```

In case I need to reference the raw data, I will read that too:
<!---  readRaw: R code (Code in Document) -->

```r
   raw.trn = read.csv("~/dmc2015/data/clean_data/train_simple_name.csv")
   raw.cls = read.csv("~/dmc2015/data/clean_data/test_simple_name.csv")
```

[//]: # (splitcols: R code (No Results in Document))








