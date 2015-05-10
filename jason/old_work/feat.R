library(dplyr)
library(ggvis)
library(tidyr)

feat <- readRDS("../data/featureMatrix/featMat_v2.0.rds")$train %>%
    tbl_df()
write.csv(feat, file = "feat2.0.csv")