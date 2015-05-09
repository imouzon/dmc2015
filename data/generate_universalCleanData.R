# New Clean Features
# Aggregate H, V, T, C from HTVset3.rds into 1 big data frame.

d = readRDS("featureMatrix/HTVset3.rds")
d2 = rbind(d$H, d$V, d$T, d$C)

saveRDS(d2, "clean_data/universalCleanData.rds")