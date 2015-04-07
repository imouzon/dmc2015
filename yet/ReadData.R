dat <- read.table("data/raw_data/DMC_2015_orders_train.txt", sep = "|", header = T)
write.csv(dat, file = "yet/dat.csv", row.names = F)
