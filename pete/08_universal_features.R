# ============================================================================
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
# Date: DD-MM-2015
#
# Purpose: Create universal features.
#
# ============================================================================

train <- read.csv("~/GitHub/dmc2015/features/feature_files/csv/nCoupTrain.csv")
class <- read.csv("~/GitHub/dmc2015/features/feature_files/csv/nCoupClass.csv")
d <- rbind(train, class)

saveRDS(d, "~/GitHub/dmc2015/features/feature_files/universal/peteCoupFreq.rds")
