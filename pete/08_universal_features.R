# ============================================================================
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
# Date: May 5, 2015
#
# Purpose: Create universal features, i.e. features that do not depend on the 
# response variables.
#
# ============================================================================

# Read in my coupon frequency features and write them to an RDS as one dataframe
train <- read.csv("~/GitHub/dmc2015/features/feature_files/csv/nCoupTrain.csv")
class <- read.csv("~/GitHub/dmc2015/features/feature_files/csv/nCoupClass.csv")
d <- rbind(train, class)

saveRDS(d, "~/GitHub/dmc2015/features/feature_files/universal/peteCoupFreq.rds")
