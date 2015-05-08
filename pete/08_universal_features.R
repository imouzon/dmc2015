# ============================================================================
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
# Date: DD-MM-2015
#
# Purpose: Create universal features.
#
# ============================================================================

old <- readRDS("~/GitHub/dmc2015/data/featureMatrix/old/featMat_v2.0.rds")
d <- rbind(old$train, old$class)

universal <- d %>% select(c(1, 369:395))

saveRDS(universal, "~/GitHub/dmc2015/features/feature_files/universal/peteCoupFreq.rds")
