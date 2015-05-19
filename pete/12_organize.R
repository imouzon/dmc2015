# ============================================================================
# File Name: 12_organize.R
#
# Author: Evan P. Walsh
# Contact: epwalsh@iastate.edu
#
# Creation Date: 19-05-2015
# Last Modified: Tue May 19 02:41:09 2015
#
# Purpose: Clean up predictions and what not.
#
# ============================================================================

vars <- readRDS("~/GitHub/dmc2015/pete/predictions/importance_H3_0.8.rds")
out <- vars[1:150,]
saveRDS(out, "~/GitHub/dmc2015/penglh/imp_set1/imp_crf_col_name.rds")
