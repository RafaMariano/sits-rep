library(sits.rep)

sits.rep::sits.rep_classify("tree_2", "~/workspace/sits.rep/script/classification.R")
sits.rep::sits.rep_pos_processing("tree_1/classification", "pos_bayesan", "~/workspace/sits.rep/script/classification_smooth.R")
sits.rep::sits.rep_merge("tree_1/pos_bayesan", "mosaic")


sits.rep::sits.rep_reproduce("tree_1", "mosaic", "rep_deeplearnings")
