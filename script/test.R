library(sits.rep)

sits.rep::sits.rep_classify("arv_1", "~/workspace/sits.rep/script/classification.R")
sits.rep::sits.rep_reproduce("arv_1", "classification", "rep_deeplearnings")


# sits.rep::sits.rep_pos_processing("arv_1/classification",
#                                   "pos_bayesan",
#                                   "~/workspace/sits.rep/script/classification_smooth.R")
#
# sits.rep::sits.rep_merge("arv_1/pos_bayesan", "mosaic")
#
#
# sits.rep::sits.rep_reproduce("arv_1", "mosaic", "rep_deeplearnings")


