source("sits-rep.R")

sits_rep_classify(tree_name = "deep_learning", "scripts/classification.R")

sits_rep_pos_process("deep_learning/classification","pos_bayes", 
                     "scripts/classification_smooth.R")

sits_rep_merge("deep_learning/classification", "mosaic_classification")
sits_rep_merge("deep_learning/pos_bayes", "mosaic_pos_bayes")

