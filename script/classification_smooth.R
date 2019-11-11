library(sits)

# Create directory
# if (!dir.exists(paste(outputDir, "2.Classification_Smooth", sep = "/")))
#   dir.create(paste(outputDir, "2.Classification_Smooth", sep = "/"), recursive = TRUE)
outputDir <- "MT"

# smooth parameters
noise = 10
window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE)

# apply the Bayesian filter on the output maps
sits_bayes_postprocess(rasters.tb, # <- como vou obter essa variável?  
                       window = window,
                       noise = noise,
                       file = paste(paste(outputDir, "2.Classification_Smooth", sep = "/"), "smooth", sep = "/"))

