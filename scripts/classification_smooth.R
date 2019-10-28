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



# 
# devtools::install_github()
# devtools::package_info('sits')
# devtools::session_info()
# 
# library(git2r)
# 
# git2r::branch_target(git2r::head(git2r::repository('e-sensing/sits')))
# 
# 
# devtools::install_github("hoxo-m/githubinstall")
# library(githubinstall)
# 
# isGithub <- function(pkg){
#   if(!is.null(packageDescription(pkg)$GithubRepo))
#     return(packageDescription(pkg)$GithubRepo)
#     }
# sapply(dir(.libPaths()), isGithub)
# 



# packageDescription("sits.validate")$GithubRepo
# packageDescription("EOCubes")
# packageDescription("sits")
