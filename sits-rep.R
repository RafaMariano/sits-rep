sits_rep <- function(script, DIR = "~/sits-rep"){
  
  # install.packages("Hmisc")
  
  if (dir.exists(DIR))
    base::unlink(DIR, recursive = TRUE, force = TRUE)
  
  dir.create(DIR, recursive = TRUE)
  
  ## pegar o diretorio corrente e guardar em uma variavel
  
  base::setwd(dir = DIR) ## Error in setwd(path_original) : character argument expected
  base::file.copy(script, base::basename(script), overwrite = TRUE, copy.date = TRUE)

  ## verificar se esta seed tambem eh usada pelo tensorflow
  ## senao tem que ter uma seed para ele tambem
  
  seed = 1
  set.seed(seed)
  
  info_r <- list(system = list(seed = seed,
                               r_version = paste0(R.Version()[c("major","minor")],
                                                  collapse = "."),
                               arch = R.Version()$arch,
                               platform = R.Version()$platform),
                 script = paste0("", base::basename(script)))
  
  json_append(info_r)
  
  source(file = script)
  
  ## setwd com diretorio corrente pra voltar
}

sits_select_bands_ <- function(data.tb, bands){
  
  bands_list <- list(bands = bands)
  json_append(bands_list)

  return(data.tb %>%
           sits::sits_select_bands_(bands = bands))
}

library <- function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
                     logical.return = FALSE, warn.conflicts, quietly = FALSE, 
                     verbose = getOption("verbose"), mask.ok, exclude, include.only, 
                     attach.required = missing(include.only)){
  
  package <- as.character(substitute(package))
  
  base::library(package, character.only = TRUE)
  packinfo <- installed.packages()[package,]
  
  Imports <- packinfo["Imports"]
  Imports <- gsub("\n", " " ,Imports)
  Imports <- base::strsplit(Imports, ", ")
  
  if (is.na(Imports$Imports[1]) == FALSE){
    list_imp <- base::lapply(Imports$Imports, function(lib){
    
      lib <- base::strsplit(lib, " ")
      return(list(library = lib[[1]][1],
                  version = base::toString(utils::packageVersion(lib[[1]][1]))))
    })
    
  }else{
    list_imp <- list(library = Imports$Imports)
   }
  
  import_obj <- list(import = list(package = package, 
                                   version = unname(packinfo["Version"]), 
                                   dependencies = list_imp))
  
  json_append(import_obj)
  
}


sits_svm <- function (data.tb = NULL, formula = sits::sits_formula_logref(), scale = FALSE, 
                      cachesize = 1000, kernel = "radial", degree = 3, coef0 = 0, cost = 10, 
                      tolerance = 0.001, epsilon = 0.1, cross = 0, ...) {
  
  
  if (!dir.exists("train/"))
    dir.create("train/", recursive = TRUE)
  
  
  path_rds = paste0("train/", "functions.rds")
  param_list <- list(model = list(type = "svm", #data.tb = path_rds,
                                  model = path_rds, formula = path_rds,
                                  scale = scale, cachesize = cachesize, kernel = kernel,
                                  degree = degree, coef0 = coef0, cost = cost, tolerance = tolerance,
                                  epsilon = epsilon, cross = cross, ...))
  
  json_append(param_list)
  
  model <- sits::sits_svm(data.tb = data.tb, formula = formula, 
                          scale = scale, cachesize = cachesize, 
                          kernel = kernel, degree = degree, 
                          coef0 = coef0, cost = cost, 
                          tolerance = tolerance, epsilon = epsilon, 
                          cross = cross, ...)
  
  if(file.exists("train/functions.rds")){
    rds <- base::readRDS(path_rds)
    rds$model <- model
    rds$formula <- formula
    base::saveRDS(rds, file=path_rds)
    
  } else{
    base::saveRDS(list(model = model, formula = formula), 
                  file=path_rds)
  }
  
  return(model)
}

sits_train <- function (data.tb, ml_method = sits::sits_svm()) 
{
  
  if (!dir.exists("train/"))
    dir.create("train/", recursive = TRUE)
  
  result_train <- data.tb %>% 
    sits::sits_train(ml_method = ml_method)
  
  if(file.exists("train/functions.rds")){
    rds <- base::readRDS("train/functions.rds")
    rds$train <- result_train
    base::saveRDS(rds, file="train/functions.rds")
    
  } else{
    base::saveRDS(list(train = result_train), 
                  file="train/functions.rds")
  }
  
  json_append(list(train = "train/functions.rds"))
  
  return(result_train)
}

sits_coverage <- function (service = "RASTER", name, timeline = NULL, bands = NULL, 
                           missing_values = NULL, scale_factors = NULL, minimum_values = NULL, 
                           maximum_values = NULL, files = NA, tiles_names = NULL, geom = NULL, 
                           from = NULL, to = NULL) {
  
  dir_geom = "geom/"
  dir.create(dir_geom)
  sf::write_sf(geom, dsn = dir_geom, layer = "geom", driver = "ESRI Shapefile", 
               delete_layer = TRUE, delete_dsn = TRUE)
  
  json <- list(coverage = list(service = service, name = name, 
                               timeline = timeline,
                               missing_values = missing_values,
                               scale_factors = scale_factors, 
                               minimum_values = minimum_values,
                               maximum_values = maximum_values, 
                               files = files, tiles_names = tiles_names, 
                               geom = paste0(file.path(getwd(), dir_geom)),
                               from = from, to = to))
  
  json_append(json)
  return(sits::sits_coverage(service, name, timeline, bands,
                             missing_values, scale_factors, minimum_values,
                             maximum_values, files, tiles_names, geom,
                             from, to))
  
  ##pegar o intervalo
}
 
sits_classify_cubes <- function (file = NULL, coverage = NULL, ml_model = NULL, interval = "12 month",
                                 filter = NULL, memsize = 4, multicores = NULL) {

  result_classify <- sits::sits_classify_cubes(file = file, coverage = coverage,
                                               ml_model = ml_model, interval = interval,
                                               filter = filter, memsize = memsize,
                                               multicores = multicores)


  
  base::file.copy(base::dirname(file), ".", overwrite = TRUE, copy.date = TRUE, recursive = TRUE)
  file.rename(base::basename(base::dirname(file)), "classification")
  if (!dir.exists("classification/RDS"))
    dir.create("classification/RDS", recursive = TRUE)
  
  base::saveRDS(list(classify_cubes = result_classify), 
                file="classification/RDS/classify_cubes.rds")
  
  json <- list(classify_cubes = list(dir = "classification",
                                     classify_cubes = "classification/RDS/classify_cubes.rds",
                                     interval = interval, filter = filter,
                                     memsize = memsize, multicores = multicores))

  json_append(json)

  return(result_classify)
}

## nao eh path_json, eh so o nome
json_append <- function(list_param, path_json = "data_rep.JSON", order = 1){
  
  if(base::file.exists(path_json) && 
     base::file.info(path_json)$size > 0){
    
    json_append_exist(list_param, path_json)
    
  }else{
    json_append_new(list_param, path_json)
  }
}

json_is_array <- function (json){
  
  return(substr(jsonlite::toJSON(json), 1, 1) == "[")
}


write_json <- function(json, path){
  
  if (base::is.list(json))
    json <- jsonlite::toJSON(json, 
                             pretty = TRUE, 
                             auto_unbox = TRUE)
  base::write(json, path)
}


json_append_new <- function (list_param, path_json){
  
  write_json(list_param, path_json)
}


json_append_exist <- function (list_param, path_json){
 
  old_json <- jsonlite::fromJSON(path_json, simplifyVector = FALSE)
  key <- names(list_param)
  
  list_temp <- base::sapply(key, function(k, l, o){
    
    if (k %in% names(o)){
      return(base::sapply(k, function(k_, list_p, old_j){
        
        if(json_is_array(old_j)){
          old_j[[length(old_j)+1]] <- list_p
          l = list(old_j)
          
        }else{
          l = list(list(old_j, list_p))
         }
        
        names(l) = k_
        return(l)
        
      }, l[[k]], o[[k]], simplify = TRUE, USE.NAMES = FALSE))
      
    }else{
      return(l[k])
    }
  }, list_param, old_json, simplify = TRUE, USE.NAMES = FALSE)
  
  old_json[key] <- list_temp[key]
  write_json(old_json, path_json)
}