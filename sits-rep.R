sits_rep_classify <- function(tree_name, script){
  
  source("./algoritms_sits_rep.R")
  if (tree_exists(tree_name))
    stop("Já existe uma árvore com esse nome!!!")

  tree_name <- start_tree(gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', tree_name))
  new_process <- new_process(tree = tree_name, parent = NULL, process_name = "classification")
  copy_script_path <- copy_script(script, new_process)

  # TODO
  # #gerar uma semente aleatoria
  seed = 42
  set.seed(seed)
  
  info_r <- list(tree = tree_name,
                 process = "classification", 
                  system = list(
                    seed = seed,
                    r_version = paste0(
                      R.Version()[c("major","minor")],
                      collapse = "."),
                    arch = R.Version()$arch,
                    platform = R.Version()$platform),
                  script = copy_script_path)

  # ,
  # result = paste0(new_process, sep = "/", "result")
  
  json_path <- paste0(new_process, sep = "/", get_metadata_json_name())
  json_append(info_r, json_path)
  
  source(file = copy_script_path, chdir = TRUE)
  
  json_append(list(hash = hash_result(tree_name, "classification")), json_path)
  
}

hash_result <- function(tree, process){
  
  library(digest)
  
  path_principal <- paste0(get_dir_principal(), "/", tree, "/", process)
  input <- paste0(path_principal, "/", "result/raster")
  output <- paste0(path_principal, "/", process, "_checksum.txt")
  
  system(paste0("sha1sum ", input, "/*.* >> ", output))
  return(output)
}



sits_rep_merge <- function(parent, process_name){
  
  source("./algoritms_sits_rep.R")
  seed = 42
  set.seed(seed)
  parent_split <- strsplit(parent, "/")[[1]]
  new_process_path <- new_process(parent_split[1], parent_split[2], process_name)
  # dir.create("result/raster")
  
  files_input <- get_result_raster(parent_split[1], parent_split[2])
  files_years <- gsub("^.*MT_[^_]{6}_[0-9]+_[0-9]+_[0-9]+_[0-9]+_([0-9]+)_.*\\.tif", "\\1", files_input)
  files_years <- files_years[!grepl("probs", files_years)]
  
  for (year in unique(files_years)) {
    
    year_list <- files_input[files_years == year]
    res <- lapply(year_list, raster::raster)
    res$filename <- paste(paste0(new_process_path, sep = "/", "result/raster"), sprintf("MT_%s.tif", year), sep = "/")
    do.call(raster::merge, res)
  }
  
  json_path <- paste0(new_process_path, sep = "/", get_metadata_json_name())
  json_append(list(tree = parent_split[1],
                   parent = parent_split[2],
                   process = process_name,
                   data_input = paste0("~", sep = "/", get_relative_path(dirname(files_input[1]))),
                   result = list(raster = "result/raster"),
                   args = c("./result/raster",
                            paste0("../", parent_split[2], "/result/raster"))), json_path)
                   
                   
                   # args = c("~/sits_rep", 
                   #          process_name, 
                   #          "result/raster", 
                   #          get_relative_path(dirname(files_input[1]), parent_split[2]))), json_path)

  script_path <- copy_script("~/.sits-rep/merge_script.R", new_process_path)
  # hash_result(parent_split[1], process_name)
  
  json_append(list(hash = hash_result(parent_split[1], process_name),
                   script = script_path), json_path)
  
}


# raster <- "/home/rafael/sits_rep/deep_learning/classification/result/raster"
# 
# get_relative_path(raster, "classification")
# 
# get_relative_path(dirname(files_input[1]), parent_split[2])



##

sits_rep_pos_process <- function(parent, process, script){
  
  source("./algoritms_sits_rep.R")

  parent_split <- strsplit(parent, "/")[[1]]
  tree <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[1])
  parent <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[2])

  new_process <- new_process(tree = tree, parent = parent, process_name = process)
  copy_script_path <- copy_script(script, new_process)

  # TODO - Gerar uma semente aleatoria
  seed = 42
  set.seed(seed)
  
  json_path <- paste0(new_process, sep = "/", get_metadata_json_name())
  json_append(list(tree = parent_split[1],
                   parent = parent_split[2],
                   process = process), json_path)
  
  source(file = copy_script_path, chdir = TRUE)
  json_append(list(hash = hash_result(tree, process),
                   script = copy_script_path), json_path)
  
}

sits_rep <- function(tree, process, dir_name){
  
  source("./algoritms_sits_rep.R")
  # tree <- "deep_learning"
  # # process <- "mosaic_pos_baseyan"
  # dir_name <- "deep_learning"

  DIR_REP <- paste0(get_dir_principal(), "/", tree, "/", "reproducible")
  if(!dir.exists(DIR_REP))
    dir.create(DIR_REP)
  
  dir_rep_process <- paste0(DIR_REP, "/", dir_name)
  if(dir.exists(dir_rep_process))
    stop(paste0("Diretório '", dir_rep_process ,"' já existe!!!"))
  
  dir.create(dir_rep_process)
  
  write("#!/usr/bin/env Rscript", paste0(dir_rep_process, "/script-rep.R"), sep = " ")
  write("source(\"sits-rep-docker.R\")", append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")
  
  list_process <- list_process_json(tree, get_branch_of_tree(tree, process))

  url <- paste0("curl --silent -f -lSL https://index.docker.io/v1/repositories/rafaelmariano/sits/tags/", getNamespaceVersion("sits")[["version"]])
  if (system(url) == 0){
    mout_container_exists(get_install_packages(list_process, install_sits = FALSE), dir_rep_process, getNamespaceVersion("sits")[["version"]])
  }else{
    create_container(paste("R -e \"", get_install_packages(list_process, install_sits = TRUE), "\""), dir_rep_process)
  }
  
  seed <- NULL
  # p <- list_process[[2]]
  for(p in list_process){
    
    process_rep_dir <- paste0(dir_rep_process, "/", p$process)
    dir.create(process_rep_dir)
    
    file.copy(p$hash, process_rep_dir)
    file.copy(p$script, process_rep_dir)
    
    if (!is.null(p$system$seed))
      seed <- p$system$seed
    
    json <- list(name = p$process,
                 parent = if(is.null(p$parent))"root" else p$parent,
                 hash = base::basename(p$hash),
                 script = base::basename(p$script),
                 dir_output = p$result,
                 metadata = "metadata.json")
    
    if(!is.null(p$args))
      json$args <- p$args
    
    json_append(json, paste0(process_rep_dir, "/", "metadata.json"))
    
    json <- NULL
    json <- list(process = list(name = p$process,
                                dir = paste0("./",p$process),
                                metadata = paste0("./", p$process, "/", "metadata.json"),
                                script = paste0("./", p$process, "/", base::basename(p$script))))
   
    json_append(json, paste0(dir_rep_process, "/", "metadata.json"))
    
    write(paste0("execution(\"", p$process,"\")"), append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")
  }
  
  write(paste0("verify_hash()"), append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")
  file.copy("~/.sits-rep/algoritms_sits_rep_docker.R", dir_rep_process)
  file.copy("~/.sits-rep/sits-rep-docker.R", dir_rep_process)

  # TIRAR O dir_name e o dir_principal
  json_append(list(seed = seed,
                   dir_principal = get_dir_principal(),
                   dir_name = dir_name), paste0(dir_rep_process, "/", "metadata.json"))
  

  # print(paste0(get_dir_principal(), "/", dir_name, "/reproducible/", dir_name))
  # system(paste0("tar -czvf ", dir_name, ".tar.gz ", paste0(get_dir_principal(), "/", dir_name, "/reproducible/", dir_name)))
  # 
  # 
  # install.packages("dockerfiler")
  
  # tar("LC8.tar.gz", 
  #     files=paste0(get_dir_principal(), "/", "deep_learning", "/reproducible/", "deep_learning"),
  #     compression="gzip",)
}

mout_container_exists <- function(run_param, dir_save, sits_version){
  
  library(dockerfiler)  
  
  my_dock <- Dockerfile$new(FROM = gsub(" ", "", paste("rafaelmariano/sits:", sits_version), fixed = TRUE))
  my_dock$RUN("mkdir -p download")
  my_dock$RUN("mkdir -p /usr/bin")
  my_dock$RUN("mkdir -p sits-rep")
  my_dock$ADD("./", "sits-rep")
  my_dock$RUN("rm sits-rep/Dockerfile")
  my_dock$RUN(run_param)
  my_dock$RUN("Rscript sits-rep/script-rep.R")
  
  my_dock$write()
  file.copy(paste0(getwd(), "/", "Dockerfile"), dir_save)
  
}

create_container <- function(run_param, dir_save){
  
  library(dockerfiler)  
  
  # TODO: Pegar a semente da execução do json da classificacao
  r_version <- gsub(" ", "", paste(R.version$major, ".", R.version$minor), fixed = TRUE)
  my_dock <- Dockerfile$new(FROM = gsub(" ", "", paste("r-base:", r_version), fixed = TRUE))
  
  my_dock$RUN("apt-get update && apt-get install -y unzip")
  my_dock$RUN("apt-get install -y libcurl4-openssl-dev libssl-dev libcurl4-openssl-dev libssl-dev libxml2-dev libudunits2-dev")
  my_dock$RUN("mkdir -p download")
  my_dock$RUN("mkdir -p /usr/bin")
  my_dock$RUN("mkdir -p sits-rep")
  my_dock$ADD("./", "sits-rep")
  my_dock$RUN("rm sits-rep/Dockerfile")
  
  my_dock$RUN("wget --no-verbose \"download.osgeo.org/geos/geos-3.7.1.tar.bz2\" && \\
	tar -xf geos-3.7.1.tar.bz2 && \\
	rm geos-3.7.1.tar.bz2 && \\
	cd geos-3.7.1 && \\
	./configure && \\
	make && \\
	make install && \\
	cd ..")
  
  my_dock$RUN("wget --no-verbose \"download.osgeo.org/proj/proj-5.2.0.tar.gz\" && \\
	tar -xf proj-5.2.0.tar.gz && \\
	rm proj-5.2.0.tar.gz && \\
	cd proj-5.2.0 && \\
	./configure && \\
	make && \\
	make install && \\
	cd ..")
  
  my_dock$RUN("wget --no-verbose \"download.osgeo.org/gdal/2.4.2/gdal-2.4.2.tar.gz\" && \\
	tar -xf gdal-2.4.2.tar.gz && \\
	rm gdal-2.4.2.tar.gz && \\
	cd gdal-2.4.2 && \\
	./configure && \\
	make && \\
	make install && \\
	cd ..")
  
  my_dock$RUN("R -e \"install.packages('devtools')\"")
  my_dock$RUN("R -e \"install.packages('rgeos')\"")
  my_dock$RUN("R -e \"install.packages('sf')\"")
  my_dock$RUN("R -e \"install.packages('raster')\"")
  my_dock$RUN("R -e \"install.packages('rversions')\"")
  my_dock$RUN("R -e \"install.packages('roxygen2')\"")
  my_dock$RUN(run_param)
  my_dock$RUN("Rscript sits-rep/script-rep.R")

  my_dock$write()
  file.copy(paste0(getwd(), "/", "Dockerfile"), dir_save)
}

get_dependencies <- function(dependencies){
 
  dep_formated <- NULL
  for(p in 1:length(dependencies)){
    
    if(!is.null(dependencies[[p]]$git_commit))
      dep_formated <- paste(dep_formated, 
                            paste("devtools::install.github('", dependencies[[p]]$library,
                                  "',ref='", dependencies[[p]]$git_commit,"');"))
    else
      dep_formated <- paste(dep_formated, 
                          paste("devtools::install_version('", dependencies[[p]]$library,
                                "',version='", dependencies[[p]]$version,"');"))
  }
  return(gsub(" ", "", dep_formated, fixed = TRUE))
}

list_process_json <- function(tree, list_process){
  
  source("./algoritms_sits_rep.R")

  list_json <- list()
  for(p in list_process){
    path_json <- paste(get_dir_principal(), tree, p, get_metadata_json_name(), sep = "/")
    list_process <- jsonlite::fromJSON(path_json, simplifyVector = FALSE)
    list_json <- append(list_json, list(list_process))
  }
  
  return(list_json)
}

get_import <- function(imp, install_sits){
  
  is_pk <- NULL
  dep_formated <- NULL
  if(!is.null(imp$dependencies)){
    dep_formated <- paste(dep_formated, 
                          get_dependencies(imp$dependencies))
  }
  if(imp$package == "sits" && install_sits == FALSE)
    return(gsub(" ", "", dep_formated, fixed = TRUE))
  
  if(!is.null(imp$git_commit)){
    is_pk <- paste("devtools::install.github('", imp$package,"',ref='", imp$git_commit,"');")
  }else{
    is_pk <- paste("devtools::install_version('", imp$package,"',version='", imp$version,"');")
  }
  
  return(paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE)))
}


get_install_packages <- function(list_process_json, install_sits = FALSE){
  
  dep_formated <- NULL
  is_pk <- NULL
  list_install_packages <- list()
  lib_name <- list()
  
  for (process_json in list_process_json){
    
    if(!is.null(process_json$import)){
      if(!is.null(process_json$import$package)){
        if(!(process_json$import$package %in% lib_name)){
          is_pk <- get_import(process_json$import, install_sits)
          lib_name <- append(lib_name, process_json$import$package)
        }
      }else{
        for(imp in process_json$import){
          if(!(imp$package %in% lib_name)){
            is_pk <- paste(is_pk, get_import(imp, install_sits))
            lib_name <- append(lib_name, imp$package)
          }
        }
      }
        dep_formated <- paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE))
        list_install_packages <- c(list_install_packages, dep_formated)
        dep_formated <- NULL
        is_pk <- NULL
      }
  } 
  return(gsub(" ", "", paste("install.packages('devtools');", list_install_packages), fixed = TRUE))
}

get_branch_of_tree <- function(tree, process){
  
  ls <- get_tree_as_matrix(tree)
  
  list_p <- NULL
  process_aux <- process

  for(pos in nrow(ls):1){
    
    if (ls[pos, ][2] == process_aux){
      list_p <- c(list_p, ls[pos, ][2])
      process_aux <- ls[pos, ][1]
    }
 }
  
  return(rev(list_p))
}

get_tree_as_matrix <- function(tree){
  
  ls <- read.table(paste("~/sits_rep", tree, "graph.txt", sep = "/"),
                        stringsAsFactors = TRUE)
  colnames(ls) <- NULL
  return(as.matrix(ls))
}

plot_tree <- function(tree){
  
  tree <- "arv_1"
  matrix <- get_tree_as_matrix(tree)
  g <- igraph::graph.edgelist(matrix)
  
  plot(g, edge.arrow.size=.9, edge.arrow.color = "yellow", vertex.label.color="black", 
       vertex.label.dist=2.5,
        vertex.color=c("green", "red", "skyblue","skyblue","skyblue" )) 
  
  
  # plot(g)
}



pkgTest <- function(x)
{
  if (!require(x, character.only = TRUE, quietly = TRUE))
    install.packages(x,dep=TRUE)

  pkgTest <- function(x)
  {
    if (!require(x, character.only = TRUE, quietly = TRUE))
      install.packages(x,dep=TRUE)
  }
  
  # 
  # pkgTest("aaaaaaaacccc")
  # pkgTest("inSitu")
  # pkgTest("abtest")
  # 
  # data(br_mt_1_8K_9classes_6bands)
  # 
  # print("a")
  }


  
