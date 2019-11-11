sits.rep_reproduce <- function(tree, process, dir_name){

  DIR_REP <- paste0(get_dir_principal(), "/", tree, "/", "reproducible")
  if(!dir.exists(DIR_REP))
    dir.create(DIR_REP)

  dir_rep_process <- paste0(DIR_REP, "/", dir_name)


  if(dir.exists(dir_rep_process))
    stop(paste0("Already exist a reproducible diretory of '", process,"' in tree '", tree,"'."))

  dir.create(dir_rep_process)
  write("#!/usr/bin/env Rscript", paste0(dir_rep_process, "/script-rep.R"), sep = " ")
  write("source(\"sits-rep-docker.R\")", append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")

  list_process <- list_process_json(tree, get_branch_of_tree(tree, process))

  url <- paste0("curl --silent -f -lSL https://index.docker.io/v1/repositories/rafaelmariano/sits/tags/", getNamespaceVersion("sits")[["version"]])
  if (system(url) == 0){
    mout_container_exists(paste("R -e \"", get_install_packages(list_process, install_sits = FALSE), "\""), dir_rep_process, getNamespaceVersion("sits")[["version"]])
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

    if (!is.null(p$coverage$geom)){
      # dir.create(paste0(process_rep_dir, "coverage"), recursive = TRUE)
      file.copy(base::dirname(p$coverage$geom),
                process_rep_dir, recursive = TRUE)

      json$coverage <- list(geom = paste0(base::basename(base::dirname(p$coverage$geom)),
                                          "/", base::basename(p$coverage$geom)))
    }

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
