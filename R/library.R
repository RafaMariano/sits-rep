library <- function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
                     logical.return = FALSE, warn.conflicts, quietly = FALSE,
                     verbose = getOption("verbose"), mask.ok, exclude, include.only,
                     attach.required = missing(include.only)){

  package <- as.character(substitute(package))

  base::library(package, character.only = TRUE)
  packinfo <- installed.packages()[package,]

  Imports <- gsub("\n", " " , packinfo["Imports"])
  Imports <- base::strsplit(Imports, ", ")

  if (!is.na(Imports$Imports[1])){
     list_imp <- base::lapply(Imports$Imports, function(lib){

      lib <- base::strsplit(lib, " ")
      if (!is.null(packageDescription(lib[[1]][1])$GithubSHA1))
        return(list(library = lib[[1]][1],
                    version = getNamespaceVersion(lib[[1]][1])[["version"]],
                    git_repository = packageDescription(lib[[1]][1])$GithubUsername,
                    git_commit = packageDescription(lib[[1]][1])$GithubSHA1))

      return(list(library = lib[[1]][1],
                  version = getNamespaceVersion(lib[[1]][1])[["version"]]))
    })
  }else{
    list_imp <- NULL
  }

  import_obj <- list(import = list(library = package,
                                   version = unname(packinfo["Version"])))
  if(!is.null(list_imp))
    import_obj$import$dependencies <- list_imp

  if (!is.null(packageDescription(package)$GithubSHA1)){
    import_obj$import$git_repository <- packageDescription(package)$GithubUsername
    import_obj$import$git_commit <- packageDescription(package)$GithubSHA1
  }

  json_save(import_obj)

}
