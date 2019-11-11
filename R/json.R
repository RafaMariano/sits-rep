json_save <- function(list, path = "."){

  path <- paste0(path, sep = "/", sits.rep.env$config$METADATA_BASE_NAME)

  if(base::file.exists(path) && base::file.info(path)$size > 0)
    .exist_json(list, path)

  else
    .new_json(list, path)

}


.exist_json <- function (list, path){

  old_json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  key <- names(list)

  list_temp <- base::sapply(key, function(k, l, o){

    if (k %in% names(o)){
      return(base::sapply(k, function(k_, list_p, old_j){

        if(.is_array(old_j)){
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
  }, list, old_json, simplify = TRUE, USE.NAMES = FALSE)

  old_json[key] <- list_temp[key]
  .write_json(old_json, path)

}


.new_json <- function (list, path){

  .write_json(list, path)

}


.write_json <- function(json, path){

  if (base::is.list(json))
    json <- jsonlite::toJSON(json,
                             pretty = TRUE,
                             auto_unbox = TRUE)
  base::write(json, path)

}


.is_array <- function (json){

  return(substr(jsonlite::toJSON(json), 1, 1) == "[")

}
