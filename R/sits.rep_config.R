sits.rep_config <- function() {

  yml_file <- system.file("extdata", "config.yml", package = "sits.rep")

  ensurer::ensure_that(yml_file, !purrr::is_null(.),
                       err_desc = "sits.rep_config : Please provide a valid configuration file")

  sits.rep.env$config <- config::get(file = yml_file)

  # WD <- getwd()
  # if (file.exists(paste0(WD, "/config.yml"))) {
  #   user_yml_file <- paste0(WD, "/config.yml")
  #   config_user <- config::get(file = user_yml_file)
  #   sits.rep.env$config <- config::merge(sits.rep.env$config, config_user)
  # }

  return(invisible(sits.rep.env$config))
}
