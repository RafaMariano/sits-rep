.onLoad = function(lib, pkg) {
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    Sys.setenv(R_CONFIG_FILE = "config.yml")
    sits.rep_config()
}

sits.rep.env <- new.env()
