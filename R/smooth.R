#' @export
smooth <-  function (input_file, input_rds, output) {
    library(raster)
    library(sp)
    library(sits)
    result <- sits_bayes_postprocess(raster_class = input_rds$rasters.tb,
                                     window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE),
                                     noise = 10, file = paste0(output, "/pos_bayesan"))
    return(result)
}
