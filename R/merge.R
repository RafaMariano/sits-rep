#' @export
merge <- function (input_file, input_rds, output) {

    files_input <- list.files(input_file, pattern = ".*\\.tif", full.names = TRUE)

    files_years <- gsub("^.*_[^_]{6}_[0-9]+_[0-9]+_[0-9]+_[0-9]+_([0-9]+)_.*\\.tif", "\\1", files_input)

    for (year in unique(files_years)) {
        year_list <- files_input[files_years == year]

        if(length(year_list) < 2)
            next

        res <- lapply(year_list, raster::raster)
        res$filename <- paste0(output, "/mosaic_", sprintf("MT_%s.tif",year))
        do.call(raster::merge, res)
    }
}

