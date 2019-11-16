#' @export
sits.rep_merge <- function(parent, process_name){

  # TODO Pegar a semente da classificação
  seed = 42
  set.seed(seed)

  parent_split <- strsplit(parent, "/")[[1]]
  new_process_path <- new_process(parent_split[1], parent_split[2], process_name)

  files_input <- get_result_raster(parent_split[1], parent_split[2])
  files_years <- gsub("^.*MT_[^_]{6}_[0-9]+_[0-9]+_[0-9]+_[0-9]+_([0-9]+)_.*\\.tif", "\\1", files_input)
  files_years <- files_years[!grepl("probs", files_years)]

  for (year in unique(files_years)) {

    year_list <- files_input[files_years == year]
    res <- lapply(year_list, raster::raster)
    res$filename <- paste(paste0(new_process_path, sep = "/", "result/raster"), sprintf("MT_%s.tif", year), sep = "/")
    do.call(raster::merge, res)
  }

  # json_path <- paste0(new_process_path, sep = "/", get_metadata_json_name())
  json_save(list(tree = parent_split[1],
                   parent = parent_split[2],
                   process = process_name,
                   data_input = paste0("~", sep = "/", get_relative_path(dirname(files_input[1]))),
                   result = list(raster = "result/raster"),
                   args = c("./result/raster",
                            paste0("../", parent_split[2], "/result/raster"))), new_process_path)

  # args = c("~/sits_rep",
  #          process_name,
  #          "result/raster",
  #          get_relative_path(dirname(files_input[1]), parent_split[2]))), json_path)

  script_path <- copy_script(system.file("extdata/scripts/merge_script.R", package = "sits.rep"), new_process_path)
  json_save(list(hash = hash_result(parent_split[1], process_name),
                 script = script_path), new_process_path)

}
