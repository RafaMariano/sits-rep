mout_container_exists <- function(run_param, dir_save, sits_version){

  # library(dockerfiler)

  my_dock <- dockerfiler::Dockerfile$new(FROM = gsub(" ", "", paste("rafaelmariano/sits:", sits_version), fixed = TRUE))
  my_dock$RUN("mkdir -p /usr/bin")
  my_dock$RUN("mkdir -p sits-rep")
  my_dock$ADD("./", "sits-rep")
  my_dock$RUN("rm sits-rep/Dockerfile")
  my_dock$RUN(run_param)
  my_dock$RUN("cd sits-rep && Rscript script-rep.R")

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
