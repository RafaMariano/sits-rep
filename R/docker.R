# mout_container_exists <- function(run_param, dir_save, sits_version){
#
#   # library(dockerfiler)
#   owd <- getwd()
#   on.exit(setwd(owd), add = TRUE)
#   setwd(dir_save)
#
#   my_dock <- dockerfiler::Dockerfile$new(FROM = gsub(" ", "", paste("rafaelmariano/sits:", sits_version), fixed = TRUE))
#   my_dock$RUN("mkdir -p /usr/bin")
#   my_dock$RUN("mkdir -p sits-rep")
#   my_dock$ADD("./", "sits-rep")
#   my_dock$RUN("rm sits-rep/Dockerfile")
#   my_dock$RUN(run_param)
#   my_dock$RUN("cd sits-rep && Rscript script-rep.R")
#
#   my_dock$write()
#   # file.copy(paste0(getwd(), "/", "Dockerfile"), dir_save)
#
# }


mout_container_exists <- function(dir_save, sits_version){

  # library(dockerfiler)
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dir_save)

  my_dock <- dockerfiler::Dockerfile$new(FROM = gsub(" ", "", paste("rafaelmariano/sits:", sits_version), fixed = TRUE))
  my_dock$RUN("mkdir -p /usr/bin")
  my_dock$RUN("mkdir -p sits-rep")
  my_dock$ADD("./", "sits-rep")
  my_dock$RUN("rm sits-rep/Dockerfile")
  my_dock$RUN("cd sits-rep && Rscript install_dependencies.R")
  my_dock$RUN("cd sits-rep && Rscript script-rep.R")

  my_dock$write()
  # file.copy(paste0(getwd(), "/", "Dockerfile"), dir_save)

}



create_container <- function(dir_save){

  library(dockerfiler)

  # TODO: Pegar a semente da execução do json da classificacao
  r_version <- gsub(" ", "", paste(R.version$major, ".", R.version$minor), fixed = TRUE)
  my_dock <- Dockerfile$new(FROM = gsub(" ", "", paste("r-base:", r_version), fixed = TRUE))

  my_dock$RUN("mkdir -p /usr/bin")

  my_dock$RUN("apt-get update && apt-get install -y unzip make build-essential libssl-dev zlib1g-dev libbz2-dev libxml2-dev \\
              libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev libcurl4-openssl-dev  \\
              xz-utils tk-dev libffi-dev liblzma-dev libudunits2-dev m4")

  my_dock$RUN("wget --no-verbose \"https://www.python.org/ftp/python/3.6.8/Python-3.6.8.tgz\" && \\
	tar -xvf Python-3.6.8.tgz && \\
	rm Python-3.6.8.tgz && \\
	cd Python-3.6.8 && \\
	./configure --with-ensurepip=install --enable-shared --prefix=/usr/local LDFLAGS=\"-Wl,--rpath=/usr/local/lib\" && \\
	make && \\
	make install && \\
	cd ..")

  my_dock$RUN("pip3.6 install --ignore-installed --upgrade tensorflow==1.9.0")
  my_dock$RUN("pip3.6 install --ignore-installed --upgrade h5py==2.10.0")

  my_dock$RUN("wget --no-verbose \"download.osgeo.org/geos/geos-3.7.1.tar.bz2\" && \\
	tar -xf geos-3.7.1.tar.bz2 && \\
	rm geos-3.7.1.tar.bz2 && \\
	cd geos-3.7.1 && \\
	./configure --prefix=/usr/local && \\
	make && \\
	make install && \\
	cd ..")

  my_dock$RUN("wget --no-verbose \"download.osgeo.org/proj/proj-5.2.0.tar.gz\" && \\
	tar -xf proj-5.2.0.tar.gz && \\
	rm proj-5.2.0.tar.gz && \\
	cd proj-5.2.0 && \\
	./configure --prefix=/usr/local && \\
	make && \\
	make install && \\
	cd ..")

  my_dock$RUN("wget --no-verbose \"https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.4/src/hdf5-1.10.4.tar.bz2\" && \\
	tar -xf hdf5-1.10.4.tar.bz2 && \\
	rm hdf5-1.10.4.tar.bz2 && \\
	cd hdf5-1.10.4 && \\
	./configure --prefix=/usr/local && \\
	make && \\
	make install && \\
	cd ..")

  my_dock$RUN("wget --no-verbose \"download.osgeo.org/gdal/2.4.2/gdal-2.4.2.tar.gz\" && \\
	tar -xf gdal-2.4.2.tar.gz && \\
	rm gdal-2.4.2.tar.gz && \\
	cd gdal-2.4.2 && \\
	./configure --prefix=/usr/local && \\
	make && \\
	make install && \\
	cd ..")
  my_dock$RUN("ldconfig")
  my_dock$RUN("R -e \"install.packages('devtools')\"")
  my_dock$RUN("R -e \"install.packages('rgeos')\"")
  my_dock$RUN("R -e \"install.packages('sf')\"")
  my_dock$RUN("R -e \"install.packages('raster')\"")
  my_dock$RUN("R -e \"install.packages('rversions')\"")
  my_dock$RUN("R -e \"install.packages('roxygen2')\"")

  my_dock$RUN(paste0("R -e \"devtools::install_github('e-sensing/sits', ref ='", packageDescription('sits')$GithubSHA1,"')\""))

  my_dock$RUN("mkdir -p sits-rep")
  my_dock$ADD("./", "sits-rep")
  my_dock$RUN("rm sits-rep/Dockerfile")

  my_dock$RUN("cd sits-rep")
  my_dock$RUN("Rscript install_dependencies.R")
  my_dock$RUN("Rscript script-rep.R")

  my_dock$write()
  file.copy(paste0(getwd(), "/", "Dockerfile"), dir_save)
}


# create_container <- function(run_param, dir_save){
#
#   library(dockerfiler)
#
#   # TODO: Pegar a semente da execução do json da classificacao
#   r_version <- gsub(" ", "", paste(R.version$major, ".", R.version$minor), fixed = TRUE)
#   my_dock <- Dockerfile$new(FROM = gsub(" ", "", paste("r-base:", r_version), fixed = TRUE))
#
#   my_dock$RUN("mkdir -p /usr/bin")
#
#   my_dock$RUN("apt-get update && apt-get install -y unzip make build-essential libssl-dev zlib1g-dev libbz2-dev libxml2-dev \\
#               libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev libcurl4-openssl-dev  \\
#               xz-utils tk-dev libffi-dev liblzma-dev libudunits2-dev m4")
#
#   my_dock$RUN("wget --no-verbose \"https://www.python.org/ftp/python/3.6.8/Python-3.6.8.tgz\" && \\
# 	tar -xvf Python-3.6.8.tgz && \\
# 	rm Python-3.6.8.tgz && \\
# 	cd Python-3.6.8 && \\
# 	./configure --with-ensurepip=install --enable-shared --prefix=/usr/local LDFLAGS=\"-Wl,--rpath=/usr/local/lib\" && \\
# 	make && \\
# 	make install && \\
# 	cd ..")
#
#   my_dock$RUN("pip3.6 install --ignore-installed --upgrade tensorflow==1.9.0")
#   my_dock$RUN("pip3.6 install --ignore-installed --upgrade h5py==2.10.0")
#
#   my_dock$RUN("wget --no-verbose \"download.osgeo.org/geos/geos-3.7.1.tar.bz2\" && \\
# 	tar -xf geos-3.7.1.tar.bz2 && \\
# 	rm geos-3.7.1.tar.bz2 && \\
# 	cd geos-3.7.1 && \\
# 	./configure --prefix=/usr/local && \\
# 	make && \\
# 	make install && \\
# 	cd ..")
#
#   my_dock$RUN("wget --no-verbose \"download.osgeo.org/proj/proj-5.2.0.tar.gz\" && \\
# 	tar -xf proj-5.2.0.tar.gz && \\
# 	rm proj-5.2.0.tar.gz && \\
# 	cd proj-5.2.0 && \\
# 	./configure --prefix=/usr/local && \\
# 	make && \\
# 	make install && \\
# 	cd ..")
#
#   my_dock$RUN("wget --no-verbose \"https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.4/src/hdf5-1.10.4.tar.bz2\" && \\
# 	tar -xf hdf5-1.10.4.tar.bz2 && \\
# 	rm hdf5-1.10.4.tar.bz2 && \\
# 	cd hdf5-1.10.4 && \\
# 	./configure --prefix=/usr/local && \\
# 	make && \\
# 	make install && \\
# 	cd ..")
#
#   my_dock$RUN("wget --no-verbose \"download.osgeo.org/gdal/2.4.2/gdal-2.4.2.tar.gz\" && \\
# 	tar -xf gdal-2.4.2.tar.gz && \\
# 	rm gdal-2.4.2.tar.gz && \\
# 	cd gdal-2.4.2 && \\
# 	./configure --prefix=/usr/local && \\
# 	make && \\
# 	make install && \\
# 	cd ..")
#   my_dock$RUN("ldconfig")
#   my_dock$RUN("R -e \"install.packages('devtools')\"")
#   my_dock$RUN("R -e \"install.packages('rgeos')\"")
#   my_dock$RUN("R -e \"install.packages('sf')\"")
#   my_dock$RUN("R -e \"install.packages('raster')\"")
#   my_dock$RUN("R -e \"install.packages('rversions')\"")
#   my_dock$RUN("R -e \"install.packages('roxygen2')\"")
#
#   my_dock$RUN(paste0("R -e \"devtools::install_github('e-sensing/sits', ref ='", packageDescription('sits')$GithubSHA1,"')\""))
#
#   my_dock$RUN("mkdir -p sits-rep")
#   my_dock$ADD("./", "sits-rep")
#   my_dock$RUN("rm sits-rep/Dockerfile")
#
#   my_dock$RUN(run_param)
#   my_dock$RUN("cd sits-rep && Rscript script-rep.R")
#
#   my_dock$write()
#   file.copy(paste0(getwd(), "/", "Dockerfile"), dir_save)
# }
