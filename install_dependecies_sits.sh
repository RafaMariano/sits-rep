#!/bin/bash

apt-get update 

apt-get install -y unzip make build-essential libssl-dev zlib1g-dev libbz2-dev libxml2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev libcurl4-openssl-dev xz-utils tk-dev libffi-dev liblzma-dev libudunits2-dev m4

wget --no-verbose "https://www.python.org/ftp/python/3.6.8/Python-3.6.8.tgz" && tar -xvf Python-3.6.8.tgz && rm Python-3.6.8.tgz && cd Python-3.6.8 && ./configure --with-ensurepip=install --enable-shared --prefix=/usr/local LDFLAGS="-Wl,--rpath=/usr/local/lib" && make && make install && cd ..

pip3.6 install --ignore-installed --upgrade tensorflow==1.9.0
pip3.6 install --ignore-installed --upgrade h5py==2.10.0

wget --no-verbose "download.osgeo.org/geos/geos-3.7.1.tar.bz2" && tar -xf geos-3.7.1.tar.bz2 && rm geos-3.7.1.tar.bz2 && cd geos-3.7.1 &&  ./configure  --prefix=/usr/local --libdir=/usr/local &&  make &&  make install && cd ..

wget --no-verbose "download.osgeo.org/proj/proj-5.2.0.tar.gz" && tar -xf proj-5.2.0.tar.gz && rm proj-5.2.0.tar.gz && cd proj-5.2.0 &&  ./configure --prefix=/usr/local --libdir=/usr/local &&  make &&  make install && cd ..

wget --no-verbose "https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.4/src/hdf5-1.10.4.tar.bz2" && tar -xf hdf5-1.10.4.tar.bz2 && rm hdf5-1.10.4.tar.bz2 && cd hdf5-1.10.4 &&  ./configure --prefix=/usr/local --libdir=/usr/local &&  make &&  make install && cd ..

wget --no-verbose "download.osgeo.org/gdal/2.4.2/gdal-2.4.2.tar.gz" && tar -xf gdal-2.4.2.tar.gz && rm gdal-2.4.2.tar.gz && cd gdal-2.4.2 &&  ./configure --prefix=/usr/local --libdir=/usr/local &&  make &&  make install && cd ..

ldconfig

apt install -y apt-transport-https software-properties-common
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -s -c)-cran35/"
apt-get update
apt-get install r-base

R -e "install.packages('devtools')"
R -e "install.packages('rgeos')"
R -e "install.packages('sf')"
R -e "install.packages('raster')"
R -e "install.packages('rversions')"
R -e "install.packages('roxygen2')"

# sits 1.12.6
#R -e "devtools::install_github('e-sensing/sits', ref ='d92bdc2f9b0f6158a8ae7ae8b7e4544fcca8390a')" 

