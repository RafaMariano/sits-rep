#!/usr/bin/env Rscript
source("sits-rep-docker.R")
execution("classification")
execution("pos_baseyan")
execution("mosaic_pos_baseyan")
verify_hash()
