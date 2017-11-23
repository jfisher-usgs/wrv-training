## ---- eval=FALSE---------------------------------------------------------
## repos <- c(GRAN = "https://owi.usgs.gov/R", CRAN = "https://cloud.r-project.org/")
## update.packages(ask = FALSE, repos = repos)
## install.packages("wrv", repos = repos, dependencies = TRUE)

## ---- message=FALSE, results="hide"--------------------------------------
library("wrv")

## ---- eval=FALSE---------------------------------------------------------
## path <- "<path/to/repository>"
## dir.create(path, recursive = TRUE)

## ---- eval=FALSE---------------------------------------------------------
## git2r::clone("https://github.com/jfisher-usgs/wrv-training.git", path)

## ---- eval=FALSE---------------------------------------------------------
## setwd(path)

