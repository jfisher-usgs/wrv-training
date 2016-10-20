## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(eval=FALSE)

## ------------------------------------------------------------------------
repos <- c(GRAN = "http://owi.usgs.gov/R", CRAN = "https://cloud.r-project.org/")
update.packages(ask = FALSE, repos = repos)
install.packages("wrv", repos = repos, dependencies = TRUE)

## ------------------------------------------------------------------------
library("wrv")

## ------------------------------------------------------------------------
path <- "<path/to/repository>"
dir.create(path, recursive = TRUE)

## ------------------------------------------------------------------------
git2r::clone("https://github.com/jfisher-usgs/wrv-training.git", path)

## ------------------------------------------------------------------------
setwd(path)

