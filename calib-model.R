## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(eval=FALSE, dpi=150, fig.path="figures/")

## ----set_wd--------------------------------------------------------------
setwd(choose.dir())

## ----load_wrv, message=FALSE, results="hide", eval=TRUE------------------
library(wrv)
library(inlmisc)
library(raster)

## ----downlaod_inputs_1, results="hide"-----------------------------------
url <- "http://water.usgs.gov/GIS/dsdl/gwmodels/SIR2016-5080/model.zip"
file <- file.path(tempdir(), basename(url))
download.file(url, file)
files <- unzip(file, exdir = tempdir())
files <- files[basename(files) != "usgs.model.reference"]
dir.create("archive", showWarnings=FALSE, recursive=TRUE)
file.copy(files, "archive", overwrite = TRUE)

## ----downlaod_outputs_1, results="hide"----------------------------------
url <- "http://water.usgs.gov/GIS/dsdl/gwmodels/SIR2016-5080/output.zip"
file <- file.path(tempdir(), basename(url))
download.file(url, file)
files <- unzip(file, exdir = tempdir())
file.copy(files, "archive", overwrite = TRUE)

## ----download_exe_1, results="hide"--------------------------------------
url <- "http://water.usgs.gov/GIS/dsdl/gwmodels/SIR2016-5080/bin.zip"
file <- file.path(tempdir(), basename(url))
download.file(url, file)
files <- unzip(file, exdir = tempdir())
file.copy(files[grep("\\.exe$", files)], "archive", overwrite = TRUE)

## ----model_run_bat_1, results="hide"-------------------------------------
cat("mfusg \"wrv_mfusg.nam\"", file = "archive/RunModflow.bat")

## ----water_budget_1, results="hide"--------------------------------------
UpdateWaterBudget("archive", "wrv_mfusg", qa.tables = "english")

## ----model_rda_1, eval=TRUE----------------------------------------------
load("archive/model.rda")
cat(ls(), sep = ", ")

## ----ref_fun_1, eval=TRUE------------------------------------------------
ReadReferenceFile <- function(file, mask.value = 1e+09) {
  x <- scan(file, quiet = TRUE)
  x[x == mask.value] <- NA
  r <- raster(rs)
  r[] <- x
  return(r)
}

## ----read_hk1_1, eval=TRUE-----------------------------------------------
r <- ReadReferenceFile("archive/hk1.ref")
names(r) <- "lay1.hk"
rs <- stack(rs, r)
r <- ReadReferenceFile("archive/hk2.ref")
names(r) <- "lay2.hk"
rs <- stack(rs, r)
r <- ReadReferenceFile("archive/hk3.ref")
names(r) <- "lay3.hk"
rs <- stack(rs, r)
print(rs)

## ----tran_1, eval=TRUE---------------------------------------------------
r <- (rs[["lay1.top"]] - rs[["lay1.bot"]]) * rs[["lay1.hk"]]

## ----map_hk1, fig.width=7.01, fig.height=9.32, eval=TRUE-----------------
r[] <- log10(r[])
Pal <- colorRampPalette(c("#F02311", "#FFFFEA", "#107FC9"))
usr.map <- c(2451504, 2497815, 1342484, 1402354)
breaks <- pretty(r[], n = 15, na.rm = TRUE)
at <- breaks[c(TRUE, FALSE)]
labels <- ToScientific(10^at, digits = 1, lab.type = "plotmath")
credit <- paste("Base derived from U.S. Geological Survey National Elevation Dataset 10-meter digital",
                "elevation model.\nIdaho Transverse Mercator projection; North American Datum of 1983.")
explanation <- "Transmissivity, in square meters per day, plotted on a logarithmic scale."
PlotMap(r, breaks = breaks, xlim = usr.map[1:2], ylim = usr.map[3:4], bg.image = hill.shading,
        bg.image.alpha = 0.6, dms.tick = TRUE, pal = Pal, explanation = explanation,
        rivers = list(x = streams.rivers), lakes = list(x = lakes),
        labels = list(at = at, labels = labels), credit = credit, contour.lines = list(col = "#1F1F1F"))
plot(cities, pch = 15, cex = 0.8, col = "#333333", add = TRUE)
text(cities, labels = cities@data$FEATURE_NA, col = "#333333", cex = 0.5, pos = 1, offset = 0.4)

## ----head_1, eval=TRUE---------------------------------------------------
heads <- ReadModflowBinary("archive/wrv_mfusg.hds")
dates <- as.Date(vapply(heads, function(i) i$totim, 0), origin = tr.stress.periods[1])
layer <- vapply(heads, function(i) i$ilay, 0L)
FUN <- function(i) {return(setValues(raster(rs), i$d))}
rs.heads.lay1 <- mask(stack(lapply(heads[layer == 1L], FUN)), rs[["lay1.bot"]])
rs.heads.lay2 <- mask(stack(lapply(heads[layer == 2L], FUN)), rs[["lay2.bot"]])
rs.heads.lay3 <- mask(stack(lapply(heads[layer == 3L], FUN)), rs[["lay3.bot"]])
raster.names <- format(dates[layer == 1L])
names(rs.heads.lay1) <- raster.names
names(rs.heads.lay2) <- raster.names
names(rs.heads.lay3) <- raster.names

## ----head_2, eval=TRUE---------------------------------------------------
well <- obs.wells
head <- obs.wells.head
well.config <- GetWellConfig(rs, well, "PESTNAME")
FUN <- function(i) {
  idxs <- which(well.config$PESTNAME == i)
  return(max(well.config[idxs, "lay"]))
}
well@data$lay <- vapply(unique(well.config$PESTNAME), FUN, 0L)
head <- dplyr::left_join(head, well@data[, c("PESTNAME", "desc", "lay")], by = "PESTNAME")
head$Date <- as.Date(head$DateTime)
head$head.obs <- head$Head
FUN <- function(i) {
  loc <- well[match(head$PESTNAME[i], well@data$PESTNAME), ]
  idx <- findInterval(head$Date[i], as.Date(raster.names), all.inside = TRUE)
  rs <- subset(get(paste0("rs.heads.lay", head$lay[i])), c(idx, idx + 1L))
  y <- extract(rs, loc)
  x <- as.numeric(as.Date(raster.names)[c(idx, idx + 1L)])
  return((y[2] - y[1]) / (x[2] - x[1]) * (as.numeric(head$Date[i]) - x[1]) + y[1])
}
head$head.sim <- vapply(seq_len(nrow(head)), FUN, 0)
head$head.res <- head$head.obs - head$head.sim
x <- aggregate(head[, c("head.obs", "head.sim", "head.res")],
               by = list(PESTNAME = head$PESTNAME), mean)
well@data <- dplyr::left_join(well@data, x, by = "PESTNAME")

## ----head_3, eval=TRUE---------------------------------------------------
site.no <- "432650114144701"
w <- well[well$SiteNo %in% site.no, ]
rb <- get(paste0("rs.heads.lay", w@data$lay))
ext <- t(extract(rb, coordinates(w)))
head.sim <- data.frame(Date = as.Date(rownames(ext)), head.sim = ext[, 1])
head.obs <- head[head$PESTNAME == w@data$PESTNAME, , drop = FALSE]
cat(with(w@data, sprintf("Well No. %s, USGS NWIS Site No. %s", WELLNUMBER, SiteNo)))

## ----map_well, fig.width=7.01, fig.height=8.65, eval=TRUE----------------
PlotMap(crs(hill.shading), xlim = usr.map[1:2], ylim = usr.map[3:4], bg.image = hill.shading,
        dms.tick = TRUE, bg.image.alpha = 0.6, rivers = list(x = streams.rivers),
        lakes = list(x = lakes), credit = credit)
plot(alluvium.extent, border = "#FFFFFFCC", col = NA, add = TRUE)
plot(cities, pch = 15, cex = 0.8, col = "#333333", add = TRUE)
text(cities, labels = cities@data$FEATURE_NA, col = "#333333", cex = 0.5, pos = 1, offset = 0.4)
points(w, pch = 21, cex = 1.5, lwd = 0.5, col = NA, bg = "#F02311")
text(w, labels = w@data$SiteNo, col = "#333333", cex = 0.8, pos = 4, offset = 0.4)

## ----graph_head, fig.width=7.16, fig.height=3.50, eval=TRUE--------------
xlim <- range(head.sim$Date)
ylim <- range(pretty(range(c(head.sim$head.sim, head.obs$head.obs))))
cols <- c("#2A8FBDE5", "#A40802", "#FAFAD2")
xbuf <- as.Date(c("1995-01-01", "1998-01-01"))
bg.polygon <- list(x = xy.coords(c(xbuf, rev(xbuf)), c(rep(ylim[1], 2), rep(ylim[2], 2))),
                   col = cols[3])
ylab <- paste("Hydraulic head in", c("meters", "feet"), "above the NAVD88")
m.to.ft <- 3.280839895
PlotGraph(head.sim, xlim = xlim, ylim = ylim, ylab = ylab, col = cols[2],
          conversion.factor = m.to.ft, bg.polygon = bg.polygon, center.date.labels = TRUE)
lines(x = as.Date(head.obs$DateTime), y = head.obs$head.obs,
      type = "b", pch = 20, lwd = 0.5, col = cols[1])
labs <- c("Measured groundwater level", "Simulated groundwater level", "Warm-up period in simulation")
legend("topright", labs, pch = c(20, NA, 22), lwd = c(0.5, 1, NA), col = c(cols[1:2], "lightgray"),
       pt.bg = c(NA, NA, cols[3]), pt.lwd = c(NA, NA, 0.5), pt.cex = c(1, NA, 1.5), inset = 0.02,
       cex = 0.7, box.lty = 1, box.lwd = 0.5, xpd = NA, bg = "#FFFFFFCD")

