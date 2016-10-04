## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(eval=FALSE, dpi=150, fig.path="figures/")

## ----load_wrv, message=FALSE---------------------------------------------
library(wrv)

## ----app_c_r_1, message=FALSE--------------------------------------------
file <- system.file("doc", "sir20165080_AppendixC.R", package = "wrv")
app.c.r <- readLines(file)
line <- grep("## ----download_git", app.c.r) - 1
eval(parse(text = app.c.r[1:line]))
app.c.r <- app.c.r[-(1:line)]

## ----app_c_r_2, message=FALSE--------------------------------------------
line <- grep("## ----create_data_dir", app.c.r) - 1
app.c.r <- app.c.r[-(1:line)]

## ----app_c_r_3, message=FALSE--------------------------------------------
dir.git <- file.path(getwd(), "extdata")

## ----app_c_r_4, message=FALSE--------------------------------------------
line <- grep("## ----canal_seep_1", app.c.r) - 1
eval(parse(text = app.c.r[1:line]))
app.c.r <- app.c.r[-(1:line)]

## ----comb_sw_irr_1-------------------------------------------------------
file <- file.path(dir.git, "div/comb.sw.irr.csv")
comb.sw.irr <- read.csv(file, strip.white = TRUE)
comb.sw.irr$Pdate <- as.Date(comb.sw.irr$Pdate, format = "%m/%d/%Y")
comb.sw.irr$MaxDivRate <- comb.sw.irr$MaxDivRate * cfs.to.m3.per.d
save(comb.sw.irr, file = file.path(dir.dat, "comb.sw.irr.rda"), compress = "xz")

## ----pod_gw_1------------------------------------------------------------
file <- file.path(dir.git, "div/pod.gw.csv")
d <- read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
d$Pdate <- as.Date(d$PriorityDa, format = "%m/%d/%Y")
d$IrrRate <- d$IRRcfs * cfs.to.m3.per.d
columns <- c("WMISNumber", "WaterRight", "EntityName", "EntitySrce", "Pdate", "IrrRate")
pod.gw <- d[, columns]
save(pod.gw, file = file.path(dir.dat, "pod.gw.rda"), compress = "xz")

## ----pod_wells_1---------------------------------------------------------
path <- file.path(dir.git, "div")
pod.wells <- readOGR(dsn = path, layer = "pod.wells", verbose = FALSE)
pod.wells <- spTransform(pod.wells, crs)
d <- pod.wells@data
columns <- c("TopOpen1", "BotOpen1", "TopOpen2", "BotOpen2")
d[, columns] <- d[, columns] * ft.to.m
d[d$TopOpen1 == 0 | d$BotOpen1 == 0, c("TopOpen1", "BotOpen1")] <- NA
d[d$TopOpen2 == 0 | d$BotOpen2 == 0, c("TopOpen2", "BotOpen2")] <- NA

## ----pod_wells_2---------------------------------------------------------
is.pred <- is.na(d$TopOpen1)
dists <- as.matrix(dist(coordinates(pod.wells)))
dists <- dists[!is.pred & d$WellUse %in% "Irrigation", ]
nearest.well <- as.integer(apply(dists, 2, function(i) names(which.min(i))))
d$TopOpen1[is.pred] <- d$TopOpen1[nearest.well[is.pred]]
d$BotOpen1[is.pred] <- d$BotOpen1[nearest.well[is.pred]]
columns <- c("WMISNumber", "WellUse", "TopOpen1", "BotOpen1", "TopOpen2", "BotOpen2")
pod.wells@data <- d[, columns]
save(pod.wells, file = file.path(dir.dat, "pod.wells.rda"), compress = "xz")

## ----irr_lands_1---------------------------------------------------------
path <- file.path(dir.git, "irr")
yr <- c(1996, 2000, 2002, 2006, 2008, 2009, 2010)
files <- paste0("irr.lands.", yr)
irr.lands <- list()
for (i in seq_along(files)) {
  p <- readOGR(dsn = path, layer = files[i], verbose = FALSE)
  p <- spTransform(p, crs)
  p@data <- p@data[, paste0("STATUS_", substr(yr[i], 1, 3)), drop = FALSE]
  names(p@data) <- "Status"
  p <- p[p@data[, "Status"] != "non-irrigated", ]
  p <- rgeos::gBuffer(p, width = 0, byid = TRUE)
  p@data <- droplevels(p@data)
  irr.lands[[i]] <- p
}
names(irr.lands) <- as.character(yr)
save(irr.lands, file = file.path(dir.dat, "irr.lands.rda"), compress = "xz")

## ----et_1----------------------------------------------------------------
files <- file.path(dir.git, "et", paste0("et.", yr.mo, ".tif"))
FUN <- function(i) {
  r <- readGDAL(files[i], band = 1, silent = TRUE)
  r[[1]] <- r[[1]] * mm.to.m
  return(r)
}
et.raw <- lapply(seq_along(files), FUN)
names(et.raw) <- as.character(yr.mo)

## ----et_2----------------------------------------------------------------
high.res.spatial.grid <- disaggregate(spatial.grid, fact = 5L) # added line
is.missing <- is.na(alluvium.thickness)
FUN <- function(i) {
  r <- aggregate(projectRaster(raster(i), high.res.spatial.grid), fact = 5L)
  r[is.missing] <- NA
  upper.limit <- mean(r[], na.rm = TRUE) + sd(r[], na.rm = TRUE) * 3
  r[r > upper.limit] <- upper.limit
  return(round(r, digits = 6))
}
et <- stack(lapply(et.raw, FUN), quick = TRUE)
names(et) <- as.character(yr.mo)
save(et, file = file.path(dir.dat, "et.rda"), compress = "xz")

## ----level_2_1, message=FALSE--------------------------------------------
line <- grep("## ----entity_components_1", app.c.r) - 1
app.c.r <- app.c.r[-(1:line)]
eval(parse(text = app.c.r))

## ----pre_1, message=FALSE, eval=TRUE-------------------------------------
rm(list = ls())
files <- list.files(file.path(getwd(), "data"), pattern = "*.rda$", full.names = TRUE)
for (i in files) load(i)

## ----pre_2, eval=TRUE----------------------------------------------------
file <- system.file("doc", "sir20165080_AppendixD.R", package = "wrv")
app.d.r <- readLines(file)
app.d.r <- app.d.r[-grep("invisible.*dev.off", app.d.r)]

## ----pre_3, collapse=TRUE, message=FALSE, results="hide", fig.keep="none", eval=TRUE----
line <- grep("## ----write_modflow_input", app.d.r)
eval(parse(text = app.d.r[1:line]))
app.d.r <- app.d.r[-(1:line)]
line <- grep("## ----", app.d.r)[1] - 1
app.d.r <- app.d.r[-(1:line)]

## ----pre_4, eval=TRUE----------------------------------------------------
id <- "wrv_mfusg"
dir.run <- file.path(getwd(), "model")

## ----pre_5, results="hide", eval=TRUE------------------------------------
WriteModflowInput(rs.model, rech, well, trib, misc, river, drain, id, dir.run,
                  is.convertible = FALSE, tr.stress.periods = tr.stress.periods,
                  ntime.steps = ntime.steps, verbose = FALSE)

## ----calibrated_1, results="hide", eval=TRUE-----------------------------
dir.archive <- file.path(getwd(), "archive")
files <- list.files(dir.archive, full.names = TRUE)
files <- files[grep("(\\.ref|\\.riv|\\.drn|eff\\.csv|seep\\.csv|trib\\.csv)$", files)]
file.copy(files, dir.run, overwrite = TRUE)

## ----rda_1, results="hide", eval=TRUE------------------------------------
rs <- subset(rs.model, c("lay1.top", sprintf("lay%s.bot", 1:3)))
save(rs, misc, trib, tr.stress.periods, ss.stress.periods, reduction, d.in.mv.ave,
     file = file.path(dir.run, "model.rda"))

## ----update_budget_1, results="hide", eval=TRUE--------------------------
UpdateWaterBudget(dir.run, id, qa.tables = "english", pod.wells = pod.wells,
                  comb.sw.irr = comb.sw.irr, et = et, pod.gw = pod.gw,
                  entity.components = entity.components, rs.entities = rs.entities)

## ----run_modflow_1-------------------------------------------------------
line <- grep("## ----read_budget", app.d.r, eval=TRUE)
eval(parse(text = app.d.r[1:line]))

## ----run_modflow_2, echo=-1, eval=TRUE-----------------------------------
line <- grep("## ----read_budget", app.d.r)
app.d.r <- app.d.r[-(1:line)]

## ----post_1, collapse=TRUE, message=FALSE, results="hide", fig.keep="none", eval=TRUE----
dir.run <- file.path(getwd(), "archive")
line <- grep("## ----table_budget", app.d.r)
eval(parse(text = app.d.r[1:line]))
d.river.arch   <- d.river
d.drain.1.arch <- d.drain.1
d.drain.2.arch <- d.drain.2
d.rech.arch    <- d.rech
d.well.arch    <- d.well
d.trib.arch    <- d.trib

## ----graph_water_budget, fig.width=fin.graph[1], fig.height=fin.graph[2], eval=TRUE----
dir.run <- file.path(getwd(), "model")
line <- grep("## ----table_budget", app.d.r)
eval(parse(text = app.d.r[1:line]))

## ----table_1, echo=FALSE, eval=TRUE--------------------------------------
sdate <- as.Date("1995-01-01", tz="MST")
is.date <- colnames(m) >= format(sdate, "%Y")

flow.direction  <- c("**Inflow**", "", "", "", "**Outflow**", "", "", "", "", "",
                      "**Inflow - Outflow**")
flow.components <- c("Water-table recharge", "Streamflow losses", "Tributary basin underflow", " ",
                     "Water-table discharge", "Streamflow gains", "Production well pumping",
                     "Stanton Crossing outlet boundary", "Silver Creek outlet boundary", " ",
                     "Change in aquifer storage")

flow.arch <- c(mean(d.rech.arch$flow.in[is.date]),
               mean(d.river.arch$flow.in[is.date]),
               mean(d.trib.arch$flow[is.date]),
               NA,
               mean(d.rech.arch$flow.out[is.date]),
               mean(d.river.arch$flow.out[is.date]),
               mean(d.well.arch$flow[is.date]),
               mean(d.drain.1.arch$flow[is.date]),
               mean(d.drain.2.arch$flow[is.date]),
               NA,
               NA)
flow.arch <- as.integer(abs(flow.arch))
flow.arch[11] <- sum(flow.arch[1:3]) - sum(flow.arch[5:9])
flow.arch <- flow.arch * m3.per.d.to.af.per.yr

flow <- c(mean(d.rech$flow.in[is.date]),
          mean(d.river$flow.in[is.date]),
          mean(d.trib$flow[is.date]),
          NA,
          mean(d.rech$flow.out[is.date]),
          mean(d.river$flow.out[is.date]),
          mean(d.well$flow[is.date]),
          mean(d.drain.1$flow[is.date]),
          mean(d.drain.2$flow[is.date]),
          NA,
          NA)
flow <- as.integer(abs(flow))
flow[11] <- sum(flow[1:3]) - sum(flow[5:9])
flow <- flow * m3.per.d.to.af.per.yr

d <- data.frame(flow.direction, flow.components, flow.arch, flow, stringsAsFactors=FALSE)
rownames(d) <- NULL

d$difference <- NA
d$difference <- d[, 4] - d[, 3]

d$percent.change <- NA
d$percent.change <- ((d[, 4] - d[, 3]) / d[, 3]) * 100

columns <- c("", "Component", "Archived rate<br>(acre-ft/yr)", "Modified rate<br>(acre-ft/yr)",
             "Difference<br>(arce-ft/yr)", "Percent<br>change")
colnames(d) <- columns
d[, 3] <- formatC(d[, 3], format="f", digits=0, big.mark=",")
d[, 4] <- formatC(d[, 4], format="f", digits=0, big.mark=",")
d[, 5] <- formatC(d[, 5], format="f", digits=0, big.mark=",")
d[, 6] <- formatC(d[, 6], format="f", digits=1, big.mark=",")
d[, 3:6][d[, 3:6] == "NA"] <- ""
knitr::kable(d, format="markdown", padding=0, booktabs=TRUE, digits=0,
             row.names=FALSE, col.names=columns, align="llrrrr")

## ----post_2, collapse=TRUE, message=FALSE, results="hide", fig.keep="none", eval=TRUE----
line <- grep("## ----read_head", app.d.r) - 1
app.d.r <- app.d.r[-(1:line)]
dir.run <- file.path(getwd(), "archive")
line <- grep("## ----map_head_exceedance", app.d.r)
eval(parse(text = app.d.r[1:line]))
rs.wt.arch <- rs.wt

## ----post_3, collapse=TRUE, message=FALSE, results="hide", fig.keep="none", eval=TRUE----
dir.run <- file.path(getwd(), "model")
line <- grep("## ----map_head_exceedance", app.d.r)
eval(parse(text = app.d.r[1:line]))
line <- grep("## ----map_wt_e", app.d.r)
app.d.r <- app.d.r[-(1:line)]

## ----map_water_tbl_diff, echo=FALSE, fig.width=fin.map.s[1], fig.height=fin.map.s[2], eval=TRUE----
FUN <- function(usr, credit=NULL, max.dev.dim=c(21, 56), add.legend=FALSE) {
  Pal <- function(...) {
    Pal1 <- colorRampPalette(c("#F02311", "#F02311", "#FFD0D4"))
    Pal2 <- colorRampPalette(c("#FCFBE3", rep("#67A9CF", 3)))
    n1 <- round(... * ratio)
    n2 <- ... - n1
    return(c(Pal1(n1), Pal2(n2)))
  }
  r <-  rs.wt[[nlayers(rs.wt)]] - rs.wt.arch[[nlayers(rs.wt.arch)]]
  r <- crop(r, extent(usr))
  zlim <- range(pretty(range(r[], na.rm=TRUE)))
  explanation <- "Hydraulic head differnce, in meters"
  PlotMap(r, xlim=usr[1:2], ylim=usr[3:4], zlim=zlim, bg.image=hill.shading,
          bg.image.alpha=0.6, pal=Pal, dms.tick=TRUE, max.dev.dim=max.dev.dim,
          credit=credit, rivers=list(x=streams.rivers), lakes=list(x=lakes),
          explanation=explanation, contour.lines=list(col="#1F1F1F"))
  if (add.legend)
    legend("topright", "Contour line", col="#1F1F1F", lty=1, lwd=0.5,
           inset=0.02, cex=0.7, box.lty=1, box.lwd=0.5, bg="#FFFFFFCD")
}
FUN(usr.map.s, credit, max.dev.dim=c(43, 56), add.legend=TRUE)

