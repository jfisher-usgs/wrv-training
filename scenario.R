## ----set_knitr_options, include=FALSE------------------------------------
knitr::opts_chunk$set(eval=TRUE, dpi=100, fig.path="figures/")

## ----set_wd, eval=TRUE---------------------------------------------------
setwd("D:/WORK/Repos/wrv-training")

## ----load_pkgs, message=FALSE, results="hide", eval=TRUE-----------------
packages <- c("wrv", "inlmisc", "raster", "rgdal", "leaflet")
lapply(packages, library, character.only = TRUE)

## ----leaflet_idle, fig.width=5.00, fig.height=5.00, eval=TRUE------------
crs <- CRS("+init=epsg:4326")
irr <- spTransform(irr.lands[["2002"]], crs)
irr$Status <- factor(irr$Status, levels = c(levels(irr$Status), "idled"))
idx <- c(980, 981, 993, 1002, 1005, 1007, 1044, 1049, 1052,
         1059, 1061, 1065, 1066, 1067, 1073, 1074, 1076)
irr$Status[idx] <- "idled"
ll <- colMeans(coordinates(irr[idx, ]))
ext <- spTransform(alluvium.extent, crs)
irr <- crop(irr, extent(ext))
Pal <- colorFactor(c("#1B9E77", "#7570B3", "#D95F02"), irr$Status)
map <- leaflet()
map <- setView(map, lng = ll[1], lat = ll[2], zoom = 12)
url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer?"
opt <- WMSTileOptions(format = "image/png", transparent = TRUE)
map <- addWMSTiles(map, url, options = opt, layers = "0")
map <- addPolygons(map, data = irr, stroke = FALSE, fillOpacity = 0.6,
                   color = Pal(irr$Status))
map <- addPolylines(map, data = ext, weight = 3, color = "#000000")
map <- addLegend(map, pal = Pal, values = irr$Status, opacity = 0.6)
map

## ----read_code_chunks, eval=TRUE-----------------------------------------
file <- system.file("doc", "sir20165080_AppendixC.R", package = "wrv")
app.c.chunks <- ReadCodeChunks(file)
file <- system.file("doc", "sir20165080_AppendixD.R", package = "wrv")
app.d.chunks <- ReadCodeChunks(file)

## ----show_example_code_chunk, comment=NA, eval=TRUE----------------------
chunk <- app.d.chunks[["write_modflow_input"]]
print(chunk)

## ----eval_setup, message=FALSE-------------------------------------------
eval(parse(text = app.c.chunks[["setup"]]))

## ----init_extdata, message=FALSE, results="hide"-------------------------
folder <- "extdata"
files <- list.files(folder, full.names = TRUE, recursive = TRUE)
dirs <- file.path(tempdir(), unique(dirname(files)))
sapply(dirs, function(i) dir.create(i, recursive = TRUE))
file.copy(files, file.path(tempdir(), files))
dir.in <- file.path(tempdir(), "extdata")
files <- list.files(dir.in, pattern = "*.zip$", full.names = TRUE, recursive = TRUE)
for (i in files) unzip(i, exdir = dirname(i))

## ----create_dir_out, message=FALSE---------------------------------------
dir.out <- "data"
dir.create(dir.out, showWarnings = FALSE)

## ----eval_unit_crs_spatial_temporal--------------------------------------
chunk.names <- c("unit_conversions", "crs", "spatial", "high_res_spatial", "temporal")
eval(parse(text = unlist(app.c.chunks[chunk.names])))

## ----eval_comb_sw_irr----------------------------------------------------
eval(parse(text = app.c.chunks[["comb_sw_irr_1"]]))

## ----eval_pod_gw---------------------------------------------------------
eval(parse(text = app.c.chunks[["pod_gw_1"]]))

## ----eval_pod_wells------------------------------------------------------
eval(parse(text = unlist(app.c.chunks[c("pod_wells_1", "pod_wells_2")])))

## ----eval_irr_lands------------------------------------------------------
eval(parse(text = app.c.chunks[["irr_lands_1"]]))

## ----eval_et_1-----------------------------------------------------------
eval(parse(text = unlist(app.c.chunks[c("et_1", "et_2")])))

## ----eval_entity_components----------------------------------------------
eval(parse(text = unlist(app.c.chunks[paste0("entity_components_", 1:3)])))

## ----eval_rs_entities----------------------------------------------------
eval(parse(text = app.c.chunks[["rs_entities_1"]]))

## ----eval_rs_rech_non_irr------------------------------------------------
eval(parse(text = app.c.chunks[["rs_rech_non_irr_1"]]))

## ----clean_env, results="hide", eval=TRUE--------------------------------
rm(list = ls()[!ls() %in% "app.d.chunks"])
files <- list.files("data", pattern = "*.rda$", full.names = TRUE)
lapply(files, load, envir = .GlobalEnv)

## ----copy_archive_to_model, results="hide"-------------------------------
dir.create("model", showWarnings = FALSE)
files <- list.files("archive", full.names = TRUE)
file.copy(files, "model", overwrite = TRUE)

## ----set_id, results="hide", eval=TRUE-----------------------------------
id <- "wrv_mfusg"

## ----load_model, results="hide", eval=TRUE-------------------------------
load("model/model.rda")

## ----assign_rs, eval=TRUE------------------------------------------------
rs.model <- rs

## ----update_water_budget, results="hide"---------------------------------
UpdateWaterBudget("model", id, qa.tables = "english", pod.wells = pod.wells,
                  comb.sw.irr = comb.sw.irr, et = et,  pod.gw = pod.gw,
                  entity.components = entity.components, rs.entities = rs.entities)

## ----run_modflow---------------------------------------------------------
wd <- setwd("model")
system2(file.path(getwd(), "RunModflow.bat"), stdout = FALSE, stderr = FALSE)
setwd(wd)

## ----eval_budget_archive, message=FALSE, warning=FALSE, eval=TRUE--------
dir.run <- "archive"  # code below is dependent on this object
eval(parse(text = unlist(app.d.chunks[paste0("read_budget_", 1:2)])))

## ----save_budget_archive, eval=TRUE--------------------------------------
budget1 <- c("Water-table recharge"             = mean(d.rech$flow.in),
             "Streamflow losses"                = mean(d.river$flow.in),
             "Tributary basin underflow"        = mean(d.trib$flow),
             "Water-table discharge"            = mean(d.rech$flow.out),
             "Streamflow gains"                 = mean(d.river$flow.out),
             "Production well pumping"          = mean(d.well$flow),
             "Stanton Crossing outlet boundary" = mean(d.drain.1$flow),
             "Silver Creek outlet boundary"     = mean(d.drain.2$flow))
budget1 <- setNames(as.integer(abs(budget1)) * 0.296106669, names(budget1))

## ----eval_budget_scenario, message=FALSE, warning=FALSE, eval=TRUE-------
dir.run <- "model"
eval(parse(text = unlist(app.d.chunks[paste0("read_budget_", 1:2)])))

## ----save_budget_scenario, eval=TRUE-------------------------------------
budget2 <- c("Water-table recharge"             = mean(d.rech$flow.in),
             "Streamflow losses"                = mean(d.river$flow.in),
             "Tributary basin underflow"        = mean(d.trib$flow),
             "Water-table discharge"            = mean(d.rech$flow.out),
             "Streamflow gains"                 = mean(d.river$flow.out),
             "Production well pumping"          = mean(d.well$flow),
             "Stanton Crossing outlet boundary" = mean(d.drain.1$flow),
             "Silver Creek outlet boundary"     = mean(d.drain.2$flow))
budget2 <- setNames(as.integer(abs(budget2)) * 0.296106669, names(budget2))

## ----table_budget, eval=TRUE---------------------------------------------
d <- data.frame(rate1 = budget1, rate2 = budget2)
d$direction <- c("**Inflow**", "", "", "**Outflow**", "", "", "", "")
d$component <- names(budget1)
d$diff <- budget2 - budget1
d$pchange <- ((budget2 - budget1) / budget1) * 100
d <- rbind(d[1:3, ], NA, d[4:8, ], NA, NA)
d$component[nrow(d)] <- "**Inflow - Outflow**"
d$rate1[nrow(d)] <- sum(budget1[1:3]) - sum(budget1[5:8])
d$rate2[nrow(d)] <- sum(budget2[1:3]) - sum(budget2[5:8])
rownames(d) <- NULL
d <- d[, c("direction", "component", "rate1", "rate2", "diff", "pchange")]
colnames(d) <- c("", "Component", "Archived rate<br>(acre-ft/yr)",
                 "Scenario rate<br>(acre-ft/yr)", "Difference<br>(arce-ft/yr)",
                 "Percent<br>change")
d[, 3] <- formatC(d[, 3], format = "f", digits = 0, big.mark = ",")
d[, 4] <- formatC(d[, 4], format = "f", digits = 0, big.mark = ",")
d[, 5] <- formatC(d[, 5], format = "f", digits = 0, big.mark = ",")
d[, 6] <- formatC(d[, 6], format = "f", digits = 1, big.mark = ",")
d[, 1:2][is.na(d[, 1:2])]  <- ""
d[, 3:6][d[, 3:6] == "NA"] <- ""
knitr::kable(d, format = "markdown", padding = 0, booktabs = TRUE, digits = 0,
             row.names = FALSE, col.names = colnames(d), align = "llrrrr")

## ----eval_head_archive, message=FALSE, warning=FALSE, eval=TRUE----------
dir.run <- "archive"
eval(parse(text = app.d.chunks[["read_head"]]))
head1 <- rs.heads.lay1

## ----eval_head_scenario, message=FALSE, warning=FALSE, eval=TRUE---------
dir.run <- "model"
eval(parse(text = app.d.chunks[["read_head"]]))
head2 <- rs.heads.lay1

## ----leaflet_head, fig.width=5.00, fig.height=5.00, eval=TRUE------------
ext <- spTransform(alluvium.extent, CRS("+init=epsg:4326"))
r <- head2[["2007-08-16"]] - head1[["2007-08-16"]]
r[] <- round(r[] * 3.28084, digits = 3)
r <- projectRaster(r, crs = CRS("+init=epsg:4326"), method = "ngb")
Pal <- colorNumeric("Spectral", r[], na.color = "transparent")
map <- leaflet()
ll  <- coordinates(ext)
map <- setView(map, lng = ll[1], lat = ll[2], zoom = 11)
url <- "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer?"
opt <- WMSTileOptions(format = "image/png", transparent = TRUE)
map <- addWMSTiles(map, url, options = opt, layers = "0")
map <- addRasterImage(map, r, colors = Pal, opacity = 0.8)
map <- addPolylines(map, data = ext, weight = 3, color = "#000000")
map <- addLegend(map, pal = Pal, values = r[], opacity = 0.8,
                 title = "Head Diff.<br/>(feet)")
ll <- c(-114.222664, 43.383047)
txt <- sprintf("<b>Longitude:</b> %s<br/><b>Latitude:</b> %s", ll[1], ll[2])
icon <- icons(iconUrl = "markers/marker-blue.png",
              iconWidth = 34, iconHeight = 34, iconAnchorX = 17, iconAnchorY = 34)
map <- addMarkers(map, lng = ll[1], lat = ll[2], popup = txt, icon = icon)
map

## ----graph_head_diff, fig.width=7.16, fig.height=3.50, eval=TRUE---------
pnt <- data.frame(lon = ll[1], lat = ll[2])
coordinates(pnt) <- c("lon", "lat")
proj4string(pnt) <- CRS("+init=epsg:4326")
pnt <- spTransform(pnt, crs(hill.shading))
d <- t(extract(head2, pnt) - extract(head1, pnt))
d <- data.frame(Date = as.Date(rownames(d)), difference = d)
ylab <- paste("Hydraulic head difference, in", c("meters", "feet"))
PlotGraph(d, ylab = ylab, col = "#025D8C", conversion.factor = 3.28084,
          center.date.labels = TRUE)

