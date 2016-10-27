## ----set_knitr_options, include=FALSE------------------------------------
knitr::opts_chunk$set(eval=TRUE, dpi=100, fig.path="figures/")

## ----set_wd, eval=TRUE---------------------------------------------------
setwd("I:/Software/wrv-training")

## ----load_pkgs, message=FALSE, results="hide", eval=TRUE-----------------
lapply(c("wrv", "inlmisc", "raster", "rgdal"), library, character.only = TRUE)

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
rm(list = ls()[ls() != "app.d.chunks"])
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

## ----save_head_archive, eval=TRUE----------------------------------------
head1 <- rs.heads.lay1[["2007-08-16"]]

## ----eval_head_scenario, message=FALSE, warning=FALSE, eval=TRUE---------
dir.run <- "model"
eval(parse(text = app.d.chunks[["read_head"]]))

## ----save_head_scenario, eval=TRUE---------------------------------------
head2 <- rs.heads.lay1[["2007-08-16"]]

## ----map_water_tbl_diff, fig.width=7.16, fig.height=5.31, eval=TRUE------
r <- head2 - head1
r[] <- r[] * 3.28084
usr <- c(2472304, 2497015, 1343284, 1358838)
r <- crop(r, extent(usr))
zlim <- range(pretty(range(r[], na.rm = TRUE)))
ratio <- abs(zlim[1]) / diff(zlim)
Pal <- function(...) {
  Pal1 <- colorRampPalette(c("#F02311", "#F02311", "#FFD0D4"))
  Pal2 <- colorRampPalette(c("#FCFBE3", "#67A9CF", "#025D8C"))
  n1 <- round(... * ratio)
  n2 <- ... - n1
  return(c(Pal1(n1), Pal2(n2)))
}
explanation <- "Hydraulic head differnce, in feet"
PlotMap(r, xlim = usr[1:2], ylim = usr[3:4], zlim = zlim, bg.image = hill.shading,
        bg.image.alpha = 0.6, pal = Pal, dms.tick = TRUE, max.dev.dim = c(43, 56),
        rivers = list(x = streams.rivers), lakes = list(x = lakes),
        explanation = explanation, credit = "", contour.lines = list(col = "#1F1F1F"))
legend("topright", "Contour line", col="#1F1F1F", lty = 1, lwd = 0.5,
       inset = 0.02, cex = 0.7, box.lty = 1, box.lwd = 0.5, bg = "#FFFFFFCD")

