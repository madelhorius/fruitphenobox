# ---------------------------------------------------------------------------- #
# Phenobox ------------------------------------------------------------------- #
# Produces fact sheets ------------------------------------------------------- #
# Output: parsed data txt files and ------------------------------------------ #
# cultivar_summaryplots.pdf and/or badge_summaryplots.pdf" ------------------- #
# Marius Hodel --------------------------------------------------------------- #
# 2019-11-14 ----------------------------------------------------------------- #
# Adapted: 2024-03-18 -------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Parameters ====================================================================
# ---------------------------------------------------------------------------- #

# Path with matlab output
path.affe_output <- "~/phenobox/affe_output/"
# File with field plan and required colnames
exp.file <- "~/phenobox/pseudoplan.txt"
delimiter <- "\t"
badge <- "id"
tree <- "Position"
variety <- "Cultivar_label"
row <- "Row"
uniq.cult <- "Genotype"

# Experiment information
location <- "Wädenswil"
year <- "2021"
# Summarize by cultivar / variety?
lsumcultivar <- F
# Did you already process polar_txt files using process.polar_txt.R?
export.polar <- F
# Did you already run process.shape.phenobox.R?
proc.shape <- F
# Did you already run process color_txt files using process.color_txt.R?
export.color <- F

# Developer Mode: Only do subset of badges. How many?
devmop <- F
if (devmop) nrdir <- 50

# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# DO NOT CHANGE anything below here if you just want to run the script
# ---------------------------------------------------------------------------- #

# Required packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
library(gtable)
library(grid)
library(qrcode)
library(magick)
library(rstudioapi)

# Set working directory
setwd(path.affe_output)

# Get path of this Rscript (only works in RStudio)
rpath <- dirname(rstudioapi::getSourceEditorContext()$path)

# Source needed Rscripts (functions)
source(paste0(rpath, "/process.polar_txt.R"))
source(paste0(rpath,"/define.shape.per.badge.R"))
source(paste0(rpath, "/get.exp.info.R"))
source(paste0(rpath, "/process.color_txt.R"))

# Check which badges are in this folder, save in vector
# Only badge directories and directories "processed" and "crop_tiled" are allowed 
# to be in working directory
dir <- list.dirs(recursive = F, full.names = F)
dir <- dir[!(dir %in% c("processed", "crop_tiled"))]

# Use only part of output (developer mode)
if (devmop) dir <- dir[1:nrdir]

# List all images in crop_tiled
img.files <- list.files("crop_tiled")

# Get field plan information
# Requires Rscript get.exp.info.R
# -------------------------------

field.info <- get.exp.info(exp.file, delimiter, badge, row, tree, variety, uniq.cult)
field.info <- field.info[field.info$badge %in% dir,]

# Read in traits from text files
# ------------------------------

# If not done before, process original text files first
if (!proc.shape) {
  source(paste0(rpath, "/process.shape.phenobox.R"))
  process.shape(path.affe_output)
}

# Read in traits (produced by process.shape.phenobox.R)
traits <- read.table("processed/traits.txt", header = T, encoding = "UTF-8", sep = "\t",
                     colClasses = c(rep("character", 2), rep("numeric", 9)))
ctraits <- c("badge", "weight_g", "diameter_mean_cm", "height_mean_cm", "width_mean_cm")
traits %>% group_by(badge) %>% select(all_of(ctraits)) %>% 
  summarise_at(ctraits[-1], mean, na.rm = T, .groups = "drop") %>% 
  as.data.frame -> btraits
btraits$badge <- as.character(btraits$badge)

traits <- merge(field.info, traits, by = "badge")
traits <- traits[,c(1, 5:15)]
# traits in long format in order to plot
trait.long <- gather(traits, trait, measurement, ShapeClass_top:symmetry_SD)
# Symmetry
if (lsumcultivar) {
  traits[traits$symmetry_mean < 0.1,] %>% group_by(uniq.cult) %>% 
    summarise(sym = mean(symmetry_mean), .groups = "drop") %>% 
    as.data.frame -> sym
} else {
  traits[traits$symmetry_mean < 0.1,] %>% group_by(badge) %>% 
    summarise(sym = mean(symmetry_mean), .groups = "drop") %>% 
    as.data.frame -> sym
}
sym$sym.char <- NA
sym$sym.char[sym$sym < 0.02] <- "symmetric"
sym$sym.char[sym$sym > 0.02] <- "normal"
sym$sym.char[sym$sym > 0.03] <- "asymmetric"

# Prepare vectors for trait gtable
trait <- c("Nr Apples:", "Top Shape Class:", "Symmetry:", "Shape category:")

# Get most common shape per badge or by uniq.cult
# Requires Rscript define.shape.per.badge.R
# -----------------------------------------

if (lsumcultivar) {
  shape.badge <- what_shape("processed/corshapes.txt", field.info)
} else {
  shape.badge <- what_shape("processed/corshapes.txt")
}



# Define themes for tables / text on fact sheet
# ---------------------------------------------
trtheme <-
  ttheme_minimal(
    core = list(
      fg_params = list(hjust = 0, x = 0, fontface = matrix(c(2,1), ncol = 4, nrow = 2, byrow = F))))
rowtreetheme <- 
  ttheme_minimal(
    core = list(
      fg_params = list(hjust = 0, x = 0, fontface = matrix(c(2,1), ncol = 2, nrow = 2, byrow = T))))
locyeartheme <- 
  ttheme_minimal(
    core = list(
      fg_params = list(hjust = 0, x = 0, fontface = matrix(c(2,1), ncol = 2, nrow = 2, byrow = T))),
    base_size = 8, padding = unit(c(1,1), "mm"))
phototimetheme <-
  ttheme_minimal(
    core = list(fg_params = list(parse = T, hjust = 0, x = 0)),
    base_size = 8, padding = unit(c(1,1), "mm"))

# Process polar output
# Rscript "process.polar_txt.R" needs to be sourced
# -------------------------------------------------

if (export.polar) {
  # Read in if already there
  cart.side.badge <- 
    read.table("processed/cart.side.badge.txt", header = T, sep = "\t",
               colClasses = c("numeric", "numeric", "character", "character"))
  cart <- 
    read.table("processed/cart.apple.txt", header = T, sep = "\t",
               colClasses = c("numeric", "numeric", "character", "character", "character"))
} else {
  # Process polar txt output from matlab output
  path = getwd()
  out.polar <- process.polar(path, dir)
  cart <- out.polar$cart.apple 
  cart$appleNR <- as.factor(cart$appleNR)
  cart.side.badge <- out.polar$cart.side.badge
}

# Merge with field info
cart <- left_join(cart, field.info, by = "badge")[,c(1:5,9)]
cart.side.badge <- left_join(cart.side.badge, field.info, by = "badge")[,c(1:4,8)]

# Process / Read in color txt files for color histograms
# Rscript "process.color_txt.R required
# ------------------------------------------------------

if (export.color) {
  # Just read in if already there
  colhist <- read.table("processed/allcolhist.txt", header = T, sep = "\t",
                        colClasses = c(rep("numeric", 13), "character", "character"))
} else {
  path <- getwd()
  colhist <- process.color(path, dir)
}
# Merge with field info
colhist <- 
  left_join(colhist, field.info, by = "badge")[
    ,c(1:ncol(colhist),(ncol(colhist) + ncol(field.info) - 1))]
# Keep only mean values
colhist <- colhist[,c('value', 'H_mean', 'S_mean', 'appleNR', 'badge', 'uniq.cult')]
# Calculate mean values per badge or cultivar
if (lsumcultivar) {
  na.omit(colhist) %>% group_by(uniq.cult, value) %>% 
    summarise(H_mean = mean(H_mean), .groups = "drop") %>% 
    as.data.frame -> colhist
} else {
  colhist %>% group_by(badge, value) %>% 
    summarise(H_mean = mean(H_mean), .groups = "drop") %>% 
    as.data.frame -> colhist
}

# If results should be summarized by cultivar, use uniq.cult instead of badge
# and open a different pdf
# ---------------------------------------------------------------------------

if (lsumcultivar) {
  badge.o.cult.uniq <- unique(field.info$uniq.cult[field.info$badge %in% dir])
  b.or.c <- 'uniq.cult'
  
  # Prepare location plot
  tree <- rep((min(field.info$tree):max(field.info$tree)), 
              max(field.info$row) - min(field.info$row) + 1)
  row <- rep(min(field.info$row):max(field.info$row), 
             max(field.info$tree) - min(field.info$tree) + 1)
  field <- as.data.frame(cbind(row, tree))
  
  ggfield <- ggplot(data = field, aes(y = tree, x = row)) +
    geom_point(col = "seagreen", size = 0.1) +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), panel.grid = element_blank()) +
    theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))
  
  # Open pdf
  pdf("processed/cultivar_summaryplots.pdf", height = 11, width = 8, onefile = TRUE)
} else {
  badge.o.cult.uniq <- dir
  b.or.c <- 'badge'
  
  # Open pdf
  pdf("processed/badge_summaryplots.pdf", height = 11, width = 8, onefile = TRUE)
}

# -----------------------------
# Loop badges to get fact sheet
# -----------------------------
for (d in seq(length(badge.o.cult.uniq))) {
  
  # Which badge / uniq.cult?
  badge <- badge.o.cult.uniq[d]
  
  # Check if folder is not emty (skip badge if it is)
  if (nrow(na.omit(cart[cart$cam == "cam1" & cart[b.or.c] == badge,])) == 0) next
  
  # Create plots required for the fact sheet
  # ----------------------------------------
  
  # Top shape Plots
  ggtop <- 
    ggplot(data = cart[cart$cam == "cam1" & cart[b.or.c] == badge,],
           aes(x = x, y = y, col = interaction(badge, appleNR, sep = "-"))) +
    geom_path() +
    theme_classic() +
    coord_fixed() +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(),axis.text = element_blank(), 
          legend.position = "none")
  
  # Plot side shape averages
  datToPlot <- cart[cart$cam == "sideAverage" & cart[b.or.c] == badge,]
  ggside <- 
    ggplot(data = datToPlot, 
           aes(x = x, y = y, col = interaction(appleNR, badge, sep = "-"))) +
    geom_path() +
    theme_classic() +
    coord_fixed(xlim = c(min(datToPlot$x), max(datToPlot$x)), 
                ylim = c(min(datToPlot$y), max(datToPlot$y))) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), axis.text = element_blank(), 
          legend.position = "none")
  
  # Plot overall badge side shape average
  ggside.av <- 
    ggplot(data = cart.side.badge[cart.side.badge[b.or.c] == badge,], 
           aes(x = x, y = y, col = badge)) +
    geom_path() +
    theme_classic() +
    coord_fixed(xlim = c(min(datToPlot$x), max(datToPlot$x)), 
                ylim = c(min(datToPlot$y), max(datToPlot$y))) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), axis.text = element_blank(), 
          legend.position = "none")
  
  # Make trait plots
  vtraits <- c("width_mean_cm", "height_mean_cm")
  ylimmin <- min(na.omit(trait.long$measurement[trait.long$trait %in% vtraits]))
  ylimmax <- max(na.omit(trait.long$measurement[trait.long$trait %in% vtraits]))
  
  ggmeasure <- 
    ggplot(data = trait.long[trait.long[b.or.c] == badge & 
                               trait.long$trait %in% vtraits,]) +
    geom_boxplot(aes(y = measurement, x = trait, fill = trait)) +
    theme_classic() +
    theme(axis.title.x = element_blank(), legend.position = "none") +
    ylab("measure [cm]") +
    ylim(c(ylimmin, ylimmax)) +
    scale_x_discrete(labels = c("width_mean_cm" = "mean width per apple",
                                "height_mean_cm" = "mean height per apple"))
  # weight
  ylimmin <- min(trait.long$measurement[trait.long$trait == "weight_g"])
  ylimmax <- max(trait.long$measurement[trait.long$trait == "weight_g"])
  
  ggweight <- 
    ggplot(data = trait.long[trait.long$trait == "weight_g" & 
                               trait.long[b.or.c] == badge,]) +
    geom_boxplot(aes(y = measurement, x = trait), fill = "seagreen") +
    theme_classic() +
    scale_y_continuous(position = "right", limits = c(ylimmin, ylimmax), 
                       name = "weight [g]") +
    theme(axis.title.x = element_blank(), 
          axis.title.y.right = element_text(margin = margin(
            t = 0, r = 0, b = 0, l = 7))) +
    scale_x_discrete(labels = c("weight_g" = "weight"))
  
  # Make color histogram plot
  tmp <- colhist[colhist[b.or.c] == badge,]
  tmp$value <- tmp$value + tmp$value[1]
  add <- mean(c(head(tmp,1)$H_mean, tail(tmp,1)$H_mean))
  tmp <- rbind(c(tmp$badge[1], 0, add),tmp)
  
  tmp$value <- as.numeric(tmp$value)
  tmp$H_mean <- as.numeric(tmp$H_mean)
  
  ggcolhist <- ggplot(data = tmp, aes(x = value, y = H_mean)) +
    xlim(-0.01, 0.35) +
    geom_rect(mapping = aes(xmin = value, xmax = value + 0.027778, ymin = 0, 
                            ymax = max(H_mean)),
              fill = hsv(tmp$value, 1, 1)) +
    geom_ribbon(aes(ymin = H_mean, ymax = max(H_mean) + 0.1), fill = "white") +
    # White rectangles to remove colored lines in pdf output
    geom_rect(mapping = aes(xmin = 0.3, xmax = 0.35, ymin = 0, ymax = max(H_mean)),
              fill = "white") +
    geom_rect(mapping = aes(xmin = -0.01, xmax = 0, ymin = 0, ymax = max(H_mean)),
              fill = "white") +
    geom_line() +
    theme_classic() +
    labs(x = "Hue value", y = "Intensity")
  
  # Combine boxplots
  lay <- rbind(c(rep(1,13),rep(2,8)))
  ggtraits <- arrangeGrob(ggmeasure, ggweight, layout_matrix = lay)
  
  # Create table with required information
  nrapples <- nrow(traits[traits[b.or.c] == badge,])
  topclass <- round(mean(na.omit(traits$ShapeClass_top[traits[b.or.c] == badge])),0)
  symmetry <- sym[sym[,1] == badge,3]
  sideshape <- shape.badge[shape.badge[b.or.c] == badge, "shape_en"]
  value <- c(nrapples, topclass, symmetry, sideshape)
  trtable <- as.data.frame(
    matrix(c(trait[1], value[1], trait[2], value[2], trait[3], value[3],
             trait[4], value[4]), ncol = 4, nrow = 2))
  gtab <- tableGrob(trtable, theme = trtheme, rows = NULL, cols = NULL)
  
  # Combination of plots to a single fact sheet
  # -------------------------------------------
  
  lay <- rbind(c(1,1,1),c(2,3,4))
  ggcom <- arrangeGrob(ggtraits, ggtop, ggside, ggside.av, layout_matrix = lay)
  
  # Add gtab to grid
  ggcom %>% gtable_add_rows(heights = unit(0.2, "null"), pos = 0) %>%
    gtable_add_grob(gtab,1,1,1,3) -> ggcom
  # Add titles for shapes
  titles <- list(textGrob(label = "Top shapes", y = 0.2),
                 textGrob(label = "Average side shapes", y = 0.2),
                 textGrob(label = "Overall average side shape", y = 0.2))
  
  ggcom %>% gtable_add_rows(heights = unit(0.2, "null"), pos = 2) %>% 
    gtable_add_grob(titles, c(rep(3,3)), c(1,2,3)) -> ggcom
  
  # Add header including QR-Code, Agroscope logo and field information
  var <- field.info$variety[field.info[b.or.c] == badge][1]
  title <- textGrob(label = var, gp = gpar(cex = 30/(nchar(var) + 10), font = 2))
  
  # Add field plot if summarize per cultivar (shows location of trees)
  if (lsumcultivar) {
    df <- as.data.frame(cbind(field.info$row[field.info[b.or.c] == badge],
                              field.info$tree[field.info[b.or.c] == badge]))
    colnames(df) <- c("row", "tree")
    ggfield2 <- ggfield +
      geom_point(data = df, col = "red", size = 1, shape = 1)
    gtabrowtree <- arrangeGrob(ggfield2)
    # Badges instead of QR codes
    qrtext <- paste(field.info[field.info$uniq.cult == badge,]$badge, collapse = "/")
    qr <- -1*((qr_code(gsub("_",".", qrtext))) - 1)
    
    # Combine title, fieldplot and qr
    lay <- rbind(c(1,2,2,3), c(1,2,2,3))
    header <- arrangeGrob(title, gtabrowtree, rasterGrob(qr, interpolate = F),
                          layout_matrix = lay)
  } else {
    qr <- -1*((qr_code(gsub("_",".", badge))) - 1)
    # Tree & Row
    rowtree <- as.data.frame(matrix(
      c("Row:", "Tree:", field.info$row[field.info[b.or.c] == badge],
        field.info$tree[field.info[b.or.c] == badge]), ncol = 2, nrow = 2))
    gtabrowtree <- 
      tableGrob(rowtree, theme = rowtreetheme, rows = NULL, cols = NULL)
    # Combine variety and tree & row
    gtabrowtree %>% gtable_add_cols(widths = unit(1, "null"), pos = 0) %>% 
      gtable_add_grob(title, t = 1, b = 2, l = 1, r = 1) -> gtabrowtree
    
    # Combine title, fieldplot and qr
    lay <- rbind(c(1,1,2), c(1,1,2))
    header <- arrangeGrob(gtabrowtree, rasterGrob(qr, interpolate = F), 
                          layout_matrix = lay)
  }
  
  ggcom %>% gtable_add_rows(heights = unit(0.1, "null"), pos = 0) %>% 
    gtable_add_rows(heights = unit(0.2, "null"), pos = 0) %>% 
    gtable_add_grob(header, t = 1, b = 1, l = 1, r = 3) -> ggcom
  
  # Add frames / lines
  ggcom %>% 
    gtable_add_grob(segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5, 
                                 gp = gpar(fill = NA, lwd = 1)),
                    t =  2, b = 2, l = 1, r = 3) %>%
    gtable_add_rows(heights = unit(0.1, "null"), pos = 3) %>% 
    gtable_add_grob(segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5, 
                                 gp = gpar(fill = NA,lwd = 1)),
                    t =  4, b = 4, l = 1, r = 3) -> ggcom
  
  ggcom %>% gtable_add_rows(heights = unit(0.01, "null"), pos = 5) %>% 
    gtable_add_grob(segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5, 
                                 gp = gpar(fill = NA, lwd = 1)),
                    t =  6, b = 6, l = 1, r = 3) %>%
    gtable_add_rows(heights = unit(0.1, "null"), pos = 8) %>% 
    gtable_add_grob(segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5, 
                                 gp = gpar(fill = NA, lwd = 1)),
                    t =  9, b = 9, l = 1, r = 3) -> ggcom
  
  
  # Read in png, remove black background, scale down and make raster
  img <- image_read(
    paste("crop_tiled", img.files[
      grepl(field.info[field.info[b.or.c] == badge,]$badge[1], img.files)][1], 
      sep = "/"))
  img <- image_transparent(img, "black")
  img <- image_scale(img, "800")
  grob.img <- rasterGrob(img)
  
  # Combine header, ggplots and example image
  lay <- cbind(c(rep(1,7), rep(2,4)), c(rep(1,7), rep(2,4)), c(rep(1,7), rep(2,4)))
  ggcom %>% arrangeGrob(grob.img, layout_matrix = lay) -> ggcom
  
  # Add colorhistogram
  grobGGcolhist <- ggplotGrob(ggcolhist)
  grobGGcolhist <- gtable_add_cols(grobGGcolhist, widths = unit(0.1, "null"), 8) 
  ggcom %>% gtable_add_grob(grobGGcolhist, t = 10, b = 11, r = 3, l = 3) -> ggcom
  
  # Add Experiment information at bottom of page
  start <- as.character(min(traits[traits[b.or.c] == badge,]$date))
  startdate <- paste(substr(start,1,4), substr(start,5,6), 
                     substr(start,7,8), sep = "-")
  starttime <- paste(substr(start,9,10), substr(start,11,12), 
                     substr(start,13,14), sep = ":")
  
  end <- as.character(max(traits[traits[b.or.c] == badge,]$date))
  enddate <- paste(substr(end,1,4), substr(end,5,6), 
                   substr(end,7,8), sep = "-")
  endtime <- paste(substr(end,9,10), substr(end,11,12), 
                   substr(end,13,14), sep = ":")
  
  if (startdate == enddate) {
    phototime <- paste("Photos taken on", startdate, "\nbetween", starttime, 
                       "and", endtime)
    phototime <- strsplit(phototime, "\\n")[[1]]
  } else {
    phototime <- paste("Photos taken between", startdate, starttime, "\nand", 
                       enddate, endtime)
  }
  grobPhototime <- tableGrob(as.matrix(phototime), theme = phototimetheme)
  
  locyear <- as.data.frame(matrix(c("Location:", "Year:", location, year), 
                                  ncol = 2, nrow = 2))
  gtablocyear <- tableGrob(locyear, theme = locyeartheme, rows = NULL, cols = NULL)
  footer <- arrangeGrob(gtablocyear, grobPhototime, 
                        layout_matrix = rbind(c(1,2,2,2), c(1,2,2,2)))
  
  ggcom %>% gtable_add_rows(unit(0.5, "null")) %>% 
    gtable_add_grob(footer, l = 1, r = 3, t = 12, b = 12) -> ggcom
  
  # Add padding and lines / frame
  ggcom %>% gtable_add_padding(unit(0.1, "null")) %>%
    gtable_add_rows(unit(0.1, "null"), 1) %>%
    gtable_add_rows(unit(0.1, "null"), -1) %>%
    gtable_add_rows(unit(1, "null"),3) %>%
    gtable_add_grob(segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, 
                                 gp = gpar(fill = NA, lwd = 1)),
                    t =  14, b = 14, l = 2, r = 4) %>%
    gtable_add_grob(grobs = rectGrob(gp = gpar(fill = NA,lwd = 5)),
                    t = 2, b = 15, l = 2, r = 4)  -> ggcom
  # Plot (to pdf)
  grid.arrange(ggcom)
  
  # Display loop round
  message(paste("Fact Sheet of", d, "out of", length(badge.o.cult.uniq), "badges created"))
}

dev.off()
