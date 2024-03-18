# ---------------------------------------------------------------------------- #
# Phenobox ------------------------------------------------------------------- #
# Processing of color_txt output --------------------------------------------- #
# This script is used by BuildFactSheets.R ----------------------------------- #
# If color histograms have to be analysed seperately, ------------------------ #
# uncomment lines at the end ------------------------------------------------- #
# Marius Hodel --------------------------------------------------------------- #
# marius.hodel@agroscope.admin.ch -------------------------------------------- #
#----------------------------------------------------------------------------- #

library(dplyr)

process.color = function(path, dir) {
  setwd(path)
  
  # Function to determine os
  get_os <- function(){
    sysinf <- Sys.info()
    if (!is.null(sysinf)) {
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  
  # Which OS?
  syst <- get_os()
  
  # Initiate output dfs
  out <- as.data.frame(matrix(nrow = 0, ncol = 15))
  for (d in seq(length(dir))) {
    
    badge <- dir[d]
    if (length(dir(paste0(badge, "/color_txt/"))) == 0) next
    if (syst %in% c("linux", "osx")) {
      colhist <- read.table(pipe(paste0("cat ", badge, "/color_txt/* | grep -v 'value' ")))
    }
    if (syst == "windows") {
      colhist <- read.table(pipe(paste0("type ", badge, "\\color_txt\\* | findstr -v value")), colClasses = "numeric")
    }
    
    colnames(colhist) <- c("value", "H_cam1","H_cam2","H_cam3","H_cam4", "H_cam5", "H_mean",
                           "S_cam1", "S_cam2", "S_cam3", "S_cam4", "S_cam5", "S_mean")
    
    # Add appleNR
    colhist$appleNR <- rep(c(1:(nrow(colhist)/36)), each = 36)
    colhist$badge <- badge
    out[(nrow(out) + 1):(nrow(colhist) + nrow(out)),] <- colhist
    if (d %% 10 == 0) message(paste("Color histograms of", d, "out of", length(dir), "badges processed"))
    }
  colnames(out) <- colnames(colhist)
  out$appleNR <- as.character(out$appleNR)
  return(out)
}

# Uncomment if you need to have the text files
# path <- getwd()
# dir <- list.files()
# dir <- dir[!(grepl("\\.txt", dir))]
# dir <- dir[!(dir %in% c("processed", "crop_tiled"))]
# # dir <- dir[1:10]
# 
# out <- process.color(path, dir)

# f <- file("processed/allcolhist.txt", open="wb")
# write.table(out, file = f, quote = F, eol = "\n", row.names = F, sep="\t")
# close(f)
