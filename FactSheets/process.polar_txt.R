# ---------------------------------------------------------------------------- #
# Phenobox ------------------------------------------------------------------- #
# Processing of polar_txt output --------------------------------------------- #
# This script is used by build fact_sheet ------------------------------------ #
# If polar coordinates have to be analysed seperately, ----------------------- #
# uncomment lines at the end ------------------------------------------------- #
# Marius Hodel --------------------------------------------------------------- #
# marius.hodel@agroscope.admin.ch -------------------------------------------- #
# 2023-12-11 ----------------------------------------------------------------- #
#----------------------------------------------------------------------------- #

# Required libraries
# library(dplyr)

process.polar = function(path, dir) {
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
  cart.apple <- as.data.frame(matrix(nrow = 0, ncol = 5))
  names(cart.apple) <- c("x", "y", "cam", "appleNR", "badge")
  cart.side.badge <- as.data.frame(matrix(nrow = 0, ncol = 4))
  names(cart.side.badge) <- c("x", "y", "trait", "badge")
  polar.side.badge <- as.data.frame(matrix(nrow = 360, ncol = 1))
  colnames(polar.side.badge) <- "t"
  
  for (d in seq(length(dir))) {
    
    badge <- dir[d]
    if (length(dir(paste0(badge, "/polar_txt/"))) == 0) next
    if (syst %in% c("linux", "osx")) {
      polar <- read.table(pipe(paste0("cat ", badge, "/polar_txt/* | grep -v 'angle' ")))
    }
    if (syst == "windows") {
      polar <- read.table(pipe(paste0("type ", badge, "\\polar_txt\\* | findstr -v angle")), 
                          colClasses = "numeric")
    }
    
    colnames(polar) <- c("t", "rad_cam1", "rad_cam2", "rad_cam3", "rad_cam4", "rad_cam5")
    
    # little hack in order to get a closed shape in the end
    polar[polar == 179.5] <- 180
    polar[polar == -179.5] <- -180
    
    polar$t <- polar$t*pi/180*(-1) # * -1 to turn around the apple shape by 180 degrees
    
    # Calculate average side shape (per apple)
    polar$rAverage <- rowMeans(polar[,c(3:6)])
    
    cart <- as.data.frame(matrix(nrow = 0, ncol = 3))
    names(cart) <- c("x", "y", "cam")
    
    # Transform to carthesian coordinates
    for (i in 2:7) {
      x <- polar[,i]*cos(polar$t)
      y <- polar[,i]*sin(polar$t)
      if (i < 7) {
        tmp <- cbind(x, y, paste("cam", i - 1, sep = ""))
      } else {
        tmp <- cbind(x, y, "sideAverage")
      }
      cart[c((length(x)*(i - 2) + 1):((i - 1)*length(x))),] <- tmp
    }
    
    colnames(cart) <- c("x", "y", "cam")
    cart$x <- as.numeric(as.character(cart$x))
    cart$y <- as.numeric(as.character(cart$y))
    
    # Add appleNR
    nrapples <- nrow(cart)/(6*360)
    cart$apple <- polar$apple <- rep(seq(nrapples), each = 360)
    cart$apple <- as.factor(cart$apple)
    polar$apple <- as.factor(polar$apple)
    
    # Sideshapes
    polar %>% group_by(c(t)) %>% 
      summarize(mean = mean(rAverage), .groups = "drop") %>% 
      as.data.frame -> full.badge.ave.polar
    colnames(full.badge.ave.polar) <- c("t", "rAverage.tot")
    
    # Add overall side shape average per badge to final df
    # First badge: Add radians
    if (d == 1) polar.side.badge[,'t'] <- full.badge.ave.polar[,1]
    polar.side.badge[,badge] <- full.badge.ave.polar$rAverage.tot
    
    # Transform overall side average to carthesian coordinates
    x <- full.badge.ave.polar[,2]*cos(full.badge.ave.polar$t)
    y <- full.badge.ave.polar[,2]*sin(full.badge.ave.polar$t)
    full.badge.ave.cart <- as.data.frame(cbind(x, y, "totAverage"))
    colnames(full.badge.ave.cart) <- c("x", "y", "trait")
    full.badge.ave.cart$x <- as.numeric(as.character(full.badge.ave.cart$x))
    full.badge.ave.cart$y <- as.numeric(as.character(full.badge.ave.cart$y))
    full.badge.ave.cart$trait <- as.character(full.badge.ave.cart$trait)
    
    # Add badge to dfs
    cart$badge <- badge
    full.badge.ave.cart$badge <- badge
    # get non-averaged polar coordinates
    polar$badge <- badge
    if (d == 1) {
      polar.side.badge.raw <- as.data.frame(matrix(nrow = 0, ncol = 9))
      names(polar.side.badge.raw) <- names(polar)
      polar.side.badge.raw[(nrow(polar.side.badge.raw) + 1):(nrow(polar) + nrow(polar.side.badge.raw)),] <- polar
    } else{
      polar.side.badge.raw <- rbind(polar.side.badge.raw,polar)
    }
    cart.apple[(nrow(cart.apple) + 1):(nrow(cart) + nrow(cart.apple)),] <- cart
    cart.side.badge[(nrow(cart.side.badge) + 1):(nrow(full.badge.ave.cart) + nrow(cart.side.badge)),] <- 
      full.badge.ave.cart
    if (d %% 10 == 0) message(paste("Polar coordinates of", d, "out of", length(dir), "badges processed"))
  }
  return(list(cart.apple = cart.apple, cart.side.badge = cart.side.badge, polar.side.badge = polar.side.badge,
              polar.side.badge.raw = polar.side.badge.raw))
}

## Uncomment if you need to have the text files
# path <- getwd()
# dir <- list.files()
# dir <- dir[!(grepl("\\.txt", dir))]
# dir <- dir[!(dir %in% c("processed", "crop_tiled"))]
# # dir <- dir[1:10]
# 
# out.polar <- process.polar(path, dir)
# 
# f <- file("processed/cart.apple.txt", open="wb")
# write.table(out.polar$cart.apple, file = f, quote = F, eol = "\n", row.names = F, sep="\t")
# close(f)
# 
# f <- file("processed/cart.side.badge.txt", open="wb")
# write.table(out.polar$cart.side.badge, file = f, quote = F, eol = "\n", row.names = F, sep="\t")
# close(f)
# 
# f <- file("processed/all.badge.ave.polar.txt", open="wb")
# write.table(out.polar$polar.side.badge, file = f, quote = F, eol = "\n", row.names = F, sep="\t")
# close(f)
# 
# ### added on 20200929 to get non-averaged polar coord
# f <- file("processed/all.badge.polar.raw.txt", open="wb")
# write.table(out.polar$polar.side.badge.raw, file = f, quote = F, eol = "\n", row.names = F, sep="\t")
# close(f)
# ####
