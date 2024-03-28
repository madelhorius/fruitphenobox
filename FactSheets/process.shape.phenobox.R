# ---------------------------------------------------------------------------- #
# Phenobox ------------------------------------------------------------------- #
# Reorganisation of txt output ----------------------------------------------- #
# Needs folder with txt output files as input -------------------------------- #
# Creates folder "processed" containing parsed  ------------------------------ #
# txt files (including all badgees)  ----------------------------------------- #
# Marius Hodel --------------------------------------------------------------- #
# 2023-12-11 ----------------------------------------------------------------- #
#----------------------------------------------------------------------------- #

library(dplyr)

process.shape = function(outputfolder){
   
   # List all files in matlab output folder
   files <- list.files(outputfolder, pattern = "*.txt")
   setwd(outputfolder)
   
   # Prepare output dfs
   traits <- as.data.frame(matrix(ncol = 7, nrow = 0))
   traitspercam <- as.data.frame(matrix(ncol = 9, nrow = 0))
   corshapes <- as.data.frame(matrix(ncol = 10, nrow = 0))
   
   for (f in files) {
      lines <- readLines(f)
      split <- which(grepl("rVals", lines))
      split.start <- split + 1
      if (length(split) == 1) { # if only one apple
         split.end <- length(lines) - 1
      } else {
         split.end <- split + split[2] - 2 
      }
      
      li.apple <- list()
      for (i in 1:length(split)) {
         li.apple[[i]] <- lines[split.start[i]:split.end[i]]
      }
      
      li.apple <- lapply(li.apple, strsplit, split = ":")
      text.to.df <- function(charvect) {
         charvect <- gsub(";", "\n", charvect)
         charvect <- gsub("\\[", "", charvect)
         charvect <- gsub("\\]", "", charvect)
         df <- read.csv(textConnection(charvect[length(charvect)]),sep = ",", header = F)
         names(df) <- gsub(" ", "", charvect[1])
         return(df)
      }
      
      df.shape <- as.data.frame(matrix(nrow = length(li.apple), ncol = 15))
      df.shape2 <- as.data.frame(matrix(nrow = 0, ncol = 5))
      df.shape3 <- as.data.frame(matrix(nrow = 0, ncol = 5))
      vbadge2 <- vapplenr2 <- vtrait2 <- vdate2 <- vector()
      vbadge3 <- vapplenr3 <- vtrait3 <- vdate3 <- vector()
      
      for (i in 1:length(li.apple)) {
         li.apple[[i]] <- lapply(li.apple[[i]], text.to.df)
         tmp <- unlist(strsplit(as.character(li.apple[[i]][[1]][1,1]),"cam1"))
         badge <- gsub("\\\\","", gsub("_$","",tmp[2]))
         nr <- unlist(strsplit(tmp[3], "_"))[2]
         date <- unlist(strsplit(tmp[3], "_"))[3:5]
         date <- gsub("\\.tif'", "", paste(date[1], date[2], date[3], sep = ""))
         df.shape[i,1] <- badge
         df.shape[i,2] <- nr
         df.shape[i,3] <- date
         for (j in 2:14) {
            if (ncol(li.apple[[i]][[j]]) == 1) {
               df.shape[i,j + 1] <- li.apple[[i]][[j]][1,1]
            } else {
               if (ncol(li.apple[[i]][[j]]) == 5) {
                  df.shape2 <- rbind(df.shape2, as.numeric(li.apple[[i]][[j]][1,]))
                  trait2 <- names(li.apple[[i]][[j]])[1]
                  vbadge2 <- c(vbadge2, badge)
                  vapplenr2 <- c(vapplenr2, nr)
                  vtrait2 <- c(vtrait2, trait2)
                  vdate2 <- c(vdate2, date)
               } else {
                  if (ncol(li.apple[[i]][[j]]) == 13) {
                     df.shape3 <- rbind(df.shape3, t(li.apple[[i]][[j]]))
                     trait3 <- names(li.apple[[i]][[j]])[1]
                     vbadge3 <- c(vbadge3, badge)
                     vapplenr3 <- c(vapplenr3, rep(nr,13))
                     vtrait3 <- c(vtrait3, trait3)
                     vdate3 <- c(vdate3, rep(date,13))
                  }
               }       
            }
         }
      }
      
      # Combine vectors and df
      df.shape2 <- cbind(vbadge2, vapplenr2, vdate2, vtrait2, df.shape2)
      colnames(df.shape2) <- c("badge", "appleNR", "date", "trait", "cam1", "cam2", "cam3", "cam4", "cam5")
      df.shape3 <- cbind(vbadge3, vapplenr3, vdate3, vtrait3, rep(c(1:13), nrow(df.shape3)/13),df.shape3)
      colnames(df.shape3) <- c("badge", "appleNR", "date", "trait", "shape", "cam1", "cam2", "cam3", "cam4", "cam5")
      row.names(df.shape3) <- c(1:nrow(df.shape3))
      
      # Add colnames to df.shape
      for (c in 1:14) {
         colnames(df.shape)[c + 1] <- names(li.apple[[1]][[c]][1])
      }
      
      colnames(df.shape)[1:3] <- c("badge", "appleNR", "date")
      # Remove NA columns
      df.shape %>% select(-width_cm, -height_cm, -symmetry, -symmetry_angle_deg, -contains("radius")) -> df.shape
      
      # First round add colnames to final dfs
      if (f == files[1]) {
         colnames(traits) <- colnames(df.shape)
         colnames(traitspercam) <- colnames(df.shape2)
         colnames(corshapes) <- colnames(df.shape3)
      }
      
      # Combine different badgees
      traits <- rbind(traits, df.shape)
      traitspercam <- rbind(traitspercam, df.shape2)
      corshapes <- rbind(corshapes, df.shape3)
   }
   
   # Remove everything except for the final dfs 
   rm(list = setdiff(ls(), c("traits", "traitspercam", "corshapes")))
   
   # Add SD and mean to traitspercam
   traitspercam$mean <- rowMeans(traitspercam[,c(6:9)])
   traitspercam %>% mutate(SD = apply(.[(6:9)],1,sd)) %>% select(SD) -> tmp
   traitspercam$SD <- tmp$SD
   traitspercam$SD <- signif(traitspercam$SD, digits = 5)
   traitspercam$mean <- signif(traitspercam$mean, digits = 5)
   
   # Add SD and mean to traits
   reshape(traitspercam, idvar = c("badge", "appleNR", "date"), 
           timevar = "trait", direction = "wide") %>% 
      select(badge, appleNR, date, SD.width_cm, SD.height_cm, mean.symmetry, SD.symmetry) -> tmp
   traits <- merge(traits, tmp, by = c("badge", "appleNR", "date"))
   
   colnames(traits)[c(8:11)] <- 
     c("width_SD_cm", "height_SD_cm", "symmetry_mean", "symmetry_SD")
   # Do not keep max and min values diameter and reorder df
   traits <- traits[,c(1:6, 8, 7, 9:11)]
   
   # Output text files
   dir.create("processed", showWarnings = FALSE)
   
   f <- file("processed/corshapes.txt", open = "wb")
   write.table(corshapes, file = f, quote = F, eol = "\n", row.names = F, sep = "\t")
   close(f)
   
   f <- file("processed/traits.txt", open = "wb")
   write.table(traits, file = f, sep = "\t", eol = "\n", na = "NA", row.names = F, quote = F)
   close(f)
   
   f <- file("processed/traitspercam.txt", open = "wb")
   write.table(traitspercam, file = f, sep = "\t",  eol = "\n", na = "NA", row.names = F, quote = F)
   close(f)
}
