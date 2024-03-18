# ---------------------------------------------------------------------------- #
# Phenobox ------------------------------------------------------------------- #
# Processing of corshapes.txt ------------------------------------------------ #
# This function extracts the most common shape of each badge according to ---- #
# their correlations to the predefined shapes -------------------------------- #
# Marius Hodel --------------------------------------------------------------- #
# 2023-12-11 ----------------------------------------------------------------- #
#----------------------------------------------------------------------------- #

library(dplyr)

what_shape = function(file, fieldinfo) {
  
  # Predefined shapes
  shapes.ger <- c("kegelfoermig", "kugelfoermig", "kugel-kegelfoermig", "breit kugel-kegelfoermig",
                 "abgeplattet", "abgeplattet kugelfoermig", "schmal kegelfoermig", "stumpf kegelfoermig",
                 "fassfoermig", "eifoermig", "rechteckig", "rechteckig kegelfoermig", "glockenfoermig")
  shapes.en <- c("conical", "spherical", "spherical-cone-shaped", "broad spherical-cone-shaped", "flattened", 
                "flattened conical", "narrow conical", "obtuse conical", "barrel-shaped", "egg-shaped", 
                "rectangular", "rectangular cone-shaped", "bell-shaped")
  corshapes <- read.table(file, header = T, sep = "\t", 
                          colClasses = c(rep("character",4), "integer",
                                                          rep("numeric", 5)))
  # Per badge or per genotype
  if (missing(fieldinfo)) {
    corshapes %>% group_by(badge, shape) %>%
      summarize(sum = sum(cam2, cam3, cam4, cam5), .groups = "drop") %>%
      group_by(badge) %>% top_n(1, sum) %>%
      select(badge,shape) %>% as.data.frame -> best
  } else {
    corshapes <- left_join(corshapes, field.info, by = "badge")[,c(1:10,14)]
    corshapes %>% group_by(uniq.cult, shape) %>%
      summarize(sum = sum(cam2, cam3, cam4, cam5), .groups = "drop") %>%
      group_by(uniq.cult) %>% top_n(1, sum) %>%
      select(uniq.cult,shape) %>% as.data.frame -> best
  }
  
  best$shape_ger <- shapes.ger[best$shape]
  best$shape_en <- shapes.en[best$shape]
  
  return(best)
}


