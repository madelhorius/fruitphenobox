# ---------------------------------------------------------------------------- #
# Phenobox ------------------------------------------------------------------- #
# Get variety and row / tree info -------------------------------------------- #
# This script is used by build fact_sheet ------------------------------------ #
# Marius Hodel --------------------------------------------------------------- #
# 2023-12-11 ----------------------------------------------------------------- #
#----------------------------------------------------------------------------- #

get.exp.info = function(file, delimiter, badge, row, tree, variety, uniq.cult) {
  df <- read.table(file, sep = delimiter, quote = "\"", header = T, colClasses = "character")
  # Get required cols
  df <- df[,c(badge, row, tree, variety, uniq.cult)]
  colnames(df) <- c("badge", "row", "tree", "variety", "uniq.cult")
  df$row <- as.integer(df$row)
  df$tree <- as.integer(df$tree)
  return(df)
}
