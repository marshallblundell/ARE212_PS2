#############################################################
#     File Name           :     ps2.R
#     Created By          :     MBlundell
#     Creation Date       :     [2018-03-11 19:25]
#     Last Modified       :     [2018-03-11 19:42]
#     Description         :      
#############################################################

# 
# Set up.
#
library(pacman)
p_load(readr, xlsx)

dir <- "C:/Users/mblundell/Documents/ARE/ARE212/PS2/"
originaldata <- paste0(dir, "OriginalData/")
output <- paste0(dir, "Output/")

#
# Load data and clean it.
#
data <- read.xlsx2(paste0(originaldata, "nerlove.xls"),
                   sheetIndex = 1)

