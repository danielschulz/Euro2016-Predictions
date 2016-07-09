
# SETUP

rm(list = ls())
set.seed(4711)

# install.packages("taRifx", "markovchain", "ggplot", "devtools")
library(taRifx)
library(markovchain)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

library(devtools)
# install_github("plotflow", "trinker")

capgeminiColors = c("#0080AC", "#D5001B", "#83A63C", "#FFBA40")
