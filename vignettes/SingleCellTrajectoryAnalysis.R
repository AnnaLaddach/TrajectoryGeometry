## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#load packages
library(TrajectoryGeometry)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

#set up colors
colors = colorRampPalette(brewer.pal(11,'Spectral')[-6])(100)

#set random seed
set.seed(42)

## -----------------------------------------------------------------------------
#filter matrices
neuralAttributes = singleCellMatrix[!is.na(neuralPseudoTime),]
glialAttributes = singleCellMatrix[!is.na(glialPseudoTime),]

#filter pseudotime values
neuralPseudoTime = neuralPseudoTime[!is.na(neuralPseudoTime)]
glialPseudoTime = glialPseudoTime[!is.na(glialPseudoTime)]

#normalise pseudotime values to 100
neuralPseudoTimeNormalised = neuralPseudoTime  %>% {100*((. - min(.))/(max(.) - min(.)))}
glialPseudoTimeNormalised = glialPseudoTime %>% {100*((. - min(.))/(max(.) - min(.)))}

## -----------------------------------------------------------------------------
neuralPath = samplePath(neuralAttributes, neuralPseudoTimeNormalised)
glialPath = samplePath(glialAttributes, glialPseudoTimeNormalised)

