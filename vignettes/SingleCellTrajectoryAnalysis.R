## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
data("singleCellMatrix", package = "TrajectoryGeometry")

## -----------------------------------------------------------------------------
data("neuralPseudoTime", package = "TrajectoryGeometry")
data("glialPseudoTime", package = "TrajectoryGeometry")

## -----------------------------------------------------------------------------
#filter matrices
neuralAttributes = singleCellMatrix[!is.na(neuralPseudoTime),]
glialAttributes = singleCellMatrix[!is.na(glialPseudoTime),]

#filter pseudotime values
neuralPseudoTime = neuralPseudoTime[!is.na(neuralPseudoTime)]
glialPseudoTime = glialPseudoTime[!is.na(glialPseudoTime)]

