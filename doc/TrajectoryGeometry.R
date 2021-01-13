## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(TrajectoryGeometry)
randomizationParams = c('bySteps','preserveLengths')
Y = generateRandomPaths(path=straight_path,
                        randomizationParams=randomizationParams,
                        N=10)

## ----message=FALSE------------------------------------------------------------
randomizationParams = c('bySteps','preserveLengths','nonNegative')
Y = generateRandomPaths(path=straight_path,
                        randomizationParams=randomizationParams,
                        N=10)

## ----message=FALSE------------------------------------------------------------
randomizationParams = c('byPermutation','permuteAsMatrix')
randomizationParams = c('byPermutation','permuteWithinColumns')

## ----message=FALSE------------------------------------------------------------
progress = pathProgression(straight_path,direction=straight_path_center)
progress = pathProgression(crooked_path,from=6,direction=crooked_path_center)

## ----message=FALSE------------------------------------------------------------
direction = oscillation[nrow(straight_path),] - oscillation[1,]
progress = pathProgression(oscillation,direction=direction)

