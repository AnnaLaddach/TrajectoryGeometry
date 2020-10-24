## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, out.width="50%", fig.cap="A path and its projection to the sphere."----
knitr::include_graphics("pathAndSphere.png")

## ----message=FALSE------------------------------------------------------------
library(TrajectoryGeometry)
randomizationParams = c('bySteps','preserveLengths')
Y = generateRandomPaths(path=straightPath,
                        randomizationParams=randomizationParams,
                        N=10)

## ----message=FALSE------------------------------------------------------------
randomizationParams = c('bySteps','preserveLengths','nonNegaive')
Y = generateRandomPaths(path=straightPath,
                        randomizationParams=randomizationParams,
                        N=10)

## ----message=FALSE------------------------------------------------------------
	 randomizationParams = c('byPermutation','permuteAsMatrix')
	 randomizationParams = c('byPermutation','permuteWithinColumns')

## ----message=FALSE------------------------------------------------------------
   progress = pathProgression(straightPath,direction=straightPathCenter)
   progress = pathProgression(crookedPath,from=6,direction=crookedPathCenter)

## ----message=FALSE------------------------------------------------------------
direction = oscillation[nrow(straightPath),] - oscillation[1,] 
progress = pathProgression(oscillation,direction=direction) 

