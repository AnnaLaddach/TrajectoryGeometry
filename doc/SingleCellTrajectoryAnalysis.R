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
cholAttributes = singleCellMatrix[!is.na(cholPseudoTime),]
hepAttributes = singleCellMatrix[!is.na(hepPseudoTime),]

#filter pseudotime values
cholPseudoTime = cholPseudoTime[!is.na(cholPseudoTime)]
hepPseudoTime = hepPseudoTime[!is.na(hepPseudoTime)]

#normalise pseudotime values to 100
cholPseudoTimeNormalised = cholPseudoTime  %>% {100*((. - min(.))/(max(.) - min(.)))}
hepPseudoTimeNormalised = hepPseudoTime %>% {100*((. - min(.))/(max(.) - min(.)))}

## -----------------------------------------------------------------------------
cholPath = samplePath(cholAttributes, cholPseudoTimeNormalised)
hepPath = samplePath(hepAttributes, hepPseudoTimeNormalised)

## -----------------------------------------------------------------------------
cholAnswerPermutation = testPathForDirectionality(cholPath[,1:3],randomizationParams = c('byPermutation','permuteWithinColumns'),statistic = "mean", N = 1000)
hepAnswerPermutation = testPathForDirectionality(hepPath[,1:3],randomizationParams = c('byPermutation','permuteWithinColumns'),statistic = "mean", N = 1000)

cholAnswerSteps = testPathForDirectionality(cholPath[,1:3],randomizationParams = c('bySteps','preserveLengths'),statistic = "mean", N = 1000)
hepAnswerSteps = testPathForDirectionality(hepPath[,1:3],randomizationParams = c('bySteps','preserveLengths'),statistic = "mean", N = 1000)

cat(paste("Mean distance of projected cholangiocyte pathway points from circle center:", cholAnswerPermutation$sphericalData$distance))
cat(paste("\nMean distance of projected hepatocyte pathway points from circle center:", hepAnswerPermutation$sphericalData$distance))

cat("\n\nPermutation results")
cat(paste("\nP value for cholangiocyte pathway:", cholAnswerPermutation$pValue))
cat(paste("\nP value for hepatocyte pathway:", hepAnswerPermutation$pValue))

cat("\n\nRandomisation by step results")
cat(paste("\nP value for cholangiocyte pathway:", cholAnswerSteps$pValue))
cat(paste("\nP value for hepatocyte pathway:", hepAnswerSteps$pValue))

## -----------------------------------------------------------------------------
plotPathProjectionCenterAndCircle(path=cholPath[,1:3],
                                  projection=cholAnswerPermutation$sphericalData$projections,
                                  center=cholAnswerPermutation$sphericalData$center,
                                  radius=cholAnswerPermutation$sphericalData$distance,
                                  color=colors[cut(1:10,breaks=100)],
                                  circleColor = "white",
                                  pathPointSize = 20,
                                  projectionPointSize = 20,
                                  newFigure=TRUE)

## -----------------------------------------------------------------------------
plotPathProjectionCenterAndCircle(path=hepPath[,1:3],
                                  projection=hepAnswerPermutation$sphericalData$projections,
                                  center=hepAnswerPermutation$sphericalData$center,
                                  radius=hepAnswerPermutation$sphericalData$distance,
                                  color=colors[cut(1:10,breaks=100)],
                                  circleColor = "white",
                                  pathPointSize = 20,
                                  projectionPointSize = 20,
                                  newFigure=TRUE)

## -----------------------------------------------------------------------------
cholAnswers = analyseSingleCellTrajectory(cholAttributes[,1:3], cholPseudoTimeNormalised, nSamples = 1000, randomizationParams = c('byPermutation','permuteWithinColumns'), statistic = "mean", N = 1)
# #N.B N = 1 allows us to generate a single random path parameterised on each sampled path.
#  
hepAnswers = analyseSingleCellTrajectory(hepAttributes[,1:3], hepPseudoTimeNormalised, nSamples = 1000, randomizationParams = c('byPermutation','permuteWithinColumns'), statistic = "mean", N = 1)

## -----------------------------------------------------------------------------
cholResultDistance = visualiseTrajectoryStats(cholAnswers, "distance")
hepResultDistance = visualiseTrajectoryStats(hepAnswers, "distance")

#visualise plots
cholResultDistance$plot
hepResultDistance$plot

cat(paste("Cholangiocyte p value (comparison of distance metric):", cholResultDistance$stats$p.value))

cat(paste("\nHepatocyte p value (comparison of distance metric):", hepResultDistance$stats$p.value)) 

## -----------------------------------------------------------------------------
distanceComparison = visualiseTrajectoryStats(cholAnswers, "distance", traj2Data = hepAnswers)
# a violin plot is returned as distanceComparison$plot
# below we change x axis labels to indicate that the two trajectories being compared are neural and glial trajectories. 
distanceComparison$plot + scale_x_discrete(labels=c("Cholangiocyte","Hepatocyte"))

cat(paste("Comparison of cholangiocyte and hepatocyte trajectories (distance metric), p value:", distanceComparison$stats$p.value)) 

## -----------------------------------------------------------------------------
distances = distanceBetweenTrajectories(cholAttributes, cholPseudoTime,
                            hepAttributes)

plot(distances$pseudotime, distances$distance, xlab = "cholangiocyte pseudotime", ylab = "minimum distance")

## -----------------------------------------------------------------------------
cholBranchPointResults = analyseBranchPoint(cholAttributes[,1:3], 
                                          cholPseudoTime,
                                          randomizationParams = c('byPermutation',
                                                          'permuteWithinColumns'), 
                                          statistic = "mean",
                                          start = 0,
                                          stop = 50,
                                          step = 5,
                                          nSamples = 100, 
                                          N = 1)

## -----------------------------------------------------------------------------
cholBranchPointStats = visualiseBranchPointStats(cholBranchPointResults)
print(cholBranchPointStats$distancePlot)
print(cholBranchPointStats$pValuePlot)

## -----------------------------------------------------------------------------
distances = distanceBetweenTrajectories(hepAttributes, hepPseudoTime,
                            cholAttributes)

plot(distances$pseudotime, distances$distance, xlab = "hepatocyte pseudotime", ylab = "minimum distance")

## -----------------------------------------------------------------------------
hepBranchPointResults = analyseBranchPoint(hepAttributes[,1:3], 
                                          hepPseudoTime,
                                          randomizationParams = c('byPermutation',
                                                          'permuteWithinColumns'), 
                                          statistic = "mean",
                                          start = 0,
                                          stop = 50,
                                          step = 5,
                                          nSamples = 100, 
                                          N = 1)

## -----------------------------------------------------------------------------
hepBranchPointStats = visualiseBranchPointStats(hepBranchPointResults)
print(hepBranchPointStats$distancePlot)
print(hepBranchPointStats$pValuePlot)

