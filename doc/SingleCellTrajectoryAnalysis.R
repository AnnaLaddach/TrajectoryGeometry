## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ---- echo=FALSE, out.width="50%", fig.cap="hepatoblast trajectory"-----------
knitr::include_graphics("trajectory.png")

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
chol_attributes = single_cell_matrix[!is.na(chol_pseudo_time),]
hep_attributes = single_cell_matrix[!is.na(hep_pseudo_time),]
#filter pseudotime values
chol_pseudo_time = chol_pseudo_time[!is.na(chol_pseudo_time)]
hep_pseudo_time = hep_pseudo_time[!is.na(hep_pseudo_time)]

#normalise pseudotime values to range from 0 to 100
chol_pseudo_time_normalised =
    chol_pseudo_time  %>% {100*((. - min(.))/(max(.) -min(.)))}
hep_pseudo_time_normalised =
    hep_pseudo_time %>% {100*((. - min(.))/(max(.)- min(.)))}

## -----------------------------------------------------------------------------
cholPath = samplePath(chol_attributes, chol_pseudo_time_normalised)
hepPath = samplePath(hep_attributes, hep_pseudo_time_normalised)

## -----------------------------------------------------------------------------
cholAnswerPermutation = testPathForDirectionality(cholPath[,1:3],
    randomizationParams = c('byPermutation','permuteWithinColumns'),
    statistic = "mean",
    N = 100)

hepAnswerPermutation = testPathForDirectionality(hepPath[,1:3],
    randomizationParams = c('byPermutation','permuteWithinColumns'),
    statistic = "mean",
    N = 100)

cholAnswerSteps = testPathForDirectionality(cholPath[,1:3],
    randomizationParams = c('bySteps','preserveLengths'),
    statistic = "mean",
    N = 100)

hepAnswerSteps = testPathForDirectionality(hepPath[,1:3],
    randomizationParams = c('bySteps','preserveLengths'),
    statistic = "mean",
    N = 100)

## -----------------------------------------------------------------------------
plotPathProjectionCenterAndCircle(path=cholPath[,1:3],
    projection=cholAnswerPermutation$sphericalData$projections,
    center=cholAnswerPermutation$sphericalData$center,
    radius=cholAnswerPermutation$sphericalData$distance,
    color=colors[cut(1:10,breaks=100)],
    circleColor = "white",
    pathPointSize = 8,
    projectionPointSize = 8,
    newFigure=TRUE)

## ---- echo=FALSE, out.width="50%", fig.cap="Cholangiocyte path plot"----------
knitr::include_graphics("cholangiocytePathPlot.png")

## -----------------------------------------------------------------------------
plotPathProjectionCenterAndCircle(path=hepPath[,1:3],
    projection=hepAnswerPermutation$sphericalData$projections,
    center=hepAnswerPermutation$sphericalData$center,
    radius=hepAnswerPermutation$sphericalData$distance,
    color=colors[cut(1:10,breaks=100)],
    circleColor = "white",
    pathPointSize = 8,
    projectionPointSize = 8,
    newFigure=TRUE)

## ---- echo=FALSE, out.width="50%", fig.cap="Hepatocyte path plot"-------------
knitr::include_graphics("hepatocytePathPlot.png")

## -----------------------------------------------------------------------------
chol_answers = analyseSingleCellTrajectory(attributes = chol_attributes[,1:3],
    pseudotime = chol_pseudo_time_normalised,
    randomizationParams = c('byPermutation','permuteWithinColumns'),
    statistic = "mean",
    nSamples = 100,
    N = 1)
# #N.B N = 1 allows us to generate a single random path parameterised on each
# sampled path.

hep_answers = analyseSingleCellTrajectory(hep_attributes[,1:3],
    hep_pseudo_time_normalised,
    randomizationParams = c('byPermutation','permuteWithinColumns'),
    statistic = "mean",
    nSamples = 100,
    N = 1)

## ---- fig.align = 'center'----------------------------------------------------
cholResultDistance = visualiseTrajectoryStats(chol_answers, "distance")
hepResultDistance = visualiseTrajectoryStats(hep_answers, "distance")

#visualise plots
cholResultDistance$plot
hepResultDistance$plot

## ---- fig.align = 'center'----------------------------------------------------
distanceComparison = visualiseTrajectoryStats(chol_answers, "distance",
    traj2Data = hep_answers)
# a violin plot is returned as distanceComparison$plot
# below we change x axis labels to indicate that the two trajectories being
# compared are neural and glial trajectories.

distanceComparison$plot +
    scale_x_discrete(labels=c("Cholangiocyte","Hepatocyte"))

## ---- fig.align = 'center'----------------------------------------------------
distances = distanceBetweenTrajectories(chol_attributes, chol_pseudo_time,
    hep_attributes)

plot(distances$pseudotime, distances$distance,
    xlab = "cholangiocyte pseudotime", ylab = "minimum distance")

## -----------------------------------------------------------------------------
chol_branch_point_results = analyseBranchPoint(chol_attributes[,1:3],
                                        chol_pseudo_time,
                                        randomizationParams = c("byPermutation",
                                                        "permuteWithinColumns"),
                                        statistic = "mean",
                                        start = 0,
                                        stop = 50,
                                        step = 5,
                                        nSamples = 100,
                                        N = 1)

## ---- fig.align = 'center'----------------------------------------------------
cholBranchPointStats = visualiseBranchPointStats(chol_branch_point_results)
print(cholBranchPointStats$distancePlot)
print(cholBranchPointStats$pValuePlot)

## ---- fig.align = 'center'----------------------------------------------------
distances = distanceBetweenTrajectories(hep_attributes, hep_pseudo_time,
                            chol_attributes)

plot(distances$pseudotime, distances$distance, xlab = "hepatocyte pseudotime",
    ylab = "minimum distance")

## -----------------------------------------------------------------------------
hepBranchPointResults = analyseBranchPoint(hep_attributes[,1:3],
    hep_pseudo_time,
    randomizationParams = c("byPermutation",
    "permuteWithinColumns"),
    statistic = "mean",
    start = 0,
    stop = 50,
    step = 5,
    nSamples = 100,
    N = 1)

## ---- fig.align = 'center'----------------------------------------------------
hepBranchPointStats = visualiseBranchPointStats(hepBranchPointResults)
print(hepBranchPointStats$distancePlot)
print(hepBranchPointStats$pValuePlot)

