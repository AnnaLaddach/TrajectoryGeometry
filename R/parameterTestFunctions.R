
## ###################################################
## Documentation has been commented out as none of these
## functions are exported.

## ###################################################
## ' Test the inputs to testPathForDirectionality
## '
## ' @param path - An n x m matrix representing a series of n points in
## '     dimension m.
## ' @param from - The starting place along the path which will be
## '     treated as the center of the sphere.
## ' @param to - The end point of the path.
## ' @param d - The dimension under consideration.
## ' @param statistic - Allowable values are 'median', 'mean' or 'max'
## ' @param randomizationParams - A character vector which is used to
## '     control the production of randomized paths for comparison.
## ' @param N - The number of random paths to generated for statistical
## '     comparison to the given path.
## ' @return A successful test returns 0, otherwise execution halts.

##' @importFrom methods is
testPathForDirectionalityTest = function(path,from,to,d,
                                        randomizationParams,statistic,N)
{

    if(! is(path,'matrix'))
        stop('testPathForDirectionality expects path to be a matrix')

    if(from >= nrow(path))
        stop('testPathForDirectionality expects from to be < nrow(path)')

    if(to > nrow(path))
        stop('testPathForDirectionality expects to to be <= nrow(path)')

    if(from >= to)
        stop('testPathForDirectionality expects from < to')

    if(d > ncol(path))
        stop('testPathForDirectionality expects d to be <= ncol(path)')

    if(! randomizationParams[1] %in% c('byPermutation','bySteps'))
        stop(paste("testPathForDirectionality expectsrandomizationParams[1]",
                "to be either 'byPermutation'or 'bySteps'"))

    if(! statistic %in% c('median','mean','max'))
        stop(paste("testPathForDirectionality expects statistic to be",
                "one of 'median', 'mean' or 'max'. See vignette."))

    if(! is(N,'numeric') | N < 1)
        stop('testPathForDirectionality expects N to be a positive integer')

    return(0)
}

## ###################################################
## ' This tests the inputs to projectPathToSphere
## '
## ' @param path - This is an mxn dimensional matrix. Each row is
## '     considered a point.
## ' @param from - The starting place along the path which will be
## '     treated as the center of the sphere.
## ' @param to - The end point of the path.
## ' @param d - The dimension under consideration.
## ' @return A successful test returns 0, otherwise execution halts.
projectPathToSphereTest = function(path,from,to,d)
{
    if(! is(path,'matrix'))
        stop('projectPathToSphere expects path to be a matrix')

    if(from >= nrow(path))
        stop('projectPathToSphere expects from to be < nrow(path)')

    if(to > nrow(path))
        stop('projectPathToSphere expects to to be <= nrow(path)')

    if(from >= to)
        stop('projectPathToSphere expects from < to')

    if(d > ncol(path))
        stop('projectPathToSphere expects d to be <= ncol(path)')

    return(0)
}

## ###################################################
## ' This tests the inputs to findSphereClusterCenter
## '
## ' @param points - A set of n points on the (d-1) sphere given as an n
## '     x d matrix.
## ' @param statistic - The statistic to be minimized.  Allowable values
## '     are 'median','mean' or 'max'.
## ' @param normalize - If this is set to TRUE, the function will start
## '     by normalizing the input points.
## ' @return A successful test returns 0, otherwise execution halts.
findSphereClusterCenterTest = function(points,statistic,normalize)
{
    if(! is(points,'matrix'))
        stop('findSphereClusterCenter expects points to be a matrix')

    if(! statistic %in% c('median','mean','max'))
        stop(paste("findSphereClusterCenter expects statistic to be",
                "one of 'median', 'mean' or 'max'"))
    if(! is(normalize,'logical'))
        stop('findSphereClusterCenter expects normalize to be a logical')

    return(0)
}

## ###################################################
## ' This tests the inputs to findSphericalDistance
## '
## ' @param center - The proposed point from which distance to
## ' the others should be measured.  This is a numerical vector
## ' of length d.
## ' @param points - The set of target points for which spherical
## ' distance to the center should be calculated.  This is in the
## ' form of a n x d matrix.
## ' @param normalize - If this is set to TRUE, the function will start
## ' by normalizing the input points.
## ' @return A successful test returns 0, otherwise execution halts.
findSphericalDistanceTest = function(center,points,normalize)
{
    if(! is(center,'numeric'))
        stop('findSphericalDistance expects center to be numeric')

    if(! is(points,'matrix'))
        stop('findSphericalDistance expects points to be a matrix')

    if(length(center) != ncol(points))
        stop('findSphericalDistance expects center and points to have the same
        dimension')

    if(! is(normalize,'logical'))
        stop('findSphericalDistance expects normalize to be a logical')

    return(0)
}

## ###################################################
## ' This tests the inputs to getSphericalData
## '
## ' @param path - an m x n matrix.  Each row is considered a point
## ' @param statistic - one of 'mean','median' or 'max'
## ' @return A successful test returns 0, otherwise execution halts.
getSphericalDataTest = function(path,statistic)
{
    if(! is(path,'matrix'))
        stop('getSphericalData expects path to be a matrix')

    if(! statistic %in% c('median','mean','max'))
        stop(paste("getSphericalData expects statistic to be",
                "one of 'median', 'mean' or 'max'"))

    return(0)
}

## ###################################################
## ' This tests the inputs to pathToSphericalData
## '
## ' @param path - This is an mxn dimensional matrix. Each row is
## '     considered a point.
## ' @param from - The starting place along the path which will be
## '     treated as the center of the sphere.
## ' @param to - The end point of the path.
## ' @param d - The dimension under consideration.
## ' @param statistic - One of 'median', 'mean' or 'max'
## ' @return A successful test returns 0, otherwise execution halts.
pathToSphericalDataTest = function(path,from,to,d,statistic)
{
    if(! is(path,'matrix'))
        stop('pathToSphericalData expects path to be a matrix')

    if(from >= nrow(path))
        stop('pathToSphericalData expects from to be < nrow(path)')

    if(to > nrow(path))
        stop('pathToSphericalData expects to to be <= nrow(path)')

    if(from >= to)
        stop('pathToSphericalData expects from < to')

    if(d > ncol(path))
        stop('pathToSphericalData expects d to be <= ncol(path)')

    if(! statistic %in% c('median','mean','max'))
        stop(paste("pathToSphericalData expects statistic to be",
                "one of 'median', 'mean' or 'max'"))

    return(0)
}

## ###################################################
## ' This tests the inputs to generateRandomPaths
## '
## ' @param path - This is an mxn dimensional matrix. Each row is
## '     considered a point.
## ' @param from - The starting place along the path which will be
## '     treated as the center of the sphere.
## ' @param to - The end point of the path.
## ' @param d - The dimension under consideration.
## ' @param randomizationParams - A character vector controling the
## '     randomization method used.  It's first entry must be either
## '     'byPermutation' or 'bySteps'  See the vignette for further
## '     details.
## ' @param N - The number of random paths required.
## ' @return A successful test returns 0, otherwise execution halts.
generateRandomPathsTest = function(path,from,to,d,randomizationParams,N)
{
    if(! is(path,'matrix'))
        stop('generateRandomPaths expects path to be a matrix')

    if(from >= nrow(path))
        stop('generateRandomPaths expects from to be < nrow(path)')

    if(to > nrow(path))
        stop('generateRandomPaths expects to to be <= nrow(path)')

    if(from >= to)
        stop('generateRandomPaths expects from < to')

    if(d > ncol(path))
        stop('generateRandomPaths expects d to be <= ncol(path)')

    if(! randomizationParams[1] %in% c('byPermutation','bySteps'))
        stop(paste("generateRandomPaths expectsrandomizationParams[1]",
                "to be either 'byPermutation'or 'bySteps'.  See vignette."))

    if(! is(N,'numeric') | N < 1)
        stop('generateRandomPaths expects N to be a positive integer')

    return(0)
}

## ###################################################
## ' This tests the inputs to getStepLengths
## '
## ' @param path - This is an mxn dimensional matrix. Each row is
## '     considered a point.
## ' @param from - The starting place along the path which will be
## '     treated as the center of the sphere.
## ' @param to - The end point of the path.
## ' @param d - The dimension under consideration.
## ' @return A successful test returns 0, otherwise execution halts.
getStepLengthsTest = function(path,from,to,d)
{
    if(! is(path,'matrix'))
        stop('getStepLengths expects path to be a matrix')

    if(from >= nrow(path))
        stop('getStepLengths expects from to be < nrow(path)')

    if(to > nrow(path))
        stop('getStepLengths expects to to be <= nrow(path)')

    if(from >= to)
        stop('getStepLengths expects from < to')

    if(d > ncol(path))
        stop('getStepLengths expects d to be <= ncol(path)')

    return(0)
}

## ###################################################
## ' This tests the inputs for getDistanceDataForPaths
## '
## ' @param paths - A list of paths.  Each of these is an n x d matrix.
## ' @param statistic - Allowable values are 'median', 'mean' or 'max'.
## ' @return A successful test returns 0, otherwise execution halts.
getDistanceDataForPathsTest = function(paths,statistic)
{
    if(! is(paths,'list'))
        stop('getDistanceDataForPaths expects paths to be a list')

    for (path in paths){
        if(! is(path,'matrix'))
            stop('getDistanceDataForPaths expects each path to be a matrix')
    }

    if(! statistic %in% c('median','mean','max'))
        stop(paste("getDistanceDataForPaths expects statistic to be",
                "one of 'median', 'mean' or 'max'"))

    return(0)
}

## ###################################################
## ' This tests the inputs for generateRandomUnitVector
## '
## ' @param d - The dimension.
## ' @return A successful test returns 0, otherwise execution halts.
generateRandomUnitVectorTest = function(d)
{
    if(d < 1)
        stop('generateRandomUnitVector expects d to be a positive integer')

    return(0)
}

## ###################################################
## ' This tests the inputs for pathProgression
## '
## ' @param path - An n x d matrix
## ' @param from - The point along the path to be taken as the starting
## '     point.
## ' @param to - The point along the path to be used as the end point.
## ' @param d - The dimension to be used.
## ' @param direction - A non-zero numeric whose length is the the
## '     dimension.
## ' @return A successful test returns 0, otherwise execution halts.
pathProgressionTest = function(path,from,to,d,direction)
{
    if(! is(path,'matrix'))
        stop('pathProgression expects path to be a matrix')

    if(from >= nrow(path))
        stop('pathProgression expects from to be < nrow(path)')

    if(to > nrow(path))
        stop('pathProgression expects to to be <= nrow(path)')

    if(from >= to)
        stop('pathProgression expects from < to')

    if(d > ncol(path))
        stop('pathProgression expects d to be <= ncol(path)')

    if(! length(direction == d))
        stop(paste('pathProgression expects direction and',
                'the points of path to have the same dimension'))

    return(0)
}

## ##########################################################################
## ' This tests the inputs for samplePath.
## '
## ' @param attributes - An n x d (cell x attribute) matrix of numeric attributes
## ' for single cell data. Rownames should be cell names.
## ' @param pseudotime - A named numeric vector of pseudotime values for cells.
## ' @param nWindows - The number of windows pseudotime should be split into to
## ' sample cells from. Defaults to 10.
## ' @return A successful test returns 0, otherwise execution halts.
samplePathTest = function(attributes, pseudotime, nWindows)
{
    if(! is(attributes,'matrix'))
        stop('samplePath expects attributes to be a matrix')
    if(! is(pseudotime,'numeric'))
        stop('samplePath expects pseudotime to be a numeric vector')

    if(! length(pseudotime) == nrow(attributes))
        stop('samplePath expects the length of pseudotime to match the number
        of rows of attributes')

    if(! sum(names(pseudotime) == rownames(attributes)) == length(pseudotime))
        stop('samplePath expects the names of pseudotime to match the row
        names of attributes')

    if(! is(nWindows,'numeric') | ! length(nWindows) == 1 | nWindows < 1)
        stop('samplePath expects nWindows to be a positive integer')

    return(0)
}

## ##########################################################################
## ' This tests the input to analyseSingleCellTrajectory.
## '
## ' @param attributes - An n x d (cell x attribute) matrix of numeric attributes
## '     for single cell data. Rownames should be cell names.
## ' @param pseudotime - A named numeric vector of pseudotime values for cells.
## ' @param randomizationParams - A character vector which is used to
## '     control the production of randomized paths for comparison.
## ' @param statistic - Allowable values are 'median', 'mean' or 'max'.
## ' @param nSamples - The number of sampled paths to generate (defaults to 1000).
## ' @param nWindows - The number of windows pseudotime should be split into to
## '     sample cells from (defaults to 10).
## ' @param d - The dimension under consideration.  This defaults to
## '     ncol(attributes).
## ' @param N - The number of random paths to generated for statistical
## '     comparison to the given path (defaults to 1000).
## ' @return A successful test returns 0, otherwise execution halts.
analyseSingleCellTrajectoryTest = function(attributes, pseudotime,
    randomizationParams, statistic, nSamples, nWindows, d, N)
{
    if(! is(attributes,'matrix'))
        stop('analyseSingleCellTrajectory expects attributes to be a matrix')

    if(! is(pseudotime,'numeric'))
        stop('analyseSingleCellTrajectory expects pseudotime to be a numeric
            vector')

    if(! length(pseudotime) == nrow(attributes))
        stop('analyseSingleCellTrajectory expects the length of pseudotime to
            match the number of rows of attributes')

    if(! sum(names(pseudotime) == rownames(attributes)) == length(pseudotime))
        stop('analyseSingleCellTrajectory expects the names of pseudotime to
            match the row names of attributes')

    if(! randomizationParams[1] %in% c('byPermutation','bySteps'))
        stop(paste("analyseSingleCellTrajectory expectsrandomizationParams[1]",
                "to be either 'byPermutation'or 'bySteps'.  See vignette."))

    if(! statistic %in% c('median','mean','max'))
        stop(paste("analyseSingleCellTrajectory expects statistic to be",
                "one of 'median', 'mean' or 'max'. See vignette."))

    if(! is(nSamples,'numeric') | ! length(nSamples) == 1 | nSamples < 1)
        stop('analyseSingleCellTrajectory expects nSamples to be a positive
            integer')

    if(! is(nWindows,'numeric') | ! length(nWindows) == 1 | nWindows < 1)
        stop('analyseSingleCellTrajectory expects nWindows to be a positive
            integer')

    if((! is(d,'integer') & ! is(d,'numeric')) | d < 1 | length(d) > 1)
        stop('analyseSingleCellTrajectory expects d to be a positive integer')

    if(d > ncol(attributes))
        stop('analyseSingleCellTrajectory expects d to be <= ncol(path)')

    if((! is(N,'integer') & ! is(N,'numeric')) | N < 1 | length(N) > 1)
        stop('analyseSingleCellTrajectory expects N to be a positive integer')

    return(0)
}

## ##########################################################################
## ' This tests the input to analyseBranchPoint.
## '
## ' @param attributes - An n x d (cell x attribute) matrix of numeric attributes
## '     for single cell data. Rownames should be cell names.
## ' @param pseudotime - A named numeric vector of pseudotime values for cells.
## ' @param randomizationParams - A character vector which is used to
## '     control the production of randomized paths for comparison.
## ' @param statistic - Allowable values are 'median', 'mean' or 'max'.
## ' @param start - The first pseudotime value (as a percentage of the trajectory)
## '     from which to analyse the trajectory from.
## '     Defaults to 25\% of the way through the trajectory.
## ' @param stop - The last pseudotime value (as a percentage of the trajectory)
## '     from which to analyse the trajectory from.
## '     Defaults to 75\% of the way through the trajectory.
## ' @param step - The size of the step to take between successively later
## '     starting points in pseudotime.
## '     Defaults to 5\% of the trajectory length.
## ' @param nSamples - The number of sampled paths to generate (defaults to 1000).
## ' @param nWindows - The number of windows pseudotime should be split into to
## '    sample cells from (defaults to 10).
## ' @param d - The dimension under consideration.  This defaults to
## '     ncol(attributes).
## ' @param N - The number of random paths to generated for statistical
## '     comparison to the given path (defaults to 1000).
## ' @return A successful test returns 0, otherwise execution halts.
analyseBranchPointTest = function(attributes, pseudotime, randomizationParams,
                            statistic, start, stop, step, nSamples, nWindows,
                            d, N)
{
    if(! is(attributes,'matrix'))
        stop('analyseBranchPoint expects attributes to be a matrix')

    if(! is(pseudotime,'numeric'))
        stop('analyseBranchPoint expects pseudotime to be a numeric vector')

    if(! length(pseudotime) == nrow(attributes))
        stop('analyseBranchPoint expects the length of pseudotime to match the
            number of rows of attributes')

    if(! sum(names(pseudotime) == rownames(attributes)) == length(pseudotime))
        stop('analyseBranchPoint expects the names of pseudotime to match the
            row names of attributes')

    if(! randomizationParams[1] %in% c('byPermutation','bySteps'))
        stop(paste("analyseBranchPoint expectsrandomizationParams[1]",
                "to be either 'byPermutation'or 'bySteps'.  See vignette."))

    if(! statistic %in% c('median','mean','max'))
        stop(paste("analyseBranchPoint expects statistic to be",
                "one of 'median', 'mean' or 'max'. See vignette."))

    if((! is(start,'integer') & ! is(start,'numeric')) | start < 0 |
        start > 100 | length(start) > 1)
        stop('analyseBranchPoint expects start to be a positive number between
            0 and 100')

    if((! is(stop,'integer') & ! is(stop,'numeric')) | stop < 0 | stop > 100 |
        stop <= start | length(stop) > 1)
        stop('analyseBranchPoint expects stop to be a positive number between
            start and 100')

    if((! is(step,'integer') & ! is(step,'numeric')) | step <= 0 |
        step > stop - start | length(step) > 1)
        stop('analyseBranchPoint expects step to be a positive number less than
        stop - start')

    if(! is(nSamples,'numeric') | ! length(nSamples) == 1 | nSamples < 1)
        stop('analyseBranchPoint expects nSamples to be a positive integer')

    if(! is(nWindows,'numeric') | ! length(nWindows) == 1 | nWindows < 1)
        stop('analyseBranchPoint expects nWindows to be a positive integer')

    if((! is(d,'integer') & ! is(d,'numeric')) | d < 1 | length(d) > 1)
        stop('analyseBranchPoint expects d to be a positive integer')

    if(d > ncol(attributes))
        stop('analyseBranchPoint expects d to be <= ncol(path)')

    if((! is(N,'integer') & ! is(N,'numeric')) | N < 1 | length(N) > 1)
        stop('analyseBranchPoint expects N to be a positive integer')

    return(0)
}

## ##########################################################################
## ' This tests the imput to distanceBetweenTrajectories
## '
## ' @param attributes1 - An n x d (cell x attribute) matrix of numeric attributes
## ' for the first single cell trajectory.
## ' @param pseudotime1 - A named numeric vector of pseudotime values for the
## ' first single cell trajectory,
## '    names should match rownames of atrributes1.
## ' @param attributes2 - An n x d (cell x attribute) matrix of numeric attributes
## ' for the sencond single cell trajectory.
## ' @return A successful test returns 0, otherwise execution halts.
distanceBetweenTrajectoriesTest = function(attributes1,
                                        pseudotime1,
                                        attributes2)
{
    if(! is(attributes1,'matrix'))
        stop('distanceBetweenTrajectories expects attributes1 to be a matrix')

    if(! is(pseudotime1,'numeric'))
        stop('distanceBetweenTrajectories expects pseudotime1 to be a numeric
            vector')

    if(! length(pseudotime1) == nrow(attributes1))
        stop('distanceBetweenTrajectories expects the length of pseudotime1 to
            match the number of rows of attributes1')

    if(! sum(names(pseudotime1) == rownames(attributes1)) ==
        length(pseudotime1))
        stop('distanceBetweenTrajectories expects the names of pseudotime1 to
            match the row names of attributes1')

    if(! is(attributes2,'matrix'))
        stop('distanceBetweenTrajectories expects attributes2 to be a matrix')

    if(! ncol(attributes1) == ncol(attributes2))
        stop('distanceBetweenTrajectories expects attributes1 and attributes2
            to have the same number of columns')

    return(0)
}


## ###################################################
## ' This tests the input to orthoNormalBasis
## '
## ' @param x - A numerical vector of length 3
## ' @return A successful test returns 0, otherwise execution halts.
orthonormalBasisTest = function(x)
{
    if(! (is(x,'numeric') &
        length(x) == 3))
        stop('orthoNormalBasis expects x to be a numeric vector of length 3')

    return(0)
}

## ###################################################
## ' This tests the inputs to circleOnTheUnitSphere
## '
## ' @param center - The center of the circle.
## ' @param radius - The radius of the circle.
## ' @param N - The number of segments to approximate the circle.
## ' @return A successful test returns 0, otherwise execution halts.
circleOnTheUnitSphereTest  = function(center,radius,N)
{
    if(! (is(center,'numeric') &
        length(center) == 3))
        stop('circleOnTheUnitSphere expects center to be a numeric vector of
            length 3')

    if(radius >= pi)
        stop('circleOnTheUnitSphere expects radius to be < pi')

    if(N <3)
        stop('circleOnTheUnitSphere expects N to be an integer >= 3')

    return(0)
}

## ###################################################
## ' This tests the inputs to plotPathProjectionCenterAndCircle
## '
## ' @param path - A path of dimension 3 in the form of an N x 3 matrix.
## ' @param from - The starting place of the section under
## '     consideration.  This is used for marking the relevant
## '     portion.
## ' @param to - Likewise.
## ' @param projection - The projection of the relevant portion of the
## '     path.
## ' @param center - The center of the projection points.
## ' @param radius - The radius of the circle.
## ' @param color - The color to use for this path and its associated
## '     data.
## ' @param circleColor - Sets the colour of the circle.
## ' @param pathPointSize - Sets the size of points which represent the
## '     path.
## ' @param projectionPointSize - Sets the size of points which represent the
## '     projected path.
## ' @param scale - The path will be start (its actual start) at 0 and
## '     will be scaled so that its most distant point will be at this
## '     distance from the origin.  This is to keep it comparable in
## '     size to the sphere.
## ' @param newFigure - When plotting a single figure or the first of
## '     multiple figures, this should be set to TRUE which is its
## '     default.  Otherwise, set this to FALSE in order to add
## '     additional paths to the same figure.
## ' @return A successful test returns 0, otherwise execution halts.
plotPathProjectionCenterAndCircleTest = function(path,
                                                from,
                                                to,
                                                projection,
                                                center,
                                                radius,
                                                color,
                                                circleColor,
                                                pathPointSize,
                                                projectionPointSize,
                                                scale,
                                                newFigure)
{
    if(! is(path,'matrix'))
        stop('plotPathProjectionCenterAndCircle expects path to be a matrix')

    if(! ncol(path) == 3)
        stop('plotPathProjectionCenterAndCircle expects nrow(path) == 3')

    if(from >= nrow(path))
        stop('plotPathProjectionCenterAndCircle expects from
            to be < nrow(path)')

    if(to > nrow(path))
        stop('plotPathProjectionCenterAndCircle expects to
            to be <= nrow(path)')

    if(from >= to)
        stop('plotPathProjectionCenterAndCircle expects from < to')

    if(! is(projection,'matrix'))
        stop('plotPathProjectionCenterAndCircle expects projection
            to be a matrix')

    if(! ncol(projection) == 3)
        stop('plotPathProjectionCenterAndCircle expects nrow(projection) == 3')

    if(! (is(center,'numeric') &
        length(center) == 3))
        stop('plotPathProjectionCenterAndCircle expects center to be numeric of
            length 3')

    if(! (is.numeric(radius) &
        length(radius) == 1 &
        radius > 0))
        stop('plotPathProjectionCenterAndCircle expects radius to be a positive
            scalar')

    if(! (is.numeric(pathPointSize) &
        length(pathPointSize) == 1 &
        pathPointSize > 0))
        stop('plotPathProjectionCenterAndCircle expects pathPointSize to be a
            positive scalar')

    if(! (is.numeric(projectionPointSize) &
        length(projectionPointSize) == 1 &
        projectionPointSize > 0))
        stop(paste('plotPathProjectionCenterAndCircle expects',
                'projectionPointSize to be a positive scalar'))

    if(! is.logical(newFigure))
        stop('plotPathProjectionCenterAndCircle expects newFigure to be a
            logical')

    return(0)
}

## ###################################################
## ' This tests the input to visualiseTrajectoryStats
## '
## ' @param traj1Data - the result of analyseSingleCellTrajectory
## ' @param metric - either "pValue" or "distance"
## ' @param average - if there are multiple distances available for each
## ' sampled trajectory, calculate the average using mean or median
## ' (defaults to mean).
## ' @param traj2Data - either an empty list or the result of
## ' analyseSingleCellTrajectory
## ' @return A successful test returns 0, otherwise execution halts.
visualiseTrajectoryStatsTest = function(traj1Data,
                                        metric,
                                        average,
                                        traj2Data){
    if(! is(traj1Data,'list'))
        stop('visualiseTrajectoryStats expects traj1Data to be a list')

    if(! sum(names(traj1Data[[1]])  == c("pValue","sphericalData",
        "randomDistances","randomizationParams")) == 4)
        stop('visualiseTrajectoryStats expects traj1Data to be the output of
        analyseSingleCellTrajectory')

    if(! is(traj2Data,'list'))
        stop('visualiseTrajectoryStats expects traj2Data to be a list')

    if(length(traj2Data) > 0){
        if(! sum(names(traj2Data[[1]])  == c("pValue","sphericalData",
            "randomDistances","randomizationParams")) == 4)
            (stop('visualiseTrajectoryStats expects traj2Data to be the output
            of analyseSingleCellTrajectory'))
    }

    if(! average %in% c('median','mean'))
        stop(paste("visualiseTrajectoryStats expects average to be",
                "'median' or 'mean'."))

    if(! metric %in% c('pValue','distance'))
        stop(paste("visualiseTrajectoryStats expects metric to be",
                "'pValue' or 'distance'"))

    return(0)
}

## ###################################################
## ' This tests the input to visualiseBranchPointStats
## '
## ' @param branchPointData - the result of analyseBranchPoint
## ' @param average - if there are multiple distances available for each
## ' @return A successful test returns 0, otherwise execution halts.
visualiseBranchPointStatsTest = function(branchPointData,
                                        average){
    if(! is(branchPointData,'list'))
        stop('visualiseBranchPointStats expects branchPointData to be a list')

    if(! sum(names(branchPointData[[names(branchPointData)[[1]]]][[1]])
        == c("pValue","sphericalData","randomDistances","randomizationParams"))
        == 4)
        stop('visualiseBranchPointStats expects branchPointData to be the output
            of analyseBranchPoint')


    if(! average %in% c('median','mean'))
        stop(paste("visualiseTrajectoryStats expects average to be",
                "'median' or 'mean'"))

    return(0)
}
