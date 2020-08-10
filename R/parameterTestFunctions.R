

## ###################################################
#' Test the inputs to testPathForDirectionality
#'
#' @param path - An n x m matrix representing a series of n points in
#'     dimension m.
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.
#' @param to - The end point of the path.  
#' @param d - The dimension under consideration.
#' @param statistic - Allowable values are 'median', 'mean' or 'max'
#' @param randomizationParams - A character vector which is used to
#'     control the production of randomized paths for comparison.
#' @param N - The number of random paths to generated for statistical
#'     comparison to the given path.
testPathForDirectionalityTest = function(path,from,to,d,
                                         randomizationParams,statistic,N)
{

    if(! class(path) == 'matrix')
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
    
    if(class(N) != 'numeric' | N < 1)
        stop('testPathForDirectionality expects N to be a positive integer')
}

## ###################################################
#' This tests the inputs to projectPathToSphere
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.
#' @param to - The end point of the path.  
#' @param d - The dimension under consideration. 
projectPathToSphereTest = function(path,from,to,d)
{
    if(! class(path) == 'matrix')
        stop('projectPathToSphere expects path to be a matrix')
    
    if(from >= nrow(path))
        stop('projectPathToSphere expects from to be < nrow(path)')
    
    if(to > nrow(path))
        stop('projectPathToSphere expects to to be <= nrow(path)')
    
    if(from >= to)
        stop('projectPathToSphere expects from < to')
    
    if(d > ncol(path))
        stop('projectPathToSphere expects d to be <= ncol(path)')
}

## ###################################################
#' This tests the inputs to findSpherClusterCenter
#'
#' @param points - A set of n points on the (d-1) sphere given as an n
#'     x d matrix.
#' @param statistic - The statistic to be minimized.  Allowable values
#'     are 'median','mean' or 'max'. 
#' @param normalize - If this is set to TRUE, the function will start
#'     by normalizing the input points.
findSphereClusterCenterTest = function(points,statistic,normalize)
{
    if(! class(point) == 'matrix')
        stop('findSphereClusterCenter expects points to be a matrix')
    
    if(! statistic %in% c('median','mean','max'))
        stop(paste("findSphereClusterCenter expects statistic to be",
                   "one of 'median', 'mean' or 'max'"))

    if(! class(normalize) == 'logical')
        stop('findSphereClusterCenter expects normalize to be a logical')
}

## ###################################################
#' This tests the inputs to findSphericalDistance
#'
#' @param center - The proposed point from which distance to
#' the others should be measured.  This is a numerical vector
#' of length d.
#' @param points - The set of target points for which spherical
#' distance to the center should be calculated.  This is in the
#' form of a n x d matrix.
#' @param normalize - If this is set to TRUE, the function will start
#' by normalizing the input points.
findSphericalDistanceTest = function(center,points,normalize)
{
    if(! class(center) == 'numeric')
        stop('findSphericalDistance expects center to be numeric')

    if(! class(points) == 'matrix')
        stop('findSphericalDistance expects points to be a matrix')

    if(length(center) != ncol(points))
        stop('findSphericalDistance expects center and points to have the same dimension')

    if(! class(normalize) == 'logical')
        stop('findSphericalDistance expects normalize to be a logical')
}

## ###################################################
#' This tests the inputs to getSphericalData
#'
#' @param path - an m x n matrix.  Each row is considered a point
#' @param statistic - one of 'mean','median' or 'max'
getSphericalDataTest = function(path,statistic)
{
    if(! class(path) == 'matrix')
        stop('getSphericalData expects path to be a matrix')
    
    if(! statistic %in% c('median','mean','max'))
        stop(paste("getSphericalData expects statistic to be",
                   "one of 'median', 'mean' or 'max'"))
}

## ###################################################
#' This tests the inputs to pathToSphericalData
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.
#' @param to - The end point of the path.
#' @param d - The dimension under consideration.
#' @param statistic - One of 'median', 'mean' or 'max'
pathToSphericalDataTest = function(path,from,to,d,statistic)
{
    if(! class(path) == 'matrix')
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
}

## ###################################################
#' This tests the inputs to generateRandomPaths
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.
#' @param to - The end point of the path.
#' @param d - The dimension under consideration. 
#' @param randomizationParams - A character vector controling the
#'     randomization method used.  It's first entry must be either
#'     'byPermutation' or 'bySteps'  See the vignette for further
#'     details. 
#' @param N - The number of random paths required.
generateRandomPathsTest = function(path,from,to,d,randomizationParams,N)
{
    if(! class(path) == 'matrix')
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

    if(class(N) != 'numeric' | N < 1)
        stop('generateRandomPaths expects N to be a positive integer')
}

## ###################################################
#' This tests the inputs to getStepLengths
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere. 
#' @param to - The end point of the path.
#' @param d - The dimension under consideration.
getStepLengthsTest = function(path,from,to,d)
{
    if(! class(path) == 'matrix')
        stop('getStepLengths expects path to be a matrix')
    
    if(from >= nrow(path))
        stop('getStepLengths expects from to be < nrow(path)')
    
    if(to > nrow(path))
        stop('getStepLengths expects to to be <= nrow(path)')
    
    if(from >= to)
        stop('getStepLengths expects from < to')
    
    if(d > ncol(path))
        stop('getStepLengths expects d to be <= ncol(path)')
}

## ###################################################
#' This tests the inputs for getDistanceDataForPaths
#'
#' @param paths - A list of paths.  Each of these is an n x d matrix.
#' @param statistic - Allowable values are 'median', 'mean' or 'max'.
getDistanceDataForPathsTest = function(paths,statistic)
{
    if(! class(path) == 'matrix')
        stop('getDistanceDataForPaths expects path to be a matrix')

    if(! statistic %in% c('median','mean','max'))
        stop(paste("getDistanceDataForPaths expects statistic to be",
                   "one of 'median', 'mean' or 'max'"))
}

## ###################################################
#' This tests the inputs for generateRandomUnitVector
#'
#' @param d - The dimension.
generateRandomUnitVectorTest = function(d)
{
    if(d < 1)
        stop('generateRandomUnitVector expects d to be a positive integer')
}

## ###################################################
#' This tests the inputs for pathProgression
#'
#' @param path - An n x d matrix
#' @param from - The point along the path to be taken as the starting
#'     point. 
#' @param to - The point along the path to be used as the end point.
#' @param d - The dimension to be used. 
#' @param direction - A non-zero numeric whose length is the the
#'     dimension.
pathProgressionTest = function(path,from,to,d,direction)
{
    if(! class(path) == 'matrix')
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
}

## ###################################################
#' This tests the input to orthoNormalBasis
#'
#' @param x - A numerical vector of length 3
orthoNormalBasisTest = function(x)
{
    if(! (class(x) == 'numeric' &
          length(x) == 3))
        stop('orthoNormalBasis expects x to be a numeric vector of length 3')
}

## ###################################################
#' This tests the inputs to circleOnTheUnitSphere
#'
#' @param center - The center of the circle.
#' @param radius - The radius of the circle.
#' @param N - The number of segments to approximate the circle.
circleOnTheUnitSphereTest  = function(center,radius,N)
{
    if(! (class(center) == 'numeric' &
          length(center) == 3))
        stop('circleOnTheUnitSphere expects center to be a numeric vector of length 3')

    if(radius >= pi)
        stop('circleOnTheUnitSphere expects radius to be < pi')

    if(N <3)
        stop('circleOnTheUnitSphere expects N to be an integer >= 3')
}

## ###################################################
#' This tests the inputs to plotPathProjectionCenterAndCircle
#'
#' @param path - A path of dimension 3 in the form of an N x 3 matrix.
#' @param from - The starting place of the section under
#'     consideration.  This is used for marking the relevant
#'     portion.
#' @param to - Likewise.  
#' @param projection - The projection of the relevant portion of the
#'     path.
#' @param center - The center of the projection points.
#' @param radius - The radius of the circle.
#' @param color - The color to use for this path and its associated
#'     data.
#' @param circleColor - Sets the colour of the circle.
#' @param pathPointSize - Sets the size of points which represent the 
#'     path. 
#' @param projectionPointSize - Sets the size of points which represent the 
#'     projected path. 
#' @param scale - The path will be start (its actual start) at 0 and
#'     will be scaled so that its most distant point will be at this
#'     distance from the origin.  This is to keep it comparable in
#'     size to the sphere. 
#' @param newFigure - When plotting a single figure or the first of
#'     multiple figures, this should be set to TRUE which is its
#'     default.  Otherwise, set this to FALSE in order to add
#'     additional paths to the same figure.
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
    if(! class(path) == 'matrix')
        stop('plotPathProjectionCenterAndCircle expects path to be a matrix')

    if(! ncol(path) == 3)
        stop('plotPathProjectionCenterAndCircle expects nrow(path) == 3')
    
    if(from >= nrow(path))
        stop('plotPathProjectionCenterAndCircle expects from to be < nrow(path)')
    
    if(to > nrow(path))
        stop('plotPathProjectionCenterAndCircle expects to to be <= nrow(path)')
    
    if(from >= to)
        stop('plotPathProjectionCenterAndCircle expects from < to')
    
    if(! class(projection) == 'matrix')
        stop('plotPathProjectionCenterAndCircle expects projection to be a matrix')

    if(! ncol(projection) == 3)
        stop('plotPathProjectionCenterAndCircle expects nrow(projection) == 3')

    if(! (class(center) == 'numeric' &
          length(center) == 3))
        stop('plotPathProjectionCenterAndCircle expects center to be numeric of length 3')

    if(! (is.numeric(radius) &
          length(radius) == 1 &
          radius > 0))
        stop('plotPathProjectionCenterAndCircle expects radius to be a positive scalar')
    
    if(! (is.numeric(pathPointSize) &
          length(pathPointSize) == 1 &
          pathPointSize > 0))
        stop('plotPathProjectionCenterAndCircle expects pathPointSize to be a positive scalar')

    if(! (is.numeric(projectionPointSize) &
          length(projectionPointSize) == 1 &
          projectionPointSize > 0))
        stop(paste('plotPathProjectionCenterAndCircle expects',
                   'projectionPointSize to be a positive scalar'))   
       
    if(! is.logical(newFigure))
        stop('plotPathProjectionCenterAndCircle expects newFigure to be a logical')
}
