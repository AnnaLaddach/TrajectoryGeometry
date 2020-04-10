
#' @importFrom pracma Norm


## ##########################################################################
#' Test a path for directionality
#'
#' This is the core function of this package.  It takes a path, and a
#' choice of statistical measure and computes a statistical significance
#' for the directionality of that path.
#'
#' @param path - An n x m matrix representing a series of n points in
#'     dimension m.
#' @param start - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @param statistic - Allowable values are 'median', 'mean' or 'max'
#' @param preserveLengths - If this is set to TRUE, the length of the
#'     steps in the randomized paths will match those of the given
#'     path.
#' @param N - The number of random paths to generated for statistical
#'     comparison to the given path.
#' @return This returns a p-value for the directionality of the given
#'     path.
#' @export
testPathForDirectionality = function(path,start=1,end=nrow(path),d=ncol(path),
                                     statistic,preserveLengths=TRUE,N)
{
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[start:end,1:d]
    
    ## ###################################################
    ## Get the spherical data:
    sphericalData = getSphericalData(path,statistic)

    ## ###################################################
    ## Generate random paths:
    randomPaths = generateRandomPaths(path,preserveLengths,N)

    ## ###################################################
    ## Compute the distance statistics for random paths:
    distances = getDistanceDataForPaths(randomPaths,statistic)


    ## ###################################################
    ## Return the p-value:
    idx = distances <= sphericalData$distance 
    pValue = sum(idx) / N

    return(pValue)
}


## ##########################################################################
#' Project a path onto the unit sphere
#'
#' This function takes a path in d dimensional space and projects it onto
#' the d-1 sphere.  It takes as additional arguments the starting and ending points 
#' under consideration and the dimension to be considered.
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param start - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @return This returns a projection of the path onto the d-1 sphere
#'     in the form of a (end - start) x d matrix.
#' @export
projectPathToSphere = function(path,start=1,end=nrow(path),d=ncol(path))
{
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[start:end,1:d]
    n = nrow(path)
    
    ## ###################################################
    ## Create and populate a matrix with projections:
    projection = matrix(0,nrow=n-1,ncol=d)

    for(i in 2:n)
    {
        v = path[i,] - path[1,]
        projection[i-1,] = v / Norm(v)
    }

    return(projection)
}

## ###################################################
#' Find a center for points on the unit sphere
#'
#' This function takes a set of points on the d-1 sphere
#' in d-space and finds a center for these.  Depending on
#' choice of statistic, this center is a point on the
#' sphere which minimizes either the median distance, the
#' mean distance or the maximum distance of the center to
#' the given points. "Distance" here is taken to mean
#' angle between the points, i.e., arccos of their dot
#' product.
#'
#' @param points - A set of n points on the (d-1) sphere given as an n
#'     x d matrix.
#' @param statistic - The statistic to be minimized.  Allowable values
#'     are 'median','mean' or 'max'.
#' @param normalize - If this is set to TRUE, the function will start
#'     by normalizing the input points.
#' @return This returns a point in dimension d given as a vector.
#' @export
findSphereClusterCenter = function(points,statistic,normalize=FALSE)
{
   n = nrow(points)
    
    ## ###################################################
    ## Normalize if necessary:
    if(normalize)
    {
        center = center / Norm(center)
        for(i in seq_len(n))
            points[i,] = points[i,] / Norm(points[i,])
    }

    
    ## ###################################################
    ## Where the rubber hits the road!

}

## ###################################################
#' Find the spherical distance from a given point to a
#' set of points.
#'
#' This function takes a point (typically a center) and
#' a set of points and finds the spherical distance between
#' the given point and each of the others.  If requested, it
#' will first normalize all of them.
#'
#' @param center - The proposed point from which distance to
#' the others should be measured.  This is a numerical vector
#' of length d.
#' @param points - The set of target points for which spherical
#' distance to the center should be calculated.  This is in the
#' form of a n x d matrix.
#' @normalize - If this is set to TRUE, the function will start
#' by normalizing the input points.
#' @return This returns a vector of n spherical distances in
#' radians.
#' @ export
findSphericalDistance = function(center,points,normalize=FALSE)
{
    n = nrow(points)
    
    ## ###################################################
    ## Normalize if necessary:
    if(normalize)
    {
        center = center / Norm(center)
        for(i in seq_len(n))
            points[i,] = points[i,] / Norm(points[i,])
    }

    ## ###################################################
    ## Find spherical distances:
    distances = numeric(n)
    for(i in seq_len(n))
    {
        dotProduct = sum(center * points[i,])
        distances[i] = acos(dotProduct)
    }

    return(distances)
}

## ###################################################
#' Find the spherical data for a given path
#'
#' This function takes a path and returns a list containing
#' its projection to the sphere, the center for that projection,
#' the spherical distance from the center to the points of the
#' projection and the name of the statistic used.
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param start - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @param statistic
#' @return This function returns a list whose elements are the
#'     projections of the path to the sphere, the center for those
#'     projections, the median, mean or max distance from the center
#'     to those projections and the name of the statistic used.
#' @export
pathToSphericalData = function(path,start,end,d,statistic)
{
    returnValues = list()
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[start:end,1:d]
    n = nrow(path)
    
    ## ###################################################
    ## Get the projections of the path to the sphere
    projections = projectPathToSphere(path)
    returnValues$projections = projections

    ## ###################################################
    ## Find the center of those projections according to the
    ## chosen statistic.
    center = findSphereClusterCenter(projections,statistic)
    returnValues$center = center
    
    ## ###################################################
    ## Find the distance to report:
    distances = findSphericalDistance(center,projections)
    if(statistic == 'median')
        distance = median(distances)

    if(statistic == 'mean')
        distance = mean(distances)

    if(statistic == 'max')
        distance = max(distances)

    returnValues$distance = distance

    ## ###################################################
    ## Append the name of the statistic:
    returnValues$statistic = statistic
    
    return(returnValues)
}

## ###################################################
#' Produce random paths modeled on a given path
#'
#' This function takes a path and produces N random paths of the same
#' dimension and length based on it. The steps along the random paths
#' are based on uniform sampling of the unit sphere.  If 
#' preserveLengths is set to TRUE, the random paths of steps of the
#' same length as the original path.  Otherwise, the steps are all
#' length 1.
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param start - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @param preserveLengths - If this is set to TRUE, the steps of the
#'     random paths have the same euclidean length as those of the
#'     give path.  Otherwise they hav length 1.
#' @param N - The number of random paths required.
#' @return This function returns a list of random paths.  Each path is
#'     a matrix.  Note that each random path begins at the origin.
#' @export
generateRandomPaths = function(path,start=1,end=nrow(path),d=ncol(path),
                               preserveLengths=TRUE,N)
{
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[start:end,1:d]
    n = nrow(path)
    
    
    ## ###################################################
    ## Find the length of the steps if required:
    stepLengths = rep(1,n-1)
    if(preserveLengths)
        stepLengths = getStepLengths(path,d)
    

    ## ###################################################
    ## Generate the random paths:
    returnAsList = list()
    for(i in seq_len(N))
    {
        randomPath = matrix(0,nrow=n,ncol=d)
        for(j in seq_len(n-1))
            randomPath[j+1,] = randomPath[j,] +
                stepLengths[j] * generateRandomUnitVector(d)

        if(N == 1)
            return(randomPath)

        returnAsList[[i]] = randomPath
    }

    return(returnAsList)
}

## ###################################################
#' Find the step lengths:
#'
#' This finds the lengths of the steps along a path
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param start - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @return This function returns the length of each step in a path.
#' @export
getStepLengths = function(path,start=1,end=nrow(path),d=ncol(path))
{
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[start:end,1:d]
    n = nrow(path)

    stepLengths = numeric(n-1)
        for(i in seq_len(n-1))
            stepLengths[i] = Norm(paths[i+1,] - paths[i,])

    return(stepLengths)
}

## ###################################################
#' Produce distance statistics for random paths
#'
#' This function takes a list of paths and a choice of
#' statistic (median, mean or max) and returns that statistic
#' for the appropriate center for each path.  Each path
#' is an n x d matrix.  In use, it is assumed that these
#' will be the randomized paths.  It is therefore assumed
#' that they are already of the correct dimensions.
#'
#' @param paths - A list of paths.  Each of these is an n x d matrix.
#' @param statistic - Allowable values are 'median', 'mean' or 'max'.
#' @return This returns a vector of n distances.
#' @export
getDistanceDataForPaths = function(paths,statistic)
{
    n = nrow(paths[[1]])
    N = length(paths)
    distances = numeric(N)

    ## ###################################################
    ## Iterate over paths:
    for(i in seq_len(N))
    {
        sphericalData = getSphericalData(paths[[i]],statistic)
        distances[i] = sphericalData$distance
    }

    return(distances)
}

## ###################################################
#' Generate random unit vector.
#'
#' This function generates a random unit vector in
#' in dimension d.
#'
#' @param d - The dimension.
#' @return A unit vector in dimension d.
#' @export
generateRandomUnitVector = function(d)
{
    x = rnorm(d)
    return(x / Norm(x))
}

## ###################################################
## ###################################################
## We need the flock of functions involved in extracting
## paths from time or pseudo-time series.  



