
## ## ##########################################################################
#' A dummy function with no other purpose than to have somewhere to put
#' all the imports.
#'
#' There really ought to be a better way to put all the importFrom comments
#' in one place.
#' @return Gornisht!
#' @importFrom pracma Norm
#' @importFrom pracma dot
#' @importFrom pracma cross
#' @importFrom rgl open3d
#' @importFrom rgl spheres3d
#' @importFrom rgl lines3d
#' @importFrom rgl points3d
dummyPlaceholder = function()
{
    
}

## ##########################################################################
## ##########################################################################
## Code for testing directionality in paths:

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
#' @param normalize - If this is set to TRUE, the function will start
#' by normalizing the input points.
#' @return This returns a vector of n spherical distances in
#' radians.
#' @export
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
        distances[i] = acos(dot(center,points[i,]))
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
#' @param statistic - One of 'median', 'mean' or 'max'
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


## ##########################################################################
## ##########################################################################
## Code for extracting paths for single cell data:




## ##########################################################################
## ##########################################################################
## Code for plotting paths and their spherical data:

## ###################################################
#' Find an orthonormal basis in dimension 3
#'
#' Given a vector, this normalizes it and then uses it as
#' the first basis vector in an orthonormal basis.  We'll use
#' this to find circles around points on the sphere.
#'
#' @param x - A vector of length 3
#' @return This function returns an orthonormal basis in
#' the the form of a 3 x 3 matrix in which the first vector is
#' parallel to v
#' @export
orthonormalBasis = function(x)
{
    x = x / Norm(x)
    B = matrix(0,nrow=3,ncol=3)
    
    ## ###################################################
    ## The first basis element:
    B[1,] = x
    
    ## ###################################################
    ## Find a vector not colinear with x:
    y = rep(0,3)
    while(dot(x,y) < 1e-3)
        y = generateRandomUnitVector(3)
    
    ## ###################################################
    ## Get the portion of y perpendicular to x:
    y = y - dot(x,y) * x
    y = y / Norm(y)
    B[2,] = y
    
    ## ###################################################
    ## The third basis element is the cross product:
    B[3,] = cross(x,y)
    
    return(B)
}

## ###################################################
#' Circle on the unit sphere
#'
#' Find a circle on the unit sphere
#'
#' Given a point on the unit sphere and a radius given
#' as a spherical distance, this finds the circle.
#'
#' It's not clear to me this should be exported, but it's
#' handy to do this for testing and debugging.
#'
#' @param center - The center of the circle.
#' @param radius - The radius of the circle.
#' @param N - The number of segments to approximate the circle. It
#'     defaults to 36.
#' @return This returns an approximation to the the circle as a N+1 x
#'     3 matrix
#' @export
circleOnTheUnitSphere = function(center,radius,N=36)
{
    ## ###################################################
    ## For sanity:
    center = center / Norm(center)
    
    ## ###################################################
    ## Get the orthonormal basis:
    B = orthonormalBasis(center)

    ## ###################################################
    ## The planar center of the circle:
    ctr = cos(radius) * center

    ## ###################################################
    ## The planar radius of the circle:
    R = sin(radius)

    ## ###################################################
    ## We sweep out the circle with these:
    x = R * B[2,]
    y = R * B[3,]

    theta = (0:N) * (2 * pi / N)
    circle = matrix(0,nrow=N+1,ncol=3)
    for(i in 1:(N+1))
        circle[i,] = ctr + cos(theta[i]) * x + sin(theta[i]) * y

    return(circle)
}


## ###################################################
#' Plot a path, its projection, its center and its circle
#'
#' This function assumes you have a path in dimension 3 and you have
#' found the projection for the portion under consideration, the
#' center for its projection and the circle (i.e., radius) for the
#' appropriate statistic.  Scales the path to keep it comparable to
#' the sphere and plots all this in your favorite color.  It can be
#' called repeatedly to add additional paths in different colors.
#'
#' @param path - A path of dimension 3 in the form of an N x 3 matrix.
#' @param start - The starting place of the section under
#'     consideration.  This is used for marking the relevant
#'     portion. It defaults to 1.
#' @param end - Likewise.  It defaults to nrow(path).
#' @param projection - The projection of the relevant portion of the
#'     path.
#' @param center - The center of the projection points.
#' @param radius - The radius of the circle.
#' @param color - The color to use for this path and its associated
#'     data.
#' @param scale - The path will be start (its actual start) at 0 and
#'     will be scaled so that its most distant point will be at this
#'     distance from the origin.  This is to keep it comparable in
#'     size to the sphere. It defaults to 2 Caution should be used
#'     here when plotting multiple paths.
#' @param newFigure - When plotting a single figure or the first of
#'     multiple figures, this should be set to TRUE which is its
#'     default.  Otherwise, set this to FALSE in order to add
#'     additional paths to the same figure.
#' @export
plotPathProjectionCenterAndCircle = function(path,start,end,
                                             projection,
                                             center,
                                             radius=2,
                                             color,
                                             scale,
                                             newFigure=TRUE)
{
    ## ###################################################
    ## Constants.  Maybe they should become parameters?
    pathPointSize = 10
    projectionPointSize = 10
    centerSize = 15
    pathLineSize = 8
    circleLineSize = 8
    

    ## ###################################################
    ## Translate the path to begin at the origin and scale:

    ## ###################################################
    ## Are we starting a new figure?

    ## ###################################################
    ## Plot the path and mark the relevant portion:

    ## ###################################################
    ## Plot the projection:

    ## ###################################################
    ## Plot the center:

    ## ###################################################
    ## Plot the circle:    


}
