
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
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param to - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @param statistic - Allowable values are 'median', 'mean' or 'max'
#' @param randomizationParams - A character vector which is used to
#'     control the production of randomized paths for comparison.
#' @param N - The number of random paths to generated for statistical
#'     comparison to the given path.
#' @return This returns a list giving whose entries are:
#'   pValue - the p-value for the path and statistic in question;
#'   sphericalData - a list containing the projections of the path to
#'     the sphere, the center of that sphere and the statistic for
#'     distance to that center;
#'   randomDistances - the corresponding distances for randomly chosen;
#'     paths;
#'   randomizationParams - the choice of randomization parameters
#' @export
#' @importFrom rgl open3d spheres3d lines3d points3d
#' @importFrom pracma Norm dot cross
#' @examples
#' randomizationParams = c('byPermutation','permuteWithinColumns')
#' p = testPathForDirectionality(path=straightPath,
#'                               randomizationParams=randomizationParams,
#'                               statistic='median',N=100)
#' q = testPathForDirectionality(path=crookedPath,from=6,
#'                               randomizationParams=randomizationParams,
#'                               statistic='median',N=100)
testPathForDirectionality = function(path,from=1,to=nrow(path),d=ncol(path),
                                     randomizationParams,statistic,N)
{
    testPathForDirectionalityTest(path,from,to,d,
                                  randomizationParams,statistic,N)
    
    ## ###################################################
    ## Subset to the data under consideration:
    ## path = path[from:to,1:d]
    
    ## ###################################################
    ## Get the spherical data:
    sphericalData = getSphericalData(path,statistic)
    
    ## ###################################################
    ## Generate random paths:
    randomPaths = generateRandomPaths(path,
                                      randomizationParams=randomizationParams,
                                      N=N)
    
    ## ###################################################
    ## Compute the distance statistics for random paths:
    distances = getDistanceDataForPaths(randomPaths,statistic)
    
    
    ## ###################################################
    ## Return the p-value:
    idx = distances <= sphericalData$distance 
    pValue = max(1,sum(idx)) / N

    answer = list()
    answer$pValue = pValue
    answer$sphericalData = sphericalData
    answer$randomDistances = distances
    answer$randomizationParams = randomizationParams

    return(answer)
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
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param to - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @return This returns a projection of the path onto the d-1 sphere
#'     in the form of a (to - from) x d matrix.
#' @export
#' @examples
#' projection1 = projectPathToSphere(straightPath)
#' projection2 = projectPathToSphere(crookedPath,from=6)
projectPathToSphere = function(path,from=1,to=nrow(path),d=ncol(path))
{
    projectPathToSphereTest(path,from,to,d)
    
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[from:to,1:d]
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
#' @importFrom stats median optim
#' @export
#' @examples
#' projection = projectPathToSphere(straightPath)
#' center = findSphereClusterCenter(projection,'mean')
findSphereClusterCenter = function(points,statistic,normalize=FALSE)
{
    findSphereClusterCenterTest(points,statistic,normalize)
    
    n = nrow(points)
    
    ## ###################################################
    # Normalize if necessary:
    if(normalize)
    {
        for(i in seq_len(n))
            points[i,] = points[i,] / Norm(points[i,])
    }

    ## ###################################################
    ## Function to be minimised
    minFunction = function(x){
      norm = Norm(x)
      unitVector = x/norm
      if (norm < 0.1){
        return(100)
      } 
      distances = findSphericalDistance(unitVector,points)
      if (statistic == "max"){
        return(max(distances))
      }
      
      if (statistic == "median"){
        return(median(distances))
      }
      
      if (statistic == "mean"){
        return(mean(distances))
      }
    }
    ## ###################################################
    ## Choose a possible center as a start for minimisation
    meanPoint = colMeans(points)
    optimStart = meanPoint/Norm(meanPoint)
    
    ## ###################################################
    ## Find center which minimises spherical distances.
    minResult = optim(optimStart, minFunction, control = list(maxit = 1000))
    
    center = minResult$par/Norm(minResult$par)
    return(center)
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
#' @examples
#' distances = findSphericalDistance(straightPathCenter,straightPathProjection)
findSphericalDistance = function(center,points,normalize=FALSE)
{
    findSphericalDistanceTest(center,points,normalize)
    
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
        distances[i] = acos(round(dot(center,points[i,]),7))
    }
    
    return(distances)
}

## ###################################################
#' This is a simplified wrapper for pathToSphericalData
#'
#' It handles the case in which from, to and d are all
#' given by the dimensions of the path
#'
#' @param path - an m x n matrix.  Each row is considered a point
#' @param statistic - one of 'mean','median' or 'max'
#' @return This function returns a list whose elements are the
#'     projections of the path to the sphere, the center for those
#'     projections, the median, mean or max distance from the center
#'     to those projections and the name of the statistic used.
#' @export
#' @examples
#' sphericalData = getSphericalData(straightPath,'max')
getSphericalData = function(path,statistic)
{
    getSphericalDataTest(path,statistic)
    
    from = 1
    to = nrow(path)
    d = ncol(path)

    return(pathToSphericalData(path,from,to,d,statistic))
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
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param to - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @param statistic - One of 'median', 'mean' or 'max'
#' @return This function returns a list whose elements are the
#'     projections of the path to the sphere, the center for those
#'     projections, the median, mean or max distance from the center
#'     to those projections and the name of the statistic used.
#' @export
#' @examples
#' sphericalData = pathToSphericalData(straightPath,from=1,to=nrow(straightPath),
#'                                     d=3,statistic='median')
pathToSphericalData = function(path,from,to,d,statistic)
{
    pathToSphericalDataTest(path,from,to,d,statistic)
    
    returnValues = list()
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[from:to,1:d]
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
#' dimension and length based on it.  This can be done either by
#' permuting the entries in path or by taking steps from the initial
#' point of path.  Exact behaviour is controlled by
#' randomizationParams. 
#' 
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param to - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @param randomizationParams - A character vector controling the
#'     randomization method used.  It's first entry must be either
#'     'byPermutation' or 'bySteps'  See the vignette for further
#'     details. 
#' @param N - The number of random paths required.
#' @return This function returns a list of random paths.  Each path is
#'     a matrix.
#' @export
#' @examples
#' randomizationParams = c('byPermutation','permuteWithinColumns')
#' randomPaths = generateRandomPaths(crookedPath,from=6,to=nrow(crookedPath),
#'               d=ncol(crookedPath),randomizationParams=randomizationParams,N=10)
generateRandomPaths = function(path,from=1,to=nrow(path),d=ncol(path),
                               randomizationParams,N)
{
    generateRandomPathsTest(path,from,to,d,randomizationParams,N)
    
    if(! randomizationParams[1] %in% c('byPermutation','bySteps'))
    {
        msg = "randomizationParams[1] must be either 'byPermutation'or 'bySteps'"
        stop(msg)
    }

    ## ###################################################
    ## Subset to the data under consideration:
    path = path[from:to,1:d]

    if(randomizationParams[1] == 'byPermutation')
        return(generateRandomPathsByPermutation(path,
                                               randomizationParams,
                                               N))

    if(randomizationParams[1] == 'bySteps')
        return(generateRandomPathsBySteps(path,
                                         randomizationParams,
                                         N))    
    
}


## ###################################################
#' Produce randomized paths by permutation
#'
#' This function produces randomized paths from a given path via
#' permutation of its entries.  This can be done either by random
#' permutation within each column thereby preserving the range of
#' values within each column or by random permutation of all entries
#' in the matrix.
#'
#' @param path - An n x d matrix.
#' @param randomizationParams - A character vector used to control the
#'     behavior of the function.
#' @param N - The number of paths required.
#' @return This function returns a list of random paths.
generateRandomPathsByPermutation =
    function(path,randomizationParams,N)
{
    randomPathList = list()
    n = nrow(path)
    d = ncol(path)

    ## ###################################################
    ## This handles the case where we wish to permute within each column:
    if(randomizationParams[2] == 'permuteWithinColumns')
    {
        for(i in seq_len(N))
        {
            randomPath = path
            for(j in seq_len(d))
            {
                perm = sample(n,n)
                randomPath[,j] = randomPath[perm,j]
            }
            randomPathList[[i]] = randomPath   
        }
        return(randomPathList)
    }

    ## ###################################################
    ## This handles the case where we wish to permute at random:
    if(randomizationParams[2] == 'permuteAsMatrix')
    {
        M = n * d
        a = as.numeric(path)
        for(i in seq_len(N))
        {
            b = as.numeric(path)
            perm = sample(M,M)
            b = b[perm]
            randomPathList[[i]] = matrix(b,nrow=n)
        }
        return(randomPathList)
    }              
}

## ###################################################
#' Produce randomized paths by steps
#'
#' This function produces randomized paths from a given path by taking
#' steps in space. This can be done either requiring that these steps
#' have the same Euclidean length as those of the original path or
#' allowing them to all have unit length.  It can also be done
#' requiring the path to stay in the non-negative orthant or allowing
#' arbitrary values.
#'
#' @param path - An n x d matrix.
#' @param randomizationParams - A character vector used to control the
#'     behavior of the function.
#' @param N - The number of paths required.
#' @return This function returns a list of random paths.
generateRandomPathsBySteps = function(path,randomizationParams,N)
{
    n = nrow(path)
    d = ncol(path)
    
    ## ###################################################
    ## Find the length of the steps if required:
    stepLengths = rep(1,n-1)
    if('preserveLengths' %in% randomizationParams)
        stepLengths = getStepLengths(path)
    
    
    ## ###################################################
    ## Generate the random paths:
    randomPathList = list()
    for(i in seq_len(N))
    {
        randomPath = matrix(0,nrow=n,ncol=d)
        randomPath[1,] = path[1,]
        
        for(j in seq_len(n-1))
            randomPath[j+1,] = randomPath[j,] +
                stepLengths[j] * generateRandomUnitVector(d)

        if('nonNegative' %in% randomizationParams)
        {
            idx = randomPath < 0
            randomPath[idx] = - randomPath[idx]
        }
        
        randomPathList[[i]] = randomPath
    }
    
    return(randomPathList)
}


## ###################################################
#' Find the step lengths:
#'
#' This finds the lengths of the steps along a path
#'
#' @param path - This is an mxn dimensional matrix. Each row is
#'     considered a point.
#' @param from - The starting place along the path which will be
#'     treated as the center of the sphere.  This defaults to 1.
#' @param to - The end point of the path.  This defaults to
#'     nrow(path).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(path)
#' @return This function returns the length of each step in a path.
#' @export
#' @examples
#' stepLengths = getStepLengths(path=crookedPath)
#' stepLengths = getStepLengths(path=crookedPath,from=4)
getStepLengths = function(path,from=1,to=nrow(path),d=ncol(path))
{
    getStepLengthsTest(path,from,to,d)
    
    ## ###################################################
    ## Subset to the data under consideration:
    path = path[from:to,1:d]
    n = nrow(path)
    
    stepLengths = numeric(n-1)
    for(i in seq_len(n-1))
        stepLengths[i] = Norm(path[i+1,] - path[i,])
    
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
#' @examples
#' paths =
#'     generateRandomPaths(path=straightPath,randomizationParam='bySteps',N=5)
#' distance = getDistanceDataForPaths(paths=paths,statistic='max')
getDistanceDataForPaths = function(paths,statistic)
{
    getDistanceDataForPathsTest(paths,statistic)
    
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
#' @importFrom stats rnorm
#' @export
#' @examples
#' randomUnitVector = generateRandomUnitVector(5)
generateRandomUnitVector = function(d)
{
    generateRandomUnitVectorTest(d)
    
    x = rnorm(d)
    return(x / Norm(x))
}

## ###################################################
#' Measure a path's progression
#'
#' This function measures the progress of a path in a specified
#' direction.  This direction will typically be the center of its
#' projection onto the sphere as revealed using your favorite
#' statistic.
#'
#' @param path - An n x d matrix
#' @param from - The point along the path to be taken as the starting
#'     point.  This defaults to 1.
#' @param to - The point along the path to be used as the end point.
#'     This defaults to nrow(path).
#' @param d - The dimension to be used.  This defaults to ncol(path).
#' @param direction - A non-zero numeric whose length is the the
#'     dimension.
#' @return This returns a numeric given the signed distance projection
#'     of the path along the line through its starting point in the
#'     given direction.
#' @export
#' @examples
#' progress =
#'     pathProgression(straightPath,direction=straightPathCenter)
#' progress =
#'     pathProgression(crookedPath,from=6,direction=crookedPathCenter)
pathProgression = function(path,from=1,to=nrow(path),d=ncol(path),
                           direction)
{
    pathProgressionTest(path,from,to,d,direction)
    
    path = path[from:to,1:d]
    direction = direction / Norm(direction)
    distance = numeric(nrow(path)-1)
    for(i in 2:nrow(path))
    {
        delta = path[i,] - path[1,]
        distance[i-1] = dot(delta,direction)
    }
    return(distance)
}


## ##########################################################################
#' Sample a path from single cell data.
#'
#' This function takes vector of pseudotime values, and a matrix of attribute values (cell x attribute).
#' It also optionally takes the number of pseudotime windows to sample a single cell from. This defaults to 10.
#' The function returns a matrix of sampled attribute values which form the coordinates of the
#' sampled path. The matrix of attribute values should consist of numeric values
#' relevant to a pseudotime trajectory i.e. gene expression values or PCA projections.
#' The vector of pseudotime values should be named according to cell names. 
#' Simarly the row names of the matrix of attribute values should be cell names.
#' Row names for the returned matrix of the sampled path give the window number a cell was sampled from.
#'
#' @param attributes - An n x d (cell x attribute) matrix of numeric attributes for single cell data. Rownames should be cell names.
#' @param pseudotime - A named numeric vector of pseudotime values for cells. 
#' @param nWindows - The number of windows pseudotime should be split into to sample cells from. Defaults to 10.
#' @return sampledPath - A path consisting of a matrix of attributes of sampled cells. The rownames refer to the pseudotime windows
#'  each cell was sampled from.
#' @export
#' @examples
#' samplePath(cholAttributes, cholPseudoTimeNormalised)
#' samplePath(hepAttributes, hepPseudoTimeNormalised)
samplePath = function(attributes, pseudotime, nWindows = 10){
  
    samplePathTest(attributes, pseudotime, nWindows)

    ## ###################################################
    ## Set parameters for path.
    start = min(pseudotime)	
    end = max(pseudotime)
    pathLength = end - start
    windowSize = pathLength/nWindows

    sampledPath = matrix(, nrow = 0, ncol = ncol(attributes))

    ## Vector to save window number as pseudotime windows with no cells will be skipped.
    windowNumber = c()

    for (i in 1:nWindows){
  	  cells = names(pseudotime[pseudotime >= (i-1) * windowSize + start & pseudotime < i * windowSize + start])
  	  windowAttributes =  attributes[cells,]
  	
  	  ## ###################################################
    	## Case when only one cell falls within a pseudotime window.
    	## Turn windowAttributes into a matrix.
    	if (is.null(dim(windowAttributes))){
          	    windowAttributes = t(matrix(windowAttributes))
        	}
    	 
    	## ###################################################
    	## Case when no cells fall within a pseudotime window.
    	if (nrow(windowAttributes) == 0){
      	    next
    	}
    	
    	## ###################################################
    	## Randomly sample cell from pseudotime window.
    	chosenIndex = sample(1:nrow(windowAttributes), 1)
      chosenAttributes = windowAttributes[chosenIndex,]
    	
    	sampledPath = rbind(sampledPath, chosenAttributes)
    	
    	## Save window number.
    	windowNumber = c(windowNumber, i)
      }	
      
    rownames(sampledPath) = windowNumber 
    return(sampledPath)
}


## ##########################################################################
#' Analyse a single cell trajectory.
#'
#' This function analyses a single cell trajectory by sampling multiple paths and comparing each path to random paths.
#' It takes vector of pseudotime values, and a matrix of attribute values (cell x attribute).
#' It also optionally takes the number of pseudotime windows to sample a single cell from. This defaults to 10.
#' The function returns a list of Answers for each comparison of a sampled path to a random path
#'
#' @param attributes - An n x d (cell x attribute) matrix of numeric attributes for single cell data. Rownames should be cell names.
#' @param pseudotime - A named numeric vector of pseudotime values for cells. 
#' @param randomizationParams - A character vector which is used to
#'     control the production of randomized paths for comparison.
#' @param statistic - Allowable values are 'median', 'mean' or 'max'.
#' @param nSamples - The number of sampled paths to generate (defaults to 1000).
#' @param nWindows - The number of windows pseudotime should be split into to sample cells from (defaults to 10).
#' @param d - The dimension under consideration.  This defaults to
#'     ncol(attributes).
#' @param N - The number of random paths to generated for statistical
#'     comparison to the given path (defaults to 1000).
#' @return This returns a list, where each entry is itself a list containing information comparing a sampled path
#'   to random paths. These entries consist of:
#'   pValue - the p-value for the path and statistic in question;
#'   sphericalData - a list containing the projections of the path to
#'     the sphere, the center of that sphere and the statistic for
#'     distance to that center;
#'   randomDistances - the corresponding distances for randomly chosen;
#'     paths;
#'   randomizationParams - the choice of randomization parameters
#' @export
#' @examples
#' cholAnswers = analyseSingleCellTrajectory(cholAttributes[,1:3], 
#'                                          cholPseudoTimeNormalised,
#'                                          nSamples = 1000, 
#'                                          randomizationParams = c('byPermutation',
#'                                                          'permuteWithinColumns'), 
#'                                          statistic = "mean", 
#'                                          N = 1)
#' hepAnswers = analyseSingleCellTrajectory(hepAttributes[,1:3], 
#'                                          hepPseudoTimeNormalised, nSamples = 1000, 
#'                                          randomizationParams = c('byPermutation',
#'                                                          'permuteWithinColumns'), 
#'                                          statistic = "mean", 
#'                                          N = 1)
analyseSingleCellTrajectory = function(attributes, 
                                       pseudotime, 
                                       randomizationParams, 
                                       statistic, 
                                       nSamples = 1000, 
                                       nWindows = 10, 
                                       d = ncol(attributes), 
                                       N = 1000)
  {
  
  analyseSingleCellTrajectoryTest(attributes, pseudotime, 
                                  randomizationParams, statistic, 
                                  nSamples, nWindows, d, N)
  
  ## ###################################################
  ## List to contain results:
  answers = list()
  
  ## ###################################################
  ## Sample paths and test each one for directionality
  for (i in 1:nSamples){
    path = samplePath(attributes, pseudotime, nWindows = nWindows)
    answers[[i]] = testPathForDirectionality(path, randomizationParams = randomizationParams, 
                                             statistic = statistic, N = N, d = d)
    if (i %% 100 == 0){
      print(paste(i, "sampled paths analysed"))
    }
  }
  return(answers)
}




## ##########################################################################
## ##########################################################################
## Code for plotting paths and their spherical data:

## ###################################################
#' Find an orthonormal basis in dimension 3
#'
#' Given a vector in R3, this normalizes it and then uses it as
#' the first basis vector in an orthonormal basis.  We'll use
#' this to find circles around points on the sphere.
#'
#' @param x - A vector of length 3
#' @return This function returns an orthonormal basis in
#' the the form of a 3 x 3 matrix in which the first vector is
#' parallel to v
#' @export
#' @examples
#' anOrthonormalBasis = orthonormalBasis(c(1,1,1))
orthonormalBasis = function(x)
{
    orthonormalBasisTest(x)
        
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
#' Find a circle on the unit 2-sphere
#'
#' Given a point on the unit 2-sphere and a radius given
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
#' @examples
#' pole = c(1,0,0)
#' radius = pi / 4
#' circle = circleOnTheUnitSphere(pole,radius)
circleOnTheUnitSphere = function(center,radius,N=36)
{
    circleOnTheUnitSphereTest(center,radius,N)
    
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
#' @param from - The starting place of the section under
#'     consideration.  This is used for marking the relevant
#'     portion. It defaults to 1.
#' @param to - Likewise.  It defaults to nrow(path).
#' @param projection - The projection of the relevant portion of the
#'     path.
#' @param center - The center of the projection points.
#' @param radius - The radius of the circle.
#' @param color - The color to use for this path and its associated
#'     data.
#' @param circleColor - Sets the colour of the circle.
#'     Defaults to white.
#' @param pathPointSize - Sets the size of points which represent the 
#'     path. Defaults to 8.
#' @param projectionPointSize - Sets the size of points which represent the 
#'     projected path. Defaults to 8.
#' @param scale - The path will be start (its actual start) at 0 and
#'     will be scaled so that its most distant point will be at this
#'     distance from the origin.  This is to keep it comparable in
#'     size to the sphere. It defaults to 1.5.  Caution should be used
#'     here when plotting multiple paths.
#' @param newFigure - When plotting a single figure or the first of
#'     multiple figures, this should be set to TRUE which is its
#'     default.  Otherwise, set this to FALSE in order to add
#'     additional paths to the same figure.
#' @export
#' @examples
#' plotPathProjectionCenterAndCircle(path=straightPath,
#'                                  projection=straightPathProjection,
#'                                  center=straightPathCenter,
#'                                  radius=straightPathRadius,
#'                                  color='red',
#'                                  newFigure=TRUE)
plotPathProjectionCenterAndCircle = function(path,
                                             from=1,
                                             to=nrow(path),
                                             projection,
                                             center,
                                             radius,
                                             color,
                                             circleColor="white",
                                             pathPointSize = 8,
                                             projectionPointSize = 8,
                                             scale=1.5,
                                             newFigure=TRUE)
{
    plotPathProjectionCenterAndCircleTest(path,from,to,projection,
                                          center,radius,color,circleColor,
                                          pathPointSize,projectionPointSize,
                                          scale,newFigure)
    
    ## ###################################################
    ## Constants.  Maybe they should become parameters?
    centerSize = 15
    pathLineWidth = 3
    circleLineWidth = 3
    relevantPortionPointHump = 4
    relevantPortionLineHump = 3
    alpha = .2
    

    ## ###################################################
    ## Translate the path to begin at the origin and scale:
    N = nrow(path)
    distances = numeric(N)
    offset = path[1,]
    for(i in seq_len(N))
    {
        path[i,] = path[i,] - offset
        distances[i] = Norm(path[i,])
    }
    path = (scale / max(distances)) * path
    
    ## ###################################################
    ## Are we starting a new figure?
    if(newFigure)
    {
        open3d()
        spheres3d(0,0,0,size=1,alpha=alpha)
    }

    ## ###################################################
    ## Plot the path and mark the relevant portion:
    points3d(path,size=pathPointSize,color=color)
    lines3d(path,lwd=pathLineWidth,color=color)

    points3d(path[from:to,],size=pathPointSize+relevantPortionPointHump,
             color=color)
    lines3d(path[from:to,],lwd=pathLineWidth+relevantPortionLineHump,
            color=color)

    ## ###################################################
    ## Plot the projection:
    points3d(projection,size=projectionPointSize,color=color)

    ## ###################################################
    ## Plot the center:
    points3d(matrix(center,nrow=1),size=centerSize,color=color)

    ## ###################################################
    ## Plot the circle:
    circle = circleOnTheUnitSphere(center,radius)
    lines3d(circle,lwd=circleLineWidth,color=circleColor)

}



## ###################################################
#' Visualise Trajectory Stats 
#'
#' This function creates boxplots and extracts statistics for comparisons of metrics 
#' for sampled paths to random paths. It can also create plots for comparing two sets
#' of sampled paths by providing the traj2Data argument.
#' 
#' @param traj1Data - the result of analyseSingleCellTrajectory
#' @param metric - either "pValue" or "distance"
#' @param average - if there are multiple distances available for each 
#' sampled trajectory, calculate the average using "mean" or "median" (defaults to "mean").
#' @param traj2Data either an empty list or the result of analyseSingleCellTrajectory
#' @return a list containing:
#'  stats - output of wilcox test (paired if comparing sampled to random paths,
#'  unpaired if comparing sampled paths for two different trajectories)
#'  values - dataframe containing plotted data in long format
#'  plot - ggplot object 
#' @importFrom ggplot2 ggplot geom_violin geom_boxplot labs aes
#' @importFrom stats wilcox.test
#' @export
#' @examples 
#' cholResultDistance = visualiseTrajectoryStats(cholAnswers, "distance")
#' hepResultDistance = visualiseTrajectoryStats(hepAnswers, "distance")
#' distanceComparison = visualiseTrajectoryStats(cholAnswers, "distance", traj2Data = hepAnswers)
visualiseTrajectoryStats = function(traj1Data,
                          metric,
                          average = "mean",
                          traj2Data = list())
  {
  
  visualiseTrajectoryStatsTest(traj1Data, metric, average, traj2Data)
  
  ## ###################################################
  ## Set averageFunc as actual function
  if (average == "mean"){
    averageFunc = mean
  }
  
  if (average == "median"){
    averageFunc = median
  }
  
  ## ###################################################
  ## Set up dataframe which will be populated with data to plot in long format
  values = data.frame(type = character(), value = numeric())
  
  ## ###################################################
  ## Code for comparing 2 trajectories
  if (length(traj2Data) > 0){
    for (i in 1:length(traj1Data)){
      
      ## ###################################################
      ## Populate values data frame with distance data
      if (metric == "distance"){
        values = rbind(values, data.frame(type = "Trajectory 1", 
                                          value = traj1Data[[i]]$sphericalData$distance))
        values = rbind(values, data.frame(type = "Trajectory 2", 
                                          value = traj2Data[[i]]$sphericalData$distance))
      }
      
      ## ###################################################
      ## Populate values data frame with pValue data
      if (metric == "pValue"){
        values = rbind(values, data.frame(type = "Trajectory 1",
                                          value = traj1Data[[i]]$pValue))
        values = rbind(values, data.frame(type = "Trajectory 2", 
                                          value = traj2Data[[i]]$pValue))
      } 
    }
    
    ## ###################################################
    ## Use unpaired wilcox test to compare values for 2 trajectories
    stats = wilcox.test(values[values$type == "Trajectory 1",]$value, 
                        values[values$type == "Trajectory 2",]$value)
  }
  
  ## ###################################################
  ## Code for comparing sampled pathways to random pathways
  if (length(traj2Data) == 0){
    
    ## Here we can only compare distance metrics
    if (metric != "distance"){
      print("Metric must be distance to compare sampled to random trajectories")
    }
    for (i in 1:length(traj1Data)){
      values = rbind(values, data.frame(type = "Sampled", 
                                        value = traj1Data[[i]]$sphericalData$distance))
      values = rbind(values, data.frame(type = "Random", 
                                        value = averageFunc(traj1Data[[i]]$randomDistances)))
    }
    
    ## ###################################################
    ## Use paired wilcox test to compare values sampled and random pathways, 
    ## as random trajectories are parametised based on the sampled pathways
    stats = wilcox.test(values[values$type == "Sampled",]$value,
                        values[values$type == "Random",]$value, paired = T)
  }
  
  ## ###################################################
  ## Create violin plot with overlaid box plot
  p = ggplot(values, aes(x=type, y=value)) + 
    geom_violin() + geom_boxplot(width=0.1) + labs(y = metric, x = "")
  #print(p) # should the default be to print this???
  results = list(stats = stats,values = values, plot=p)
  return(list(stats = stats,values = values, plot=p))
}



