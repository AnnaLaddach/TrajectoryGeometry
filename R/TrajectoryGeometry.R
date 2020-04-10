

## ##########################################################################
#' Test a path for directionality
#'
#' This is the core function of this package.  It takes a path, and a
#' choice of statistical measure and computes a statistical significance
#' for the directionality of that path.
#'
#' @param path - An n x m matrix representing a series of n points in
#' dimension m.
#' @param start - The starting place along the path which will be treated as
#' the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to nrow(path).
#' @param d - The dimension under consideration.  This defaults to ncol(path)
#' @param statistic - Allowable values are 'median', 'mean' or 'max'
#' @param preserveLengths - If this is set to TRUE, the length of the
#' steps in the randomized paths will match those of the given path.
#' @param N - The number of random paths to generated for statistical
#' comparison to the given path.
#' @return This returns a p-value for the directionality of the given path.
#' @export
testPathForDirectionality = function(path,start=1,end=nrow(path),d=ncol(path),
                                     statistic,preserveLength=TRUE,N)
{


}


## ##########################################################################
#' Project a path onto the unit sphere
#'
#' This function takes a path in d dimensional space and projects it onto
#' the d-1 sphere.  It takes as additional arguments the starting and ending points 
#' under consideration and the dimension to be considered.
#'
#' @param path - This is an mxn dimensional matrix. Each row is considered
#' a point.
#' @param start - The starting place along the path which will be treated as
#' the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to nrow(path).
#' @param d - The dimension under consideration.  This defaults to ncol(path)
#' @return This returns a projection of the path onto the d-1 sphere in the form
#' of a (end - start) x d matrix.
#' @export
projectPathToSphere = function(path,start=1,end=nrow(path),d=ncol(path))
{


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
#' @param points - A set of n points on the (d-1) sphere
#' given as an n x d matrix.
#' @param statistic - The statistic to be minimized.  Allowable
#' values are 'median','mean' or 'max'.
#' @param testNorm - If this is set to TRUE, the function
#' tests whether the given points have norm within epsilon
#' of 1.
#' @param epsilon - The tolerance used in the afore-mentioned
#' test.  This defaults to 1e-6.
#' @return This returns a point in dimension d given as a
#' vector.
#' @export
findSphereClusterCenter = function(points,statistic,testNorm=TRUE,epsilon=1e-6)
{


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


}

## ###################################################
#' Find the spherical data for a given path
#'
#' This functiontakes a path and returns a list containing
#' its projection to the sphere, the center for that projection,
#' the spherical distance from the center to the points of the
#' projection and the name of the statistic used.
#'
#' @param path - This is an mxn dimensional matrix. Each row is considered
#' a point.
#' @param start - The starting place along the path which will be treated as
#' the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to nrow(path).
#' @param d - The dimension under consideration.  This defaults to ncol(path)
#' @param statistic
pathToSphericalData = function(path,start,end,d,statistic)
{

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
#' @param path - This is an mxn dimensional matrix. Each row is considered
#' a point.
#' @param start - The starting place along the path which will be treated as
#' the center of the sphere.  This defaults to 1.
#' @param end - The end point of the path.  This defaults to nrow(path).
#' @param d - The dimension under consideration.  This defaults to ncol(path)
#' @param preserveLengths - If this is set to TRUE, the steps of the random
#' paths have the same euclidean length as those of the give path.  Otherwise
#' they hav length 1.
#' @param N - The number of random paths required.
#' @return If N is 1, this returns a single random path as a matrix, othewise
#' it returns a list of N such matrices.
#' @export
generateRandomPaths = function(path,start=1,end=nrow(path),d=ncol(path),
                             preserveLengths=TRUE,N)
{


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
#' @param paths - A list of paths.  Each of these is an n x d
#' matrix.
#' @param statistic - Allowable values are 'median',
#' 'mean' or 'max'.
#' @return This returns a vector of n distances.
#' @export
getDistanceDataForPaths = function(paths,statistic)
{


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

}

## ###################################################
## ###################################################
## We need the flock of functions involved in extracting
## paths from time or pseudo-time series.  



