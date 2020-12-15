
## ###################################################
#' Straight path
#'
#' A path of n points in dimension d is an n x d matrix.  This
#' particular path is relatively straight.
#'
#' @format A 14 x 3 matrix
#' \describe{
#' The path is roughly in the (1,0,0) direction.
#' }
#' @source Synthetic data
"straightPath"

## ###################################################
#' Straight path projection
#'
#' The projection of straightPath onto the unit sphere.  This is a
#' collection of 13 unit points in dimension 3
#'
#' @format A 13 x 3 matrix
#' \describe{
#' The projection of straightPath[2:14,] onto the unit sphere as seen
#'     from straightPath[1,] .
#' }
#' @source Synthetic data
"straightPathProjection"


## ###################################################
#' Straight path center
#'
#' The point on the unit sphere minimizing mean spherical distance to
#'     the projection so straightPath
#'
#' @format A vector of length 3
#' \describe{
#' A unit vector of length 3 minimizing distance to the points of the
#'     projection of straightPath.
#' }
#' @source Synthetic data.
"straightPathCenter"

## ###################################################
#' Straight path radius
#'
#' The mean spherical distance from the points of the projection of
#' straightPath to the point minimizing this mean distance.
#'
#' @format Numeric
#' \describe{
#' The mean spherical distance from the points of the projection of
#' straightPath to the point minimizing this mean distance.
#' }
#' @source Synthetic data
"straightPathRadius"

## ###################################################
## ###################################################


## ###################################################
#' Crooked path
#'
#' A path of n points in dimension d is an n x d matrix.  This
#' particular path is relatively crooked.
#'
#' @format A 14 x 3 matrix
#' \describe{
#' This path changes direction after the 5th point.
#' }
#' @source Synthetic data
"crookedPath"

## ###################################################
#' Crooked path projection
#'
#' The projection of the last 8 points on crookedPath onto the unit
#' sphere as seen from the the 6th.  This is a collection of 8 unit
#' points in dimension 3.
#'
#' @format An 8 x 3 matrix
#' \describe{
#' The projection of crookedPath[7:14,] onto the unit sphere as seen
#'     from crookedPath[6,] .
#' }
#' @source Synthetic data
"crookedPathProjection"


## ###################################################
#' Crooked path center
#'
#' The point on the unit sphere minimizing mean spherical distance to
#'     the projection so crookedPath
#'
#' @format A vector of length 3
#' \describe{ A unit vector of length 3
#'     minimizing mean spherical distance to the points of the
#'     projection of crookedPath.
#' }
#' @source Synthetic data.
"crookedPathCenter"

## ###################################################
#' Crooked path radius
#'
#' The mean spherical distance from the points of the projection of
#' crookedPath to the point minimizing this mean distance.
#'
#' @format Numeric
#' \describe{
#' The mean spherical distance from the points of the projection of
#' crookedPath to the point minimizing this mean distance.
#' }
#' @source Synthetic data
"crookedPathRadius"

## ###################################################
#' Oscillation
#'
#' This a path which prepends small oscillations to straight path.
#' Its purpose is to illustrate instability of spherical projection
#' near the beginning of a path.
#'
#' @format A matrix
#' \describe{
#' This a path which prepends small oscillations to straight path.
#' Its purpose is to illustrate instability of spherical projection
#' near the beginning of a path.
#' }
#' @source Synthetic data
"oscillation"

## ###################################################
#' singleCellMatrix
#'
#' PCA projections derived from normalised gene expression values for
#' single cells. The columns are the PCs and the rows are the cells.
#'
#' @format A matrix
#' \describe{
#' PCA projections derived from normalised gene expression values for
#' single cells. The columns are the PCs and the rows are the cells.
#' }
#' @source Single cell data
"singleCellMatrix"

## ###################################################
#' hepAttributes
#'
#' PCA projections derived from normalised gene expression values for
#' single cells, and filtered for cells which feature in a trajectory
#' from hepatoblast to hepatocyte. The columns are the PCs and the
#' rows are the cells.
#'
#' @format A matrix
#' \describe{
#' PCA projections derived from normalised gene expression values for
#' single cells, and filtered for cells which feature in a trajectory
#' from hepatoblast to hepatocyte. The columns are the PCs and the
#' rows are the cells.
#' }
#' @source Single cell data
"hepAttributes"

## ###################################################
#' cholAttributes
#'
#' PCA projections derived from normalised gene expression values for
#' single cells, and filtered for cells which feature in a trajectory
#' from hepatoblast to cholangiocyte. The columns are the PCs and the
#' rows are the cells.
#'
#' @format A matrix
#' \describe{
#' PCA projections derived from normalised gene expression values for
#' single cells, and filtered for cells which feature in a trajectory
#' from hepatoblast to cholangiocyte. The columns are the PCs and the
#' rows are the cells.
#' }
#' @source Single cell data
"cholAttributes"

## ###################################################
#' hepPseudoTime
#'
#' A vector of pseudotime values for a trajectory describing the
#' development of hepatocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#'
#' @format A vector
#' \describe{
#' A vector of pseudotime values for a trajectory describing the
#' development of hepatocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#' }
#' @source Single cell data
"hepPseudoTime"

## ###################################################
#' cholPseudoTime
#'
#' A vector of pseudotime values for a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#'
#' @format A vector
#' \describe{
#' A vector of pseudotime values for a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#' }
#' @source Single cell data
"cholPseudoTime"

## ###################################################
#' hepPseudoTimeNormalised
#'
#' A vector of pseudotime values, normalised to range from
#' 0 to 100, for a trajectory describing the
#' development of hepatocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#'
#' @format A vector
#' \describe{
#' A vector of pseudotime values for a trajectory describing the
#' development of hepatocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#' }
#' @source Single cell data
"hepPseudoTimeNormalised"

## ###################################################
#' cholPseudoTimeNormalised
#'
#' A vector of pseudotime values, normalised to range from
#' 0 to 100, for a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#'
#' @format A vector
#' \describe{
#' A vector of pseudotime values, normalised to range from
#' 0 to 100, for a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' Pseudotime values have been inferred using the SlingShot
#' package. The vector is named according to cell ID.
#' }
#' @source Single cell data
"cholPseudoTimeNormalised"

## ###################################################
#' hepAnswers
#'
#' Results of running analyseSingleCellTrajectory() on a trajectory describing
#' the development of hepatocytes from hepatoblasts.
#' @format A list
#' \describe{
#' Results of running analyseSingleCellTrajectory() on a trajectory describing
#' the development of hepatocytes from hepatoblasts.
#' }
#' @source Single cell data
"hepAnswers"

## ###################################################
#' cholAnswers
#'
#' Results of running analyseSingleCellTrajectory() on
#' a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' @format A list
#' \describe{
#' Results of running analyseSingleCellTrajectory() on
#' a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' }
#' @source Single cell data
"cholAnswers"

## ###################################################
#' cholBranchPointResults
#'
#' Results of running analyseBranchPoint() on
#' a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' @format A list
#' \describe{
#' Results of running analyseBranchPoint() on
#' a trajectory describing the
#' development of cholangiocytes from hepatoblasts.
#' }
#' @source Single cell data
"cholBranchPointResults"
