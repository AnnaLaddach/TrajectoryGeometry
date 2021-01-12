
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
"straight_path"

## ###################################################
#' Straight path projection
#'
#' The projection of straight_path onto the unit sphere.  This is a
#' collection of 13 unit points in dimension 3
#'
#' @format A 13 x 3 matrix
#' \describe{
#' The projection of straight_path[2:14,] onto the unit sphere as seen
#'     from straight_path[1,] .
#' }
#' @source Synthetic data
"straight_path_projection"


## ###################################################
#' Straight path center
#'
#' The point on the unit sphere minimizing mean spherical distance to
#'     the projection so straight_path
#'
#' @format A vector of length 3
#' \describe{
#' A unit vector of length 3 minimizing distance to the points of the
#'     projection of straight_path.
#' }
#' @source Synthetic data.
"straight_path_center"

## ###################################################
#' Straight path radius
#'
#' The mean spherical distance from the points of the projection of
#' straight_path to the point minimizing this mean distance.
#'
#' @format Numeric
#' \describe{
#' The mean spherical distance from the points of the projection of
#' straight_path to the point minimizing this mean distance.
#' }
#' @source Synthetic data
"straight_path_radius"

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
"crooked_path"

## ###################################################
#' Crooked path projection
#'
#' The projection of the last 8 points on crooked_path onto the unit
#' sphere as seen from the the 6th.  This is a collection of 8 unit
#' points in dimension 3.
#'
#' @format An 8 x 3 matrix
#' \describe{
#' The projection of crooked_path[7:14,] onto the unit sphere as seen
#'     from crooked_path[6,] .
#' }
#' @source Synthetic data
"crooked_path_projection"


## ###################################################
#' Crooked path center
#'
#' The point on the unit sphere minimizing mean spherical distance to
#'     the projection of crooked_path
#'
#' @format A vector of length 3
#' \describe{ A unit vector of length 3
#'     minimizing mean spherical distance to the points of the
#'     projection of crooked_path.
#' }
#' @source Synthetic data.
"crooked_path_center"

## ###################################################
#' Crooked path radius
#'
#' The mean spherical distance from the points of the projection of
#' crooked_path to the point minimizing this mean distance.
#'
#' @format Numeric
#' \describe{
#' The mean spherical distance from the points of the projection of
#' crooked_path to the point minimizing this mean distance.
#' }
#' @source Synthetic data
"crooked_path_radius"

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
#' single_cell_matrix
#'
#' PCA projections derived from normalised gene expression values for
#' single cells. The columns are the PCs and the rows are the cells.
#'
#' @format A matrix
#' \describe{
#' PCA projections derived from normalised gene expression values for
#' single cells. The columns are the PCs and the rows are the cells.
#' }
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"single_cell_matrix"

## ###################################################
#' hep_attributes
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"hep_attributes"

## ###################################################
#' chol_attributes
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"chol_attributes"

## ###################################################
#' hep_pseudo_time
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"hep_pseudo_time"

## ###################################################
#' chol_pseudo_time
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"chol_pseudo_time"

## ###################################################
#' hep_pseudo_time_normalised
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"hep_pseudo_time_normalised"

## ###################################################
#' chol_pseudo_time_normalised
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"chol_pseudo_time_normalised"

## ###################################################
#' hep_answers
#'
#' Results of running analyseSingleCellTrajectory() on a trajectory describing
#' the development of hepatocytes from hepatoblasts.
#' @format A list
#' \describe{
#' Results of running analyseSingleCellTrajectory() on a trajectory describing
#' the development of hepatocytes from hepatoblasts.
#' }
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"hep_answers"

## ###################################################
#' chol_answers
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"chol_answers"

## ###################################################
#' chol_branch_point_results
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
#' @source Single-cell data has been obtained from GEO (GSE90047) and the script
#'     used for upstream processing is available at
#'     https://github.com/AnnaLaddach/TrajectoryGeometryData
"chol_branch_point_results"
