
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
