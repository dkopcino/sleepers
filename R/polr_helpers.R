#' Inverse logit function.
#'
#' @param x A number, a vector of numbers or other numerical structure.
#' @return Inverse logit, \code{exp(x)/(1+exp(x))}, with the same numerical strucure as \code{x}.
invlogit = function(x) {
  exp(x)/(1+exp(x))
}

#' Transform cumulative probabilities (distribution) into single probabilities.
#'
#' Transform cumulative probabilities (distribution) into single probabilities.
#' Adds 1 at the end and calculates differences respectively.
#'
#' @seealso \code{\link{invlogit}}
#'
#' @param cumprobs A vector of cumulative probabilities (without the last 1).
#' @param nms Optional, names of the probabilities values.
#' @return Vector of single probabilities.
toprobabilities = function(cumprobs, nms = NULL) {
  cumprobs = c(cumprobs, 1)
  cumprobs[2:length(cumprobs)] = unlist(lapply(2:length(cumprobs), function(i) cumprobs[i]-cumprobs[i-1]))
  if (!is.null(nms)) names(cumprobs) = nms
  cumprobs
}

