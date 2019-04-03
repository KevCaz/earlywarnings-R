#' @import Kendall
#' @import KernSmooth
#' @import lmtest
#' @import nortest
#' @import som
#' @import spam
#' @import stats
NULL

#' Description: Get group assigment indices for univariate data points, given cluster break points
#'
#' Arguments:
#' @param x Univariate data vector
#' @param breakpoints Cluster breakpoints
#'
#' Returns:
#'   @return A vector of cluster indices
#'
#' @export
#'
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @examples #
#'
#' @keywords early-warning

UnivariateGrouping <- function(x, breakpoints) {
    g <- rep.int(NA, length(x))
    mps <- c(breakpoints, Inf)
    for (i in 1:length(mps)) {
        g[x <= mps[[i]] & is.na(g)] <- i
    }
    g
}


#' detrending
detrending_ews <- function(detrending, Y, timeindex, bandwidth, bw, degree = NULL, span = NULL) {
  switch(detrending,
    gaussian = detrending_gaussian(Y, timeindex, bandwidth, bw),
    linear = detrending_linear(Y, timeindex),
    first_diff = detrending_first_diff(Y, timeindex),
    loess = detrending_loess(Y, timeindex, degree = degree, span = span),
    no = list(smY = Y, nsmY = Y)
  )
}

detrending_gaussian <- function(Y, timeindex, bandwidth, bw) {
  bw <- ifelse(is.null(bandwidth), is.null(bandwidth),
        round(length(Y) * .01*bandwidth))
  smYY <- ksmooth(timeindex, Y, kernel = c("normal"), bandwidth = bw,
    range.x = range(timeindex),n.points = length(timeindex))
  list(nsmY = Y - smYY$y, smY = smYY$y)
}

detrending_linear <- function(Y, timeindex) {
  list(
    nsmY =  resid(lm(Y ~ timeindex)),
    smY = fitted(lm(Y ~ timeindex))
  )
}

detrending_first_diff <- function(Y, timeindex) {
  list(nsmY = diff(Y), smY = Y)
}

detrending_loess <- function(Y, timeindex, degree = NULL, span = NULL) {
  span <- ifelse(is.null(span), .25, .01*span)
  if (is.null(degree)) degree <- 2
  smYY <- loess(Y ~ timeindex, span = span, degree = degree, normalize = FALSE,
      family = "gaussian")
  list(
    smY = predict(smYY, data.frame(x = timeindex), se = FALSE),
    nsmY = Y - smY
  )
}
