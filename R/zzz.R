#' @import Kendall
#' @import KernSmooth
#' @import lmtest
#' @import nortest
#' @import som
#' @import spam
#' @import stats
NULL


# Message when package loaded
.onAttach <- function(lib, pkg) {
    packageStartupMessage("\nearlywarnings Copyright (C) 2011-2015 Vasilis Dakos and Leo Lahti\nhttp://www.early-warning-signals.org\n")
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
  smYY <- stats::ksmooth(timeindex, Y, kernel = c("normal"), bandwidth = bw,
    range.x = range(timeindex),n.points = length(timeindex))
  list(nsmY = Y - smYY$y, smY = smYY$y)
}

detrending_linear <- function(Y, timeindex) {
  list(
    nsmY = stats::resid(stats::lm(Y ~ timeindex)),
    smY = stats::fitted(stats::lm(Y ~ timeindex))
  )
}

detrending_first_diff <- function(Y, timeindex) {
  list(nsmY = diff(Y), smY = Y)
}

detrending_loess <- function(Y, timeindex, degree = NULL, span = NULL) {
  span <- ifelse(is.null(span), .25, .01*span)
  if (is.null(degree)) degree <- 2
  smYY <- stats::loess(Y ~ timeindex, span = span, degree = degree, normalize = FALSE,
      family = "gaussian")
  smY <- stats::predict(smYY, data.frame(x = timeindex), se = FALSE)
  list(
    smY = smY,
    nsmY = Y - smY
  )
}


# print sutff
add_stars <- function(n = 60) {
  print(paste(rep("*", n), collapse = ""), quote = FALSE)
}
