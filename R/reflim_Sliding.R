#' Reference intervals based on a sliding window technique
#'
#' Computes reference intervals based on a sliding window technique. There are two variants.
#' A fixed window size and a fixed step width by which the window is shifted or windows with
#' a guaranteed minimum number of values.
#'
#' @param x Vector with lab values.
#' @param covariate The numeric covariate, e.g. age.
#' @param window.size Size of the window if a fixed window size should be used.
#' @param step.width The width by which the window should be shifted in each step if a fixed window size should be used.
#' @param lognormal Boolean indicating whether a lognormal distribution should be assumed (NULL means that the distribution type is defined automatically).
#' @param perc.trunc Percentage of presumably normal values to be removed from each side. If perc.trunc is increased (e.g. to 3.5 instead of 2.5), more values are removed.
#' @param n.min.window Minimum number of values in a window if a flexible window size should be used.
#' @param n.min minimum Number of observations needed for a robust estimate of reference intervals.
#' @param apply.rounding Boolean indicating whether the estimated reference limits should be rounded.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{lower.lim}{The computed lower reference limits.}
#'   \item{upper.lim}{The computed upper reference limits.}
#'   \item{ci.lower.lim.l}{Lower confidence limit for the lower reference limit.}
#'   \item{ci.lower.lim.u}{Upper confidence limit for the lower reference limit.}
#'   \item{ci.upper.lim.l}{Lower confidence limit for the upper reference limit.}
#'   \item{ci.upper.lim.u}{Upper confidence limit for the upper reference limit.}
#'   \item{distribution.type}{Specifies whether a normal or lognormal distribution was chosen.}
#'   \item{covariate.left}{The left limits of the covariate windows.}
#'   \item{covariate.right}{The right limits of the covariate windows.}
#'   \item{covariate.mean}{The mean values of the covariate in the interval.}
#'   \item{covariate.median}{The medians of the covariate in the interval.}
#'   \item{covariate.n}{The number of values in each window.}
#' }
#'
#' @import reflimR
#'
#' @export
sliding.reflim <- function(x, covariate, window.size=NULL, step.width=NULL, lognormal=NULL, perc.trunc=2.5, n.min.window=200, n.min=100, apply.rounding=FALSE){

  is.nona <- !is.na(x) & !is.na(covariate)
  xx <- x[is.nona]
  covcomp <- covariate[is.nona]

  ord.cov <- order(covcomp)
  xx <- xx[ord.cov]
  covcomp <- covcomp[ord.cov]

  if (!is.numeric(xx)) {stop("(reflim) x must be numeric.")}
  if (min(xx) <= 0) {stop("(reflim) only positive values allowed.")}
  n <- length(xx)
  if (n < 39) {stop(paste0("(iboxplot) n = ", n, ". The length of x should be 200 or more. The absolute minimum for reference limit estimation is 39."))}
  if (n < n.min) {
    print(noquote(paste("n =", n, "where a minimum of", n.min, "is required. You may try to reduce n.min at the loss of accuracy.")))
    return(c(mean = NA, sd = NA, lower.lim = NA, upper.lim = NA))
  }

  cov.unique <- covcomp[!duplicated(covcomp)]
  n.steps <- length(cov.unique)
  if (n.steps == 1) {stop("The covariate is constant.")}

  if (!is.null(window.size) & !is.null(step.width)) {
    n.steps <- ceiling(max(c(1,(covcomp[length(covcomp)] - covcomp[1] - window.size)/step.width)))
  }

  lower.lim <- rep(NA,n.steps)
  upper.lim <- rep(NA,n.steps)
  ci.lower.lim.l <- rep(NA,n.steps)
  ci.lower.lim.u <- rep(NA,n.steps)
  ci.upper.lim.l <- rep(NA,n.steps)
  ci.upper.lim.u <- rep(NA,n.steps)
  distribution.type <- rep(NA,n.steps)

  covariate.left <- rep(NA,n.steps)
  covariate.right <- rep(NA,n.steps)
  covariate.mean <- rep(NA,n.steps)
  covariate.median <- rep(NA,n.steps)
  covariate.n <- rep(NA,n.steps)

  if (!is.null(window.size) & !is.null(step.width)) {
    window.left <- covcomp[1]
    window.right <- window.left + window.size
    for (i in 1:n.steps) {
      is.in.interval <- covcomp >= window.left &  covcomp <= window.right
      if (sum(is.in.interval) >= n.min) {
        xxx <- xx[is.in.interval]
        res.reflim <- reflimR::reflim(xxx,n.min = n.min,apply.rounding = apply.rounding,lognormal = lognormal,plot.it = F)

        lower.lim[i] <- res.reflim$limits[1]
        upper.lim[i] <- res.reflim$limits[2]
        ci.lower.lim.l[i] <- res.reflim$confidence.int[1]
        ci.lower.lim.u[i] <- res.reflim$confidence.int[2]
        ci.upper.lim.l[i] <- res.reflim$confidence.int[3]
        ci.upper.lim.u[i] <- res.reflim$confidence.int[4]
        if (names(res.reflim$stats)[1] == "mean") {
          distribution.type[i] <- "normal"
        }else{
          distribution.type[i] <- "lognormal"
        }

        covals <- covcomp[is.in.interval]
        covariate.left[i] <- window.left
        covariate.right[i] <- window.right
        covariate.mean[i] <- mean(covals)
        covariate.median[i] <- median(covals)
        covariate.n[i] <- sum(is.in.interval)
      }else{
        covariate.left[i] <- window.left
        covariate.right[i] <- window.right
        covariate.n[i] <- sum(is.in.interval)
      }
      window.left <- window.left + step.width
      window.right <- window.right + step.width
    }
  }else{
    ind <- 1
    indl <- 1
    indr <- 2
    while (indr <= length(cov.unique)) {
      is.in.interval <- covcomp >= cov.unique[indl] &  covcomp < cov.unique[indr]
      if (sum(is.in.interval) >= n.min.window) {

        xxx <- xx[is.in.interval]
        res.reflim <- reflimR::reflim(xxx,n.min = n.min,apply.rounding = apply.rounding,lognormal = lognormal,plot.it = F)

        lower.lim[ind] <- res.reflim$limits[1]
        upper.lim[ind] <- res.reflim$limits[2]
        ci.lower.lim.l[ind] <- res.reflim$confidence.int[1]
        ci.lower.lim.u[ind] <- res.reflim$confidence.int[2]
        ci.upper.lim.l[ind] <- res.reflim$confidence.int[3]
        ci.upper.lim.u[ind] <- res.reflim$confidence.int[4]
        if (names(res.reflim$stats)[1] == "mean") {
          distribution.type[ind] <- "normal"
        }else{
          distribution.type[ind] <- "lognormal"
        }

        covals <- covcomp[is.in.interval]
        covariate.left[ind] <- min(covals)
        covariate.right[ind] <- max(covals)
        covariate.mean[ind] <- mean(covals)
        covariate.median[ind] <- median(covals)
        covariate.n[ind] <- sum(is.in.interval)

        indl <- indl + 1
        indr <- indr + 1
        ind <- ind + 1
      }else{
        indr <- indr + 1
      }
    }
  }

  res <- data.frame(lower.lim,upper.lim,ci.lower.lim.l,ci.lower.lim.u,ci.upper.lim.l,ci.upper.lim.u,distribution.type,covariate.left,covariate.right,covariate.mean,covariate.median,covariate.n)
  res <- res[!is.na(covariate.n),]

  return(res)
}

#' Computes the number of values in reference intervals based on a sliding window
#'
#' Computes the number of values in reference intervals based on a sliding window with a
#' fixed window size and a fixed step width by which the window is shifted.
#'
#' @param x Vector with lab values.
#' @param covariate The numeric covariate, e.g. age.
#' @param window.size Size of the window if a fixed window size should be used.
#' @param step.width The width by which the window should be shifted in each step if a fixed window size should be used.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{covariate.n}{The number of values in each window.}
#'   \item{covariate.left}{The left limits of the covariate windows.}
#'   \item{covariate.right}{The right limits of the covariate windows.}
#' }
#'
#' @export
count.n.per.window <- function(x,covariate,window.size,step.width){
  is.nona <- !is.na(x) & !is.na(covariate)
  covcomp <- covariate[is.nona]

  ord.cov <- order(covcomp)
  covcomp <- covcomp[ord.cov]

  n.steps <- ceiling(max(c(1,(covcomp[length(covcomp)] - covcomp[1] - window.size)/step.width)))

  covariate.left <- rep(NA,n.steps)
  covariate.right <- rep(NA,n.steps)
  covariate.n <- rep(NA,n.steps)

  window.left <- covcomp[1]
  window.right <- window.left + window.size
  for (i in 1:n.steps) {

    covariate.left[i] <- window.left
    covariate.right[i] <- window.right
    covariate.n[i] <- sum(covcomp >= window.left &  covcomp <= window.right)

    window.left <- window.left + step.width
    window.right <- window.right + step.width
  }

  return(data.frame(covariate.n,covariate.left,covariate.right))
}

#' Draws the reference limits computed by the function sliding.reflim()
#'
#' @param result.sliding.reflim A data frame returned by the function sliding.reflim().
#' @param use.mean Indicates whether the mean value of the covariate in the window or the median should be used.
#' @param xlim The usual graphics parameters.
#' @param ylim The usual graphics parameters.
#' @param xlab The usual graphics parameters.
#' @param ylab The usual graphics parameters.
#' @param col.low The colors for the lower reference limit. It should be specified by three values for red, green and blue.
#' @param col.upp The colors for the lower reference limit. It should be specified by three values for red, green and blue.
#' @param lwd The line width for the reference limits.
#' @param transparency The transparency of the confidence region.
#' @param draw.cis Indicates whether confidence bands should be drawn.
#' @param grid.col Color for the grid if a grid should be added.
#'
#' @importFrom grDevices rgb
#' @importFrom graphics points polygon grid
#'
#' @export
draw.sliding.reflims <- function(result.sliding.reflim, use.mean=T, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, col.low=c(0,0,1), col.upp=c(1,0,0), lwd=2, transparency=0.8, draw.cis=T, grid.col=NULL){

  rsr <- result.sliding.reflim

  cova <- rsr$covariate.mean
  if (!use.mean) {
    cova <- rsr$covariate.mean
  }else{
    cova <- rsr$covariate.median
  }

  ylim.mod <- ylim
  if (is.null(ylim)) {
    ylim.mod <- c(min(rsr$lower.lim),max(rsr$upper.lim))
    if (draw.cis) {
      ylim.mod <- c(min(rsr$ci.lower.lim.l),max(rsr$ci.upper.lim.u))
    }
  }

  plot(cova,rsr$lower.lim,xlim = xlim,ylim = ylim.mod,type = "l",lwd = lwd,col = rgb(col.low[1],col.low[2],col.low[3]),xlab = xlab,ylab = ylab,)
  if (!is.null(grid.col)) {
    grid(col = grid.col)
  }

  points(cova,rsr$upper.lim,type = "l",lwd = lwd,col = rgb(col.upp[1],col.upp[2],col.upp[3]))

  if (draw.cis) {
    collot <- rgb(col.low[1],col.low[2],col.low[3],1 - transparency)
    colupt <- rgb(col.upp[1],col.upp[2],col.upp[3],1 - transparency)
    polygon(c(cova,rev(cova)),c(rsr$ci.lower.lim.l,rev(rsr$ci.lower.lim.u)),col = collot,border = collot)
    polygon(c(cova,rev(cova)),c(rsr$ci.upper.lim.l,rev(rsr$ci.upper.lim.u)),col = colupt,border = colupt)
  }
}
