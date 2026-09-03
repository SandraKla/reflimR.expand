#' Gaussian mixture modelling for laboratory data
#'
#' Uses Gaussian finite mixture models to identify different subpopulations
#' in laboratory measurement data and estimate component-specific reference
#' intervals.
#'
#' @param x Numeric vector containing positive laboratory measurements.
#'   At least 200 observations are recommended.
#' @param lognormal Logical. If `TRUE`, the model is fitted to logarithmically
#'   transformed measurements.
#' @param remove.extremes Logical. If `TRUE`, very large values are removed
#'   before fitting the model.
#' @param targets Optional numeric vector containing two target limits to add
#'   to the plot.
#' @param plot.it Logical. If `TRUE`, diagnostic plots are generated.
#' @param add.boxplot Logical. If `TRUE`, a boxplot is added to the histogram.
#' @param plot.legend Logical. If `TRUE`, a legend is added.
#' @param pos.legend Position passed to [graphics::legend()].
#' @param plot.bic Logical. If `TRUE`, the BIC values are plotted.
#' @param xlim Optional numeric vector defining the x-axis limits.
#' @param ylim Optional numeric vector defining the y-axis limits.
#' @param main Main title of the plot.
#' @param xlab Label of the x-axis.
#' @param hist.bins Number of histogram break points.
#' @param model Optional `mclust` model name, such as `"E"` or `"V"`.
#' @param n.cluster Optional number or vector of numbers of mixture components.
#' @param apply.rounding Logical. If `TRUE`, reference limits are rounded.
#' @param digits Number of decimal places used for rounding.
#'
#' @return A list containing:
#' \describe{
#'   \item{n.cluster}{Selected number of mixture components.}
#'   \item{stats}{Estimated limits, proportions, means and standard deviations.}
#'   \item{y.max}{Maximum y-axis value used for plotting, or `NULL`.}
#'   \item{BIC}{BIC values returned by [mclust::Mclust()].}
#' }
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' values <- c(
#'   rnorm(120, mean = 20, sd = 2),
#'   rnorm(120, mean = 40, sd = 3)
#' )
#'
#' result <- lab.mclust(
#'   values,
#'   lognormal = FALSE,
#'   remove.extremes = FALSE,
#'   plot.it = FALSE,
#'   model = "V",
#'   n.cluster = 2
#' )
#'
#' result$stats
#' }
#'
#' @import mclust
#'
#' @export
lab.mclust <- function(
  x,
  lognormal = FALSE,
  remove.extremes = FALSE,
  targets = NULL,
  plot.it = TRUE,
  add.boxplot = TRUE,
  plot.legend = TRUE,
  pos.legend = "topright",
  plot.bic = FALSE,
  xlim = NULL,
  ylim = NULL,
  main = "",
  xlab = "",
  hist.bins = 50,
  model = NULL,
  n.cluster = NULL,
  apply.rounding = TRUE,
  digits = NULL) {

  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  x <- x[is.finite(x) & x > 0]
  original.length <- length(x)

  if (original.length < 2L) {
    stop("x must contain at least two positive finite values.")
  }

  if (original.length < 200L) {
    warning(
      paste(
        "The data has",
        original.length,
        "numeric elements, where 200 are recommended."
      )
    )
  }

  if (remove.extremes) {
    upper.bound <- stats::median(x) + 6 * stats::IQR(x)
    x <- x[x < upper.bound]
  }

  if (length(x) < 2L) {
    stop("Too few values remain after removing extremes.")
  }

  xx <- if (lognormal) {
    log(x)
  } else {
    x
  }

  if (length(xx) < 200L && original.length >= 200L) {
    warning(
      paste(
        "After removal of extremes, the data has",
        length(xx),
        "numeric elements, where 200 are recommended."
      )
    )
  }

  if (!is.null(targets) && length(targets) < 2L) {
    stop("targets must contain at least two values.")
  }

  if (is.null(model)) {
    mc <- mclust::Mclust(
      xx,
      G = n.cluster
    )
  } else {
    mc <- mclust::Mclust(
      xx,
      modelNames = model,
      G = n.cluster
    )
  }

  component.variance <- mc$parameters$variance$sigmasq

  if (length(component.variance) == 1L) {
    component.variance <- rep(component.variance, mc$G)
  } else {
    component.variance <- rep_len(component.variance, mc$G)
  }

  component.sd <- sqrt(component.variance)

  res.tab <- data.frame(
    ll = rep(NA_real_, mc$G),
    ul = rep(NA_real_, mc$G),
    percent = round(mc$parameters$pro * 100, 1)
  )

  if (lognormal) {
    res.tab$meanlog <- round(mc$parameters$mean, 3)
    res.tab$sdlog <- round(component.sd, 3)

    for (i in seq_len(mc$G)) {
      res.tab$ll[i] <- stats::qlnorm(
        0.025,
        meanlog = mc$parameters$mean[i],
        sdlog = component.sd[i]
      )

      res.tab$ul[i] <- stats::qlnorm(
        0.975,
        meanlog = mc$parameters$mean[i],
        sdlog = component.sd[i]
      )
    }
  } else {
    res.tab$mean <- round(mc$parameters$mean, 3)
    res.tab$sd <- round(component.sd, 3)

    for (i in seq_len(mc$G)) {
      res.tab$ll[i] <- stats::qnorm(
        0.025,
        mean = mc$parameters$mean[i],
        sd = component.sd[i]
      )

      res.tab$ul[i] <- stats::qnorm(
        0.975,
        mean = mc$parameters$mean[i],
        sd = component.sd[i]
      )
    }
  }

  # Calculate the number of decimal places to retain based on specification and safety checks
  if (is.null(digits)) {
    med_val <- stats::median(x)
    digits <- if (med_val > 0) max(0, 2 - floor(log10(med_val))) else 2
  }

  if (apply.rounding) {
    res.tab$ll <- round(res.tab$ll, digits)
    res.tab$ul <- round(res.tab$ul, digits)
  }

  y.max <- NULL

  if (plot.it) {
    component.colors <- grDevices::rainbow(max(9, mc$G))
    data.density <- stats::density(x)

    y.max <- max(data.density$y) * 1.1

    if (add.boxplot) {
      y.max <- y.max * 1.4
    }

    if (length(x) > 200L) {
      breaks <- seq(
        0.9 * min(x),
        1.1 * max(x),
        length.out = hist.bins
      )
    } else {
      breaks <- "Sturges"
    }

    if (is.null(xlim)) {
      xlim <- range(x)
    }

    if (is.null(ylim)) {
      ylim <- c(0, y.max * 1.1)
    }

    graphics::hist(
      x,
      freq = FALSE,
      breaks = breaks,
      col = "white",
      border = "grey",
      xlim = xlim,
      ylim = ylim,
      yaxt = "n",
      main = main,
      xlab = xlab,
      ylab = ""
    )

    graphics::box()
    graphics::lines(data.density, lty = 2)

    if (add.boxplot) {
      graphics::boxplot(
        x,
        horizontal = TRUE,
        at = y.max * 0.9,
        boxwex = y.max * 0.1,
        add = TRUE
      )
    }

    if (lognormal) {
      for (i in seq_len(nrow(res.tab))) {
        graphics::curve(
          stats::dlnorm(
            x,
            meanlog = mc$parameters$mean[i],
            sdlog = component.sd[i]
          ) * mc$parameters$pro[i],
          from = min(x),
          to = max(x),
          lwd = 2,
          col = component.colors[i],
          add = TRUE
        )
      }
    } else {
      for (i in seq_len(nrow(res.tab))) {
        graphics::curve(
          stats::dnorm(
            x,
            mean = mc$parameters$mean[i],
            sd = component.sd[i]
          ) * mc$parameters$pro[i],
          from = min(x),
          to = max(x),
          lwd = 2,
          col = component.colors[i],
          add = TRUE
        )
      }
    }

    if (!is.null(targets)) {
      graphics::lines(
        rep(targets[1], 2),
        c(0, y.max * 0.8),
        lty = 2
      )

      graphics::lines(
        rep(targets[2], 2),
        c(0, y.max * 0.8),
        lty = 2
      )

      graphics::text(
        targets[1:2],
        rep(y.max * 0.85, 2),
        targets[1:2]
      )
    }

    if (plot.legend) {
      graphics::legend(
        pos.legend,
        paste0(
          round(res.tab$ll, digits),
          "-",
          round(res.tab$ul, digits),
          " (",
          res.tab$percent,
          "%)"
        ),
        lwd = 2,
        col = component.colors[seq_len(nrow(res.tab))],
        cex = 0.8,
        bty = "n"
      )
    }

    if (plot.bic) {
      graphics::plot(mc$BIC)
    }
  }

  selected.clusters <- if (is.null(n.cluster)) {
    mc$G
  } else {
    paste(mc$G, "from", paste(n.cluster, collapse = ", "))
  }

  list(
    n.cluster = noquote(selected.clusters),
    stats = res.tab,
    y.max = y.max,
    BIC = mc$BIC
  )
}

utils::globalVariables(c("x"))