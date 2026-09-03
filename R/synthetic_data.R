#' Generator for Age-Dependent Laboratory Measurements Using Mathematical Trends
#'
#' Generates synthetic age-dependent laboratory dataset across continuous age ranges
#' using normal, log-normal, or Box-Cox transformation distributions (BCCG, BCPE, BCT).
#' Supports adding pathological cases and censoring values below Limit of Detection (LOD).
#'
#' @param age Numeric. Maximum age range for simulation (in years).
#' @param age_steps Numeric. Step size for data generation in days (e.g., 365 for 1-year steps).
#' @param distribution Character. Type of distribution: \code{"NO"} (Normal), \code{"LOGNO"} (Log-Normal),
#'   \code{"BCCG"}, \code{"BCPE"}, or \code{"BCT"}.
#' @param n_ Integer. Total number of observations per age step.
#' @param name_value Character. Name of the analyte (default: \code{"Synthetic_Analyte"}).
#' @param formula_mu Character string or expression defining the trend for mu over age \code{x}.
#' @param formula_sigma Character string or expression defining the trend for sigma over age \code{x}.
#' @param formula_nu Character string or expression defining the trend for nu (for BCCG, BCPE, BCT).
#' @param formula_tau Character string or expression defining the trend for tau (for BCPE, BCT).
#' @param prop.path Numeric between 0 and 1. Proportion of pathological cases per age step (default: 0).
#' @param ill_factor Alias for \code{prop.path} for backward compatibility.
#' @param mu_factor_ill Numeric. Mean shift magnitude added to simulate pathological values.
#' @param lod Optional numeric. Limit of Detection threshold. Values below LOD will be flagged or processed.
#' @param seed Optional integer. Seed for random number generation (default: NULL).
#'
#' @return A data.frame with standard laboratory columns: \code{AGE_YEARS}, \code{AGE_DAYS},
#'   \code{VALUE}, \code{IS_BELOW_LOD}, \code{ID}, \code{SEX}, \code{STATION}, and \code{ANALYTE}.
#'
#' @examples
#' # Generate simple normal synthetic data across age range 0-5 years
#' df_synth <- generate_data(
#'   age = 5,
#'   age_steps = 365,
#'   distribution = "NO",
#'   n_ = 20,
#'   name_value = "ALT",
#'   seed = 42
#' )
#' head(df_synth)
#'
#' @export
generate_data <- function(age,
                          age_steps = 365,
                          distribution = "NO",
                          n_ = 100,
                          name_value = "Synthetic_Analyte",
                          formula_mu = "linear(x, 0, 10)",
                          formula_sigma = "linear(x, 0, 1)",
                          formula_nu = "linear(x, 0, 1)",
                          formula_tau = "linear(x, 0, 2)",
                          prop.path = 0,
                          ill_factor = NULL,
                          mu_factor_ill = 0,
                          lod = NULL,
                          seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Backward compatibility for the old parameter `ill_factor`
  if (!is.null(ill_factor)) {
    prop.path <- ill_factor
  }

  linear <- function(x, a, b) { x * a + b }
  expo <- function(x, a, b) { a * exp(x * b) }

  parse_formula <- function(f) {
    if (is.character(f)) parse(text = f) else f
  }

  parsed_mu <- parse_formula(formula_mu)
  parsed_sigma <- parse_formula(formula_sigma)
  parsed_nu <- parse_formula(formula_nu)
  parsed_tau <- parse_formula(formula_tau)

  step_years <- age_steps / 365
  age_sequence <- seq(0, age, by = step_years)

  eval_env <- list(linear = linear, expo = expo)
  eval_trend <- function(parsed_f, x_val) {
    eval_env$x <- x_val
    eval(parsed_f, envir = eval_env)
  }

  generated_list <- vector("list", length(age_sequence))

  for (idx in seq_along(age_sequence)) {
    i_age <- age_sequence[idx]

    current_mu <- eval_trend(parsed_mu, i_age)
    current_sigma <- eval_trend(parsed_sigma, i_age)

    # follow the logic defined in `lod.artificial.sample` to separate the diseased and healthy samples
    n_path <- round(n_ * prop.path)
    n_healthy <- n_ - n_path

    if (distribution == "NO") {
      healthy_vals <- if (n_healthy > 0) stats::rnorm(n = n_healthy, mean = current_mu, sd = current_sigma) else numeric(0)
      ill_vals <- if (n_path > 0) stats::rnorm(n = n_path, mean = current_mu + mu_factor_ill, sd = current_sigma) else numeric(0)
      vals <- c(healthy_vals, ill_vals)

    } else if (distribution == "LOGNO") {
      healthy_vals <- if (n_healthy > 0) stats::rlnorm(n = n_healthy, meanlog = current_mu, sdlog = current_sigma) else numeric(0)
      ill_vals <- if (n_path > 0) stats::rlnorm(n = n_path, meanlog = current_mu + log(1 + mu_factor_ill), sdlog = current_sigma) else numeric(0)
      vals <- c(healthy_vals, ill_vals)

    } else if (distribution == "BCCG") {
      if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
        stop("Package 'gamlss.dist' is required for BCCG distribution.", call. = FALSE)
      }
      current_nu <- eval_trend(parsed_nu, i_age)
      vals <- gamlss.dist::rBCCG(n = n_, mu = current_mu, sigma = current_sigma, nu = current_nu)

    } else if (distribution == "BCPE") {
      if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
        stop("Package 'gamlss.dist' is required for BCPE distribution.", call. = FALSE)
      }
      current_nu <- eval_trend(parsed_nu, i_age)
      current_tau <- eval_trend(parsed_tau, i_age)
      vals <- gamlss.dist::rBCPE(n = n_, mu = current_mu, sigma = current_sigma, nu = current_nu, tau = current_tau)

    } else if (distribution == "BCT") {
      if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
        stop("Package 'gamlss.dist' is required for BCT distribution.", call. = FALSE)
      }
      current_nu <- eval_trend(parsed_nu, i_age)
      current_tau <- eval_trend(parsed_tau, i_age)
      vals <- gamlss.dist::rBCT(n = n_, mu = current_mu, sigma = current_sigma, nu = current_nu, tau = current_tau)

    } else {
      stop("Unsupported distribution type. Choose from: NO, LOGNO, BCCG, BCPE, BCT.", call. = FALSE)
    }

    generated_list[[idx]] <- data.frame(
      age = i_age,
      value = vals
    )
  }

  generated_data <- do.call(rbind, generated_list)

  is_below_lod <- rep(FALSE, nrow(generated_data))
  if (!is.null(lod)) {
    is_below_lod <- generated_data$value < lod
  }

  res <- data.frame(
    AGE_YEARS = round(generated_data$age, 3),
    AGE_DAYS = round(generated_data$age * 365, 0),
    VALUE = round(generated_data$value, 3),
    IS_BELOW_LOD = is_below_lod,
    ID = seq_len(nrow(generated_data)),
    SEX = "NA",
    STATION = "Generator",
    ANALYTE = name_value,
    stringsAsFactors = FALSE
  )

  return(res)
}

#' Generator for Synthetic Laboratory Data from Provided Reference Intervals
#'
#' Generates synthetic observations using pre-defined 2.5\% and 97.5\% reference limits
#' (percentiles) assuming an underlying Gaussian distribution.
#'
#' @param reference_data A data.frame containing columns \code{age} (in days), \code{down} (lower limit),
#'   and \code{up} (upper limit).
#' @param n_ Integer. Number of observations to generate per age step.
#' @param text_name Character. Name of the analyte (default: \code{"Analyte"}).
#' @param seed Optional integer. Random seed for reproducibility (default: NULL).
#'
#' @return A data.frame with standard laboratory columns.
#'
#' @examples
#' ref_intervals <- data.frame(
#'   age = c(365, 730, 1095),
#'   down = c(10, 12, 11),
#'   up = c(40, 45, 42)
#' )
#' df_from_ri <- generate.data.from.ri(ref_intervals, n_ = 25, text_name = "AST", seed = 123)
#' head(df_from_ri)
#'
#' @export
generate.data.from.ri <- function(reference_data, n_ = 100, text_name = "Analyte", seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  req_cols <- c("age", "down", "up")
  if (all(req_cols %in% colnames(reference_data))) {
    ref_clean <- data.frame(
      age = as.integer(reference_data$age),
      down = as.numeric(reference_data$down),
      up = as.numeric(reference_data$up)
    )
  } else {
    if (ncol(reference_data) < 3) {
      stop("`reference_data` must have at least 3 columns (age, down, up).", call. = FALSE)
    }
    ref_clean <- data.frame(
      age = as.integer(reference_data[[1]]),
      down = as.numeric(reference_data[[2]]),
      up = as.numeric(reference_data[[3]])
    )
  }

  sigma <- (ref_clean$up - ref_clean$down) / (1.96 - (-1.96))
  mu <- ref_clean$up - 1.96 * sigma

  generated_list <- vector("list", nrow(ref_clean))

  for (i in seq_len(nrow(ref_clean))) {
    vals <- stats::rnorm(n = n_, mean = mu[i], sd = sigma[i])
    generated_list[[i]] <- data.frame(
      age = ref_clean$age[i],
      value = vals
    )
  }

  generated_data <- do.call(rbind, generated_list)
  generated_data <- generated_data[generated_data$value > 0, , drop = FALSE]

  res <- data.frame(
    AGE_YEARS = round(generated_data$age / 365, 3),
    AGE_DAYS = generated_data$age,
    VALUE = round(generated_data$value, 3),
    ID = seq_len(nrow(generated_data)),
    SEX = "NA",
    STATION = "Generator",
    ANALYTE = text_name,
    stringsAsFactors = FALSE
  )

  return(res)
}

#' Generate Synthetic Laboratory Data from Subgroups with Limits
#'
#' Generates synthetic data for multiple subgroups based on reference limits (lower and upper limits),
#' estimates underlying distribution parameters, and optionally plots histograms, densities, and boxplots.
#'
#' @param n Integer vector specifying sample sizes for each subgroup.
#' @param ll Numeric vector specifying the lower reference limits for each subgroup.
#' @param ul Numeric vector specifying the upper reference limits for each subgroup.
#' @param lognormal Logical, whether to generate lognormal distribution data instead of normal. Default is \code{FALSE}.
#' @param hist.bins Number of histogram bins if sample size > 200. Default is 50.
#' @param plot.it Logical, whether to generate a visualization plot. Default is \code{TRUE}.
#' @param add.boxplot Logical, whether to superimpose a horizontal boxplot on the top. Default is \code{TRUE}.
#' @param plot.legend Logical, whether to display a legend with reference intervals. Default is \code{TRUE}.
#' @param pos.legend Position of the legend. Default is \code{"topright"}.
#' @param main Title of the plot. Default is \code{""}.
#' @param xlab Label for x-axis. Default is \code{""}.
#' @param apply.rounding Logical, whether to round the generated data. Default is \code{TRUE}.
#' @param digits Integer, number of decimal digits to round. If \code{NULL}, auto-determined.
#'
#' @return A list containing `values` and `stats`.
#'
#' @examples
#' res <- synthetic.data(
#'   n = c(50, 200, 50),
#'   ll = c(10, 12, 15),
#'   ul = c(13, 16, 20),
#'   plot.it = FALSE
#' )
#' head(res$values)
#' print(res$stats)
#'
#' @export
synthetic.data <- function(n = c(100, 800, 100),
                           ll = c(10, 12, 15),
                           ul = c(13, 16, 20),
                           lognormal = FALSE, hist.bins = 50,
                           plot.it = TRUE, add.boxplot = TRUE,
                           plot.legend = TRUE, pos.legend = "topright",
                           main = "", xlab = "",
                           apply.rounding = TRUE, digits = NULL){
  if (length(n) != length(ll) || length(n) != length(ul) || length(ll) != length(ul)) {
    stop("The three vectors n, ll, and ul must have the same length.", call. = FALSE)
  }

  estimate_parameters <- function(lower, upper){
    m <- (lower + upper) / 2
    s <- (upper - lower) / 3.92
    return(c(m, s))
  }

  l <- length(n)
  val_list <- vector("list", l)
  res.tab <- data.frame(
    n = n,
    ll = ll,
    ul = ul,
    param1 = numeric(l),
    param2 = numeric(l)
  )

  if (lognormal) {
    colnames(res.tab)[4:5] <- c("meanlog", "sdlog")
  } else {
    colnames(res.tab)[4:5] <- c("mean", "sd")
  }

  for (i in seq_len(l)) {
    if (lognormal) {
      params <- estimate_parameters(log(ll[i]), log(ul[i]))
      res.tab[i, 4] <- params[1]
      res.tab[i, 5] <- params[2]
      val_list[[i]] <- stats::rlnorm(n[i], params[1], params[2])
    } else {
      params <- estimate_parameters(ll[i], ul[i])
      res.tab[i, 4] <- params[1]
      res.tab[i, 5] <- params[2]
      val_list[[i]] <- stats::rnorm(n[i], params[1], params[2])
    }
  }

  dat <- unlist(val_list, use.names = FALSE)

  if (apply.rounding) {
    if (is.null(digits)) {
      med_val <- stats::median(dat)
      digits <- if (med_val > 0) max(0, 2 - floor(log10(med_val))) else 2
    }
    dat <- round(dat, digits)
    if (lognormal) {
      res.tab[, 4:5] <- round(res.tab[, 4:5], 3)
    } else {
      res.tab[, 4:5] <- round(res.tab[, 4:5], digits + 1)
    }
  }

  if (length(dat) > 200) {
    breaks <- seq(0.9 * min(dat), 1.1 * max(dat), length.out = hist.bins)
  } else {
    breaks <- "Sturges"
  }

  if (plot.it) {
    col <- grDevices::rainbow(max(9, l))
    d <- stats::density(dat)
    y.max <- max(d$y) * 1.1
    if (add.boxplot) { y.max <- y.max * 1.4 }

    graphics::hist(dat, freq = FALSE,
                   breaks = breaks,
                   col = "white", border = "grey",
                   ylim = c(0, y.max), yaxt = "n",
                   main = main, xlab = xlab, ylab = "")
    graphics::box()
    graphics::lines(d, lty = 2)

    n.total <- sum(n)
    if (lognormal) {
      for (i in seq_len(l)) {
        graphics::curve(stats::dlnorm(x, res.tab[i, 4], res.tab[i, 5]) * (n[i] / n.total),
                        from = min(dat), to = max(dat), lwd = 2, col = col[i], add = TRUE)
      }
    } else {
      for (i in seq_len(l)) {
        graphics::curve(stats::dnorm(x, res.tab[i, 4], res.tab[i, 5]) * (n[i] / n.total),
                        from = min(dat), to = max(dat), lwd = 2, col = col[i], add = TRUE)
      }
    }

    if (add.boxplot) {
      graphics::boxplot(dat, horizontal = TRUE, at = y.max * 0.9, boxwex = y.max * 0.1, add = TRUE)
    }

    if (plot.legend) {
      legend_labels <- paste0(res.tab[, 2], "-", res.tab[, 3], " (", round(n / sum(n) * 100, 1), "%)")
      graphics::legend(pos.legend,
                       legend = legend_labels,
                       lwd = 2, col = col[seq_len(l)], cex = 0.8)
    }
  }

  return(list(values = dat, stats = res.tab))
}

utils::globalVariables(c("x"))