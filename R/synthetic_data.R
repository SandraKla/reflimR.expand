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
#' @param n_ Integer. Number of healthy observations per age step.
#' @param name_value Character. Name of the analyte (default: \code{"Synthetic_Analyte"}).
#' @param formula_mu Character string or expression defining the trend for mu over age \code{x}.
#' @param formula_sigma Character string or expression defining the trend for sigma over age \code{x}.
#' @param formula_nu Character string or expression defining the trend for nu (for BCCG, BCPE, BCT).
#' @param formula_tau Character string or expression defining the trend for tau (for BCPE, BCT).
#' @param ill_factor Numeric between 0 and 1. Proportion of pathological cases to add per age step.
#' @param mu_factor_ill Numeric. Mean shift magnitude added to simulate pathological values.
#' @param lod Optional numeric. Limit of Detection threshold. Values below LOD will be flagged or processed.
#' @param seed Optional integer. Seed for random number generation (default: NULL).
#'
#' @return A data.frame with standard laboratory columns: \code{AGE_YEARS}, \code{AGE_DAYS},
#'   \code{VALUE}, \code{IS_BELOW_LOD}, \code{ID}, \code{SEX}, \code{STATION}, and \code{ANALYTE}.
#' @export
#'
#' @examples
#' df <- make_data(
#'   age = 10,
#'   age_steps = 365,
#'   distribution = "NO",
#'   n_ = 50,
#'   formula_mu = "linear(x, a = 0.5, b = 10)",
#'   formula_sigma = "linear(x, a = 0, b = 2)",
#'   ill_factor = 0.1,
#'   mu_factor_ill = 5,
#'   lod = 8.5
#' )
#' head(df)
make_data <- function(age,
                      age_steps = 365,
                      distribution = "NO",
                      n_ = 100,
                      name_value = "Synthetic_Analyte",
                      formula_mu = "linear(x, 0, 10)",
                      formula_sigma = "linear(x, 0, 1)",
                      formula_nu = "linear(x, 0, 1)",
                      formula_tau = "linear(x, 0, 2)",
                      ill_factor = 0,
                      mu_factor_ill = 0,
                      lod = NULL,
                      seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
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

  eval_trend <- function(parsed_f, x_val) {
    eval(parsed_f, envir = list(x = x_val, linear = linear, expo = expo))
  }

  generated_list <- vector("list", length(age_sequence))

  for (idx in seq_along(age_sequence)) {
    i_age <- age_sequence[idx]

    current_mu <- eval_trend(parsed_mu, i_age)
    current_sigma <- eval_trend(parsed_sigma, i_age)

    if (distribution == "NO") {
      healthy_vals <- stats::rnorm(n = n_, mean = current_mu, sd = current_sigma)
      n_ill <- round(n_ * ill_factor)
      ill_vals <- if (n_ill > 0) stats::rnorm(n = n_ill, mean = current_mu + mu_factor_ill, sd = current_sigma) else numeric(0)
      vals <- c(healthy_vals, ill_vals)

    } else if (distribution == "LOGNO") {
      healthy_vals <- stats::rlnorm(n = n_, meanlog = current_mu, sdlog = current_sigma)
      n_ill <- round(n_ * ill_factor)
      ill_vals <- if (n_ill > 0) stats::rlnorm(n = n_ill, meanlog = current_mu + log(1 + mu_factor_ill), sdlog = current_sigma) else numeric(0)
      vals <- c(healthy_vals, ill_vals)

    } else if (distribution == "BCCG") {
      if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
        stop("Package 'gamlss.dist' is required for BCCG distribution.")
      }
      current_nu <- eval_trend(parsed_nu, i_age)
      vals <- gamlss.dist::rBCCG(n = n_, mu = current_mu, sigma = current_sigma, nu = current_nu)

    } else if (distribution == "BCPE") {
      if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
        stop("Package 'gamlss.dist' is required for BCPE distribution.")
      }
      current_nu <- eval_trend(parsed_nu, i_age)
      current_tau <- eval_trend(parsed_tau, i_age)
      vals <- gamlss.dist::rBCPE(n = n_, mu = current_mu, sigma = current_sigma, nu = current_nu, tau = current_tau)

    } else if (distribution == "BCT") {
      if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
        stop("Package 'gamlss.dist' is required for BCT distribution.")
      }
      current_nu <- eval_trend(parsed_nu, i_age)
      current_tau <- eval_trend(parsed_tau, i_age)
      vals <- gamlss.dist::rBCT(n = n_, mu = current_mu, sigma = current_sigma, nu = current_nu, tau = current_tau)

    } else {
      stop("Unsupported distribution type. Choose from: NO, LOGNO, BCCG, BCPE, BCT.")
    }

    generated_list[[idx]] <- data.frame(
      age = i_age,
      value = vals
    )
  }

  generated_data <- do.call(rbind, generated_list)

  # Check Limit of Detection (LOD)
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
#' (percentiles) assuming an underlying Gaussian distribution. Ported from AdRI_Generator.
#'
#' @param reference_data A data.frame containing columns \code{age} (in days), \code{down} (lower limit),
#'   and \code{up} (upper limit).
#' @param n_ Integer. Number of observations to generate per age step.
#' @param text_name Character. Name of the analyte (default: \code{"Analyte"}).
#' @param seed Optional integer. Random seed for reproducibility (default: NULL).
#'
#' @return A data.frame with standard laboratory columns: \code{AGE_YEARS}, \code{AGE_DAYS},
#'   \code{VALUE}, \code{ID}, \code{SEX}, \code{STATION}, and \code{ANALYTE}.
#' @export
#'
#' @examples
#' ref_df <- data.frame(
#'   age = c(365, 730, 1095),
#'   down = c(2, 2, 3),
#'   up = c(6, 6, 6)
#' )
#' df <- generate_data_from_ri(ref_df, n_ = 20, text_name = "ALT", seed = 42)
#' head(df)
generate_data_from_ri <- function(reference_data, n_ = 100, text_name = "Analyte", seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  colnames(reference_data)[1:3] <- c("age", "down", "up")
  ref_clean <- data.frame(
    age = as.integer(reference_data$age),
    down = as.numeric(reference_data$down),
    up = as.numeric(reference_data$up)
  )

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
  generated_data <- generated_data[generated_data$value > 0, ]

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