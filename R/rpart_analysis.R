#' Generate and Plot Regression Tree for Patient Partitioning
#'
#' Fits an ANOVA regression tree using \code{rpart} to partition patients by Age and Sex,
#' and visualizes the resulting tree via \code{rpart.plot}.
#'
#' @param df A data.frame containing analyte values, \code{Age}, and \code{Sex}.
#' @param analyte Character string specifying the target analyte column name (default is \code{"ALT"}).
#' @param tree_cp Numeric. Complexity parameter for rpart pruning (default is \code{0.01}).
#' @param tree_minsplit Integer. Minimum number of observations in a node to attempt a split (default is \code{30}).
#'
#' @return An \code{rpart} object (returned invisibly).
#'
#' @import rpart
#' @importFrom rpart.plot rpart.plot
#' @importFrom stats as.formula
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' sample_data <- data.frame(
#'   Age = sample(18:80, 200, replace = TRUE),
#'   Sex = sample(c("m", "f"), 200, replace = TRUE),
#'   ALT = rnorm(200, mean = 25, sd = 5)
#' )
#' fit <- generate.rpart(sample_data, analyte = "ALT", tree_cp = 0.05, tree_minsplit = 20)
#' }
#'
#' @export
generate.rpart <- function(df, analyte = "ALT", tree_cp = 0.01, tree_minsplit = 30) {
  # Eingabepruefung
  if (!is.data.frame(df)) {
    stop("'df' muss ein data.frame sein.")
  }
  required_cols <- c("Age", "Sex", analyte)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Fehlende Spalten im Datensatz:", paste(missing_cols, collapse = ", ")))
  }

  # Formel dynamisch aufbauen
  formula_obj <- stats::as.formula(paste(analyte, "~ Age + Sex"))

  # Regressionsbaum anpassen
  fit_tree <- rpart::rpart(
    formula_obj,
    data = df,
    method = "anova",
    control = rpart::rpart.control(cp = tree_cp, minsplit = tree_minsplit)
  )

  # Beschriftung 'yes/no' nur anzeigen, wenn Splits vorhanden sind
  yesno_val <- if (nrow(fit_tree$frame) > 1L) 2 else 0

  # Visualisierung ueber rpart.plot
  rpart.plot::rpart.plot(
    fit_tree,
    box.palette = "RdBu",
    roundint = FALSE,
    yesno = yesno_val,
    main = paste0("Regression tree: Patient Partitioning by Age & Sex (", analyte, ")"),
    node.fun = function(x, labs, digits, varlen) {
      paste0(round(100 * x$frame$n / x$frame$n[1]), "%")
    }
  )

  invisible(fit_tree)
}