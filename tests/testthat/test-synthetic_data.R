library(testthat)

test_that("generate_data generiert gueltige Normalverteilungsdaten mit prop.path und LOD", {
  df <- generate_data(
    age = 2,
    age_steps = 365,
    distribution = "NO",
    n_ = 20,
    formula_mu = "linear(x, 0, 10)",
    formula_sigma = "linear(x, 0, 1)",
    prop.path = 0.1,
    mu_factor_ill = 3,
    lod = 9.5,
    seed = 42
  )

  # Struktur- und Typpruefung
  expect_s3_class(df, "data.frame")
  expect_true(all(c("AGE_YEARS", "AGE_DAYS", "VALUE", "IS_BELOW_LOD", "ID", "ANALYTE") %in% colnames(df)))
  expect_type(df$IS_BELOW_LOD, "logical")

  # Zeilenanzahl und Attribute pruefen (3 Altersgruppen * 20 = 60)
  expect_equal(nrow(df), 60)
  expect_true(any(df$IS_BELOW_LOD))
})

test_that("generate_data unterstuetzt LOGNO, Rueckwaertskompatibilitaet und wirft Fehler bei ungueltiger Verteilung", {
  # 1. LOGNO mit ill_factor Rueckwaertskompatibilitaet
  df_logno <- generate_data(
    age = 1,
    age_steps = 365,
    distribution = "LOGNO",
    n_ = 15,
    formula_mu = "linear(x, 0, 2)",
    formula_sigma = "linear(x, 0, 0.2)",
    ill_factor = 0.2,
    mu_factor_ill = 0.5,
    seed = 42
  )
  expect_s3_class(df_logno, "data.frame")
  expect_equal(nrow(df_logno), 30)

  # 2. Fehler bei unbekanntem Verteilungstyp
  expect_error(
    generate_data(age = 1, distribution = "UNKNOWN_DIST"),
    regexp = "Unsupported distribution type"
  )
})

test_that("generate_data unterstuetzt Box-Cox Verteilungen (BCCG, BCPE, BCT)", {
  skip_if_not_installed("gamlss.dist")

  # BCCG
  df_bccg <- generate_data(
    age = 1, age_steps = 365, distribution = "BCCG", n_ = 10,
    formula_mu = "linear(x, 0, 10)", formula_sigma = "linear(x, 0, 0.2)", formula_nu = "linear(x, 0, 1)",
    seed = 42
  )
  expect_equal(nrow(df_bccg), 20)

  # BCPE
  df_bcpe <- generate_data(
    age = 1, age_steps = 365, distribution = "BCPE", n_ = 10,
    formula_mu = "linear(x, 0, 10)", formula_sigma = "linear(x, 0, 0.2)",
    formula_nu = "linear(x, 0, 1)", formula_tau = "linear(x, 0, 2)",
    seed = 42
  )
  expect_equal(nrow(df_bcpe), 20)

  # BCT
  df_bct <- generate_data(
    age = 1, age_steps = 365, distribution = "BCT", n_ = 10,
    formula_mu = "linear(x, 0, 10)", formula_sigma = "linear(x, 0, 0.2)",
    formula_nu = "linear(x, 0, 1)", formula_tau = "linear(x, 0, 2)",
    seed = 42
  )
  expect_equal(nrow(df_bct), 20)
})

test_that("generate.data.from.ri funktioniert mit definierten Referenzgrenzen und Spalten-Fallback", {
  # 1. Standard-Spalten
  ref_df <- data.frame(
    age = c(365, 730),
    down = c(2, 2),
    up = c(6, 6)
  )
  df <- generate.data.from.ri(ref_df, n_ = 10, text_name = "AST", seed = 123)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 20)
  expect_equal(unique(df$ANALYTE), "AST")

  # 2. Spalten ohne exakte Namen (Positions-Fallback)
  ref_anon <- data.frame(c(365, 730), c(2, 2), c(6, 6))
  df_anon <- generate.data.from.ri(ref_anon, n_ = 5, seed = 123)
  expect_equal(nrow(df_anon), 10)

  # 3. Fehler bei zu wenigen Spalten
  expect_error(
    generate.data.from.ri(data.frame(c(365), c(2))),
    regexp = "must have at least 3 columns"
  )
})

test_that("synthetic.data berechnet korrekte Subgruppenparameter und faengt Fehler ab", {
  # 1. Standard-Normalverteilung testen (ohne Plot)
  res <- synthetic.data(
    n = c(50, 50),
    ll = c(10, 15),
    ul = c(20, 25),
    plot.it = FALSE
  )

  expect_type(res, "list")
  expect_named(res, c("values", "stats"))
  expect_length(res$values, 100)
  expect_equal(nrow(res$stats), 2)
  expect_true(all(c("mean", "sd") %in% colnames(res$stats)))

  # 2. Lognormal-Verteilung testen
  res_log <- synthetic.data(
    n = c(30),
    ll = c(5),
    ul = c(15),
    lognormal = TRUE,
    plot.it = FALSE
  )
  expect_true(all(c("meanlog", "sdlog") %in% colnames(res_log$stats)))

  # 3. Fehlerbehandlung bei ungleicher Vektorlaenge validieren
  expect_error(
    synthetic.data(n = c(100, 100), ll = c(10), ul = c(20, 30), plot.it = FALSE),
    regexp = "must have the same length"
  )
})