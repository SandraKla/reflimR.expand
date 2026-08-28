library(testthat)

test_that("make_data generiert gueltige Normalverteilungsdaten mit prop.path und LOD", {
  df <- make_data(
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

test_that("generate_data_from_ri funktioniert mit definierten Referenzgrenzen", {
  # Referenztabelle mit 2 Altersstufen definieren
  ref_df <- data.frame(
    age = c(365, 730),
    down = c(2, 2),
    up = c(6, 6)
  )
  df <- generate_data_from_ri(ref_df, n_ = 10, text_name = "AST", seed = 123)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 20)
  expect_equal(unique(df$ANALYTE), "AST")
})

test_that("synthetic_data berechnet korrekte Subgruppenparameter und faengt Fehler ab", {
  # 1. Standard-Normalverteilung testen (ohne Plot)
  res <- synthetic_data(
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
  res_log <- synthetic_data(
    n = c(30),
    ll = c(5),
    ul = c(15),
    lognormal = TRUE,
    plot.it = FALSE
  )
  expect_true(all(c("meanlog", "sdlog") %in% colnames(res_log$stats)))

  # 3. Fehlerbehandlung bei ungleicher Vektorlaenge validieren
  expect_error(
    synthetic_data(n = c(100, 100), ll = c(10), ul = c(20, 30), plot.it = FALSE),
    regexp = "must have the same length"
  )
})