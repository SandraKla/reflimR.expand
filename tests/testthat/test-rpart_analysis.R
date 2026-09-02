library(testthat)
library(reflimR.expand)

test_that("generate_rpart erzeugt ein gueltiges rpart-Objekt und fuehrt Splits aus", {
  set.seed(42)
  # Beispieldaten mit deutlichem Alters- und Geschlechtseffekt erzeugen
  df <- data.frame(
    Age = c(rep(20, 50), rep(60, 50)),
    Sex = rep(c("m", "f"), 50),
    ALT = c(rnorm(50, mean = 20, sd = 2), rnorm(50, mean = 40, sd = 2))
  )

  # Grafik-Ausgabe unterdruecken, um keine Rplots.pdf im Testordner zu erzeugen
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- generate_rpart(
    df = df,
    analyte = "ALT",
    tree_cp = 0.01,
    tree_minsplit = 10
  )

  # Rueckgabetyp und Struktur pruefen
  expect_s3_class(res, "rpart")
  expect_true(nrow(res$frame) > 1L)
  expect_equal(res$method, "anova")
})

test_that("generate_rpart funktioniert ohne Fehler, wenn keine Splits entstehen (Stump)", {
  set.seed(42)
  # Homogene Daten ohne signifikanten Trend
  df_flat <- data.frame(
    Age = runif(40, min = 20, max = 30),
    Sex = rep(c("m", "f"), 20),
    ALT = rep(25, 40)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Hoher cp-Wert erzwingt einen Baum ohne Splits
  res_stump <- generate_rpart(
    df = df_flat,
    analyte = "ALT",
    tree_cp = 0.99,
    tree_minsplit = 50
  )

  expect_s3_class(res_stump, "rpart")
  expect_equal(nrow(res_stump$frame), 1L)
})

test_that("generate_rpart faengt fehlerhafte Eingaben sauber ab", {
  # 1. Kein data.frame uebergeben
  expect_error(
    generate_rpart(list(Age = 1, Sex = "m", ALT = 20)),
    regexp = "'df' muss ein data.frame sein"
  )

  # 2. Fehlende Pflichtspalte ('Sex' fehlt)
  df_missing_sex <- data.frame(Age = 20:30, ALT = 10:20)
  expect_error(
    generate_rpart(df_missing_sex, analyte = "ALT"),
    regexp = "Fehlende Spalten im Datensatz: Sex"
  )

  # 3. Angegebener Analyt existiert nicht in den Spalten
  df_valid <- data.frame(Age = 20:30, Sex = rep("m", 11), ALT = 10:20)
  expect_error(
    generate_rpart(df_valid, analyte = "AST"),
    regexp = "Fehlende Spalten im Datensatz: AST"
  )
})