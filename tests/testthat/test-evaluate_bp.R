test_that("evaluate_bp returns expected labels in Chinese and English", {
  standards <- CNChildBP::bp_standards
  expect_true(is.data.frame(standards))
  expect_true(nrow(standards) > 0)

  row <- standards[1, , drop = FALSE]
  height_mid <- floor(((row$Height_Lower + row$Height_Upper) / 2) + 0.5)

  df_cn <- data.frame(
    性别 = row$Sex,
    年龄 = row$Age,
    身高 = height_mid,
    收缩压 = row$SBP_P90 - 1,
    舒张压 = row$DBP_P90 - 1
  )

  res_cn <- evaluate_bp(df_cn, language = "chinese")
  expect_true("BP_Evaluation" %in% names(res_cn))
  expect_equal(res_cn$BP_Evaluation[[1]], "正常")

  df_en <- data.frame(
    sex = if (identical(row$Sex, "男")) "male" else "female",
    age = row$Age,
    height = height_mid,
    sbp = row$SBP_P90 - 1,
    dbp = row$DBP_P90 - 1
  )

  res_en <- evaluate_bp(df_en, language = "english")
  expect_equal(res_en$BP_Evaluation[[1]], "Normal")
})

test_that("age parsing supports multiple formats and floors to whole years", {
  standards <- CNChildBP::bp_standards
  row <- standards[1, , drop = FALSE]
  height_mid <- floor(((row$Height_Lower + row$Height_Upper) / 2) + 0.5)

  # Use a fractional age that should floor back to row$Age
  df <- data.frame(
    性别 = row$Sex,
    年龄 = paste0(row$Age, ".9"),
    身高 = height_mid,
    收缩压 = row$SBP_P90 - 1,
    舒张压 = row$DBP_P90 - 1
  )

  res <- evaluate_bp(df)
  expect_equal(res$BP_Evaluation[[1]], "正常")
})

test_that("height is rounded using round-half-up before matching", {
  standards <- CNChildBP::bp_standards
  row <- standards[1, , drop = FALSE]

  # Pick a height that should round up (e.g., 140.5 -> 141)
  rounded <- floor(((row$Height_Lower + row$Height_Upper) / 2) + 0.5)
  height_input <- rounded - 0.5

  df <- data.frame(
    性别 = row$Sex,
    年龄 = row$Age,
    身高 = height_input,
    收缩压 = row$SBP_P90 - 1,
    舒张压 = row$DBP_P90 - 1
  )

  res <- evaluate_bp(df)
  expect_equal(res$BP_Evaluation[[1]], "正常")
})

test_that("120/80 cap rule produces High-normal even below P90", {
  standards <- CNChildBP::bp_standards

  idx <- which(standards$SBP_P90 > 120 & standards$DBP_P90 > 80)
  skip_if(length(idx) == 0, "No standards row with P90 above 120/80 in dataset")

  row <- standards[idx[1], , drop = FALSE]
  height_mid <- floor(((row$Height_Lower + row$Height_Upper) / 2) + 0.5)

  # Force SBP/DBP to 120/80 which should trigger High-normal by cap rule,
  # even though it's below P90 for this row.
  df <- data.frame(
    sex = if (identical(row$Sex, "男")) "male" else "female",
    age = row$Age,
    height = height_mid,
    sbp = 120,
    dbp = 80
  )

  res <- evaluate_bp(df, language = "english")
  expect_equal(res$BP_Evaluation[[1]], "High-normal")
})

test_that("missing BP yields Missing label", {
  standards <- CNChildBP::bp_standards
  row <- standards[1, , drop = FALSE]
  height_mid <- floor(((row$Height_Lower + row$Height_Upper) / 2) + 0.5)

  df <- data.frame(
    sex = if (identical(row$Sex, "男")) "male" else "female",
    age = row$Age,
    height = height_mid,
    sbp = NA_real_,
    dbp = row$DBP_P90 - 1
  )

  res <- evaluate_bp(df, language = "english")
  expect_equal(res$BP_Evaluation[[1]], "Missing")
})

test_that("out of range age yields out_of_range label", {
  df <- data.frame(
    sex = "male",
    age = 2,
    height = 100,
    sbp = 100,
    dbp = 60
  )

  res <- evaluate_bp(df, language = "english")
  expect_equal(res$BP_Evaluation[[1]], "N/A")
})
