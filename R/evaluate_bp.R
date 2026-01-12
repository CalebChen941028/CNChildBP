#' @title Blood pressure evaluation for Chinese children and adolescents (ages 3-17)
#' @name evaluate_bp
#'
#' @description
#' Evaluate pediatric blood pressure according to the 2017 Chinese
#' sex-/age-/height-specific reference standards. The function includes
#' robust input parsing for non-standard age formats and performs height
#' rounding and percentile-based classification.
#'
#' @details
#' Column mapping behavior:
#' \itemize{
#'   \item If `sex_col/age_col/height_col/sbp_col/dbp_col` are not provided (all `NULL`), the function selects a default column-name mapping by `language`.
#'   \item `language = "chinese"` prefers: "性别"/"年龄"/"身高"/"收缩压"/"舒张压".
#'   \item `language = "english"` prefers: "sex"/"age"/"height"/"sbp"/"dbp".
#'   \item If the preferred set is not found and the user did not provide any mapping arguments, the function will try the other set as a fallback.
#'   \item If any `*_col` is explicitly provided, the function uses the provided mapping (no automatic fallback).
#' }
#'
#' Sex normalization:
#' \itemize{
#'   \item Common inputs such as "male"/"female" (or "m"/"f") are normalized to "男"/"女" before table lookup.
#' }
#'
#' Robust age parsing logic:
#' \itemize{
#'   \item Mixed years and months (e.g. "3 years 5 months") are parsed as \eqn{years + months/12}.
#'   \item Pure numeric values are interpreted as years if <= 18, otherwise as months (divided by 12).
#'   \item Parsed ages are floored to whole years for table lookup.
#' }
#'
#' Height matching:
#' \itemize{
#'   \item Heights are rounded using "round half up": \code{floor(x + 0.5)} before matching table intervals.
#' }
#'
#' Evaluation categories:
#' \itemize{
#'   \item Normal: SBP < P90 and DBP < P90
#'   \item High-normal: P90 ≤ BP < P95, or BP ≥ 120/80 mmHg (but below hypertension thresholds)
#'   \item Stage 1 Hypertension: P95 ≤ BP < P99 + 5 mmHg
#'   \item Stage 2 Hypertension: BP ≥ P99 + 5 mmHg
#' }
#'
#' Important note (diagnosis vs classification):
#' \itemize{
#'   \item This function performs single-record BP \emph{classification} based on percentile thresholds.
#'   \item Clinical \emph{diagnosis} of pediatric hypertension typically requires elevated BP (≥ P95)
#'     confirmed at multiple visits (e.g., re-check after 2--4 weeks and confirmed across 3 time points),
#'     as described in related hypertension guidelines.
#' }
#'
#' @param data A data.frame containing measurements to evaluate.
#' @param sex_col Character; name of the sex column.
#'   If `NULL` (default), the function uses a language-specific default mapping:
#'   Chinese: "性别"; English: "sex".
#' @param age_col Character; name of the age column.
#'   If `NULL` (default), the function uses a language-specific default mapping:
#'   Chinese: "年龄"; English: "age".
#'   Supported formats include numeric years, "10", "10.5", "10岁",
#'   "3岁5月", "3y5m", "75months", "75月", etc.
#' @param height_col Character; name of the height column in cm.
#'   If `NULL` (default), the function uses a language-specific default mapping:
#'   Chinese: "身高"; English: "height".
#' @param sbp_col Character; name of systolic BP column.
#'   If `NULL` (default), the function uses a language-specific default mapping:
#'   Chinese: "收缩压"; English: "sbp".
#' @param dbp_col Character; name of diastolic BP column.
#'   If `NULL` (default), the function uses a language-specific default mapping:
#'   Chinese: "舒张压"; English: "dbp".
#' @param language Character; one of "chinese" (default) or "english" for output labels.
#'
#' @return The input data.frame with an added `BP_Evaluation` column.
#'
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' # 1. Basic usage (Chinese columns; default language = "chinese")
#' df_cn <- data.frame(
#'   性别 = c("男", "女"),
#'   年龄 = c(10, 12),
#'   身高 = c(140, 150),
#'   收缩压 = c(110, 130),
#'   舒张压 = c(70, 85)
#' )
#' # evaluate_bp(df_cn)
#'
#' # 2. Basic usage (English columns; use language = "english")
#' df_en <- data.frame(
#'   sex = c("male", "female"),
#'   age = c(10, 12),
#'   height = c(140, 150),
#'   sbp = c(110, 130),
#'   dbp = c(70, 85)
#' )
#' # evaluate_bp(df_en, language = "english")
#'
#' # 3. Handle mixed age formats
#' df_complex <- data.frame(
#'   sex = c("male", "male", "female"),
#'   age = c("75months", "3y5m", "120"), # 6.25y, 3.41y, 10y
#'   height = c(120.5, 98, 140),
#'   sbp = c(110, 90, 130),
#'   dbp = c(70, 60, 85)
#' )
#' # evaluate_bp(df_complex, language = "english")
#'
#' # 4. Output labels can be switched via `language`
#' # evaluate_bp(df_cn, language = "english")
evaluate_bp <- function(data,
                        sex_col = NULL,
                        age_col = NULL,
                        height_col = NULL,
                        sbp_col = NULL,
                        dbp_col = NULL,
                        language = c("chinese", "english")) {

  # 0. match language argument
  language <- match.arg(language)

  # 0b. column name mapping defaults
  user_provided_mapping <- !is.null(sex_col) || !is.null(age_col) || !is.null(height_col) || !is.null(sbp_col) || !is.null(dbp_col)
  default_cols <- if (language == "chinese") {
    list(
      sex_col = "\u6027\u522b",
      age_col = "\u5e74\u9f84",
      height_col = "\u8eab\u9ad8",
      sbp_col = "\u6536\u7f29\u538b",
      dbp_col = "\u8212\u5f20\u538b"
    )
  } else {
    list(
      sex_col = "sex",
      age_col = "age",
      height_col = "height",
      sbp_col = "sbp",
      dbp_col = "dbp"
    )
  }

  if (is.null(sex_col)) sex_col <- default_cols$sex_col
  if (is.null(age_col)) age_col <- default_cols$age_col
  if (is.null(height_col)) height_col <- default_cols$height_col
  if (is.null(sbp_col)) sbp_col <- default_cols$sbp_col
  if (is.null(dbp_col)) dbp_col <- default_cols$dbp_col

  # define labels/messages (Chinese/English)
  labels <- if (language == "chinese") {
    list(
      normal = "\u6b63\u5e38",
      high_normal = "\u6b63\u5e38\u9ad8\u503c",
      stage1 = "1\u671f\u9ad8\u8840\u538b",
      stage2 = "2\u671f\u9ad8\u8840\u538b",
      missing = "\u7f3a\u5c11",
      out_of_range = "\u65e0\u6cd5\u8bc4\u4ef7(\u5e74\u9f84/\u8eab\u9ad8\u8d85\u51fa\u8303\u56f4)"
    )
  } else {
    list(
      normal = "Normal",
      high_normal = "High-normal",
      stage1 = "Stage 1",
      stage2 = "Stage 2",
      missing = "Missing",
      out_of_range = "N/A"
    )
  }

  messages <- if (language == "chinese") {
    list(
      missing_cols_prefix = "\u6570\u636e\u4e2d\u7f3a\u5c11\u4ee5\u4e0b\u5217: "
    )
  } else {
    list(
      missing_cols_prefix = "Missing required column(s): "
    )
  }

  # helper: normalize sex values to match standards (typically "\u7537"/"\u5973")
  normalize_sex <- function(x) {
    x_chr <- stringr::str_trim(as.character(x))
    x_lower <- tolower(x_chr)
    dplyr::case_when(
      is.na(x_chr) ~ NA_character_,
      x_lower %in% c("\u7537", "m", "male", "boy", "man") ~ "\u7537",
      x_lower %in% c("\u5973", "f", "female", "girl", "woman") ~ "\u5973",
      TRUE ~ x_chr
    )
  }

  # 1. check required columns exist
  required_cols <- c(sex_col, age_col, height_col, sbp_col, dbp_col)
  missing_cols <- setdiff(required_cols, names(data))

  # If user did not provide any mapping args, try the other language's defaults as a fallback.
  if (length(missing_cols) > 0 && !user_provided_mapping) {
    fallback_cols <- if (language == "chinese") {
      list(sex_col = "sex", age_col = "age", height_col = "height", sbp_col = "sbp", dbp_col = "dbp")
    } else {
      list(
        sex_col = "\u6027\u522b",
        age_col = "\u5e74\u9f84",
        height_col = "\u8eab\u9ad8",
        sbp_col = "\u6536\u7f29\u538b",
        dbp_col = "\u8212\u5f20\u538b"
      )
    }

    fallback_required <- c(fallback_cols$sex_col, fallback_cols$age_col, fallback_cols$height_col, fallback_cols$sbp_col, fallback_cols$dbp_col)
    if (all(fallback_required %in% names(data))) {
      sex_col <- fallback_cols$sex_col
      age_col <- fallback_cols$age_col
      height_col <- fallback_cols$height_col
      sbp_col <- fallback_cols$sbp_col
      dbp_col <- fallback_cols$dbp_col
      required_cols <- fallback_required
      missing_cols <- character(0)
    }
  }

  if (length(missing_cols) > 0) {
    stop(messages$missing_cols_prefix, paste(missing_cols, collapse = ", "))
  }

  # ========================================================================
  # internal helper: robust age parsing
  # ========================================================================
  parse_smart_age <- function(x) {
    # convert to character and remove whitespace
    x_str <- stringr::str_remove_all(as.character(x), "\\s+")

    year_pat <- "(?i)(\\d+(?:\\.\\d+)?)\\s*(\u5c81|y|yr|yrs|year|years)"
    month_pat <- "(?i)(\\d+(?:\\.\\d+)?)\\s*(\u6708|m|mo|mos|month|months)"

    extract_num <- function(s, pat) {
      m <- stringr::str_match(s, pat)
      if (is.null(dim(m)) || nrow(m) == 0 || is.na(m[1, 2])) return(NA_real_)
      as.numeric(m[1, 2])
    }

    sapply(x_str, function(s) {
      if (is.na(s) || s == "NA" || s == "") return(NA_real_)

      years <- extract_num(s, year_pat)
      months <- extract_num(s, month_pat)

      if (!is.na(years) && !is.na(months)) {
        return(years + months / 12)
      }

      if (!is.na(years)) {
        return(years)
      }

      if (!is.na(months)) {
        return(months / 12)
      }

      num_val <- suppressWarnings(as.numeric(s))
      if (!is.na(num_val)) {
        # If value > 18, it is likely months (e.g., 74 months). Otherwise treat as years.
        if (num_val > 18) return(num_val / 12)
        return(num_val)
      }

      NA_real_
    })
  }

  # 2. prepare and clean data
  # copy data and add temporary id to preserve row order
  work_data <- data
  work_data$..temp_id.. <- 1:nrow(work_data)

  work_data <- work_data %>%
    dplyr::rename(
      Sex_ = !!dplyr::sym(sex_col),
      Age_Raw_ = !!dplyr::sym(age_col),
      Height_ = !!dplyr::sym(height_col),
      SBP_ = !!dplyr::sym(sbp_col),
      DBP_ = !!dplyr::sym(dbp_col)
    ) %>%
    dplyr::mutate(
      # step 0: normalize sex to match reference table
      Sex_ = normalize_sex(Sex_),

      # step A: parse age
      Age_Parsed_ = parse_smart_age(Age_Raw_),

      # step B: standardize
      # age: floor to whole years for table lookup
      Age_Final_ = floor(Age_Parsed_),

      # height: round half up (floor(x + 0.5)) for table matching
      Height_Final_ = floor(Height_ + 0.5)
    )

  # 3. load reference standards
  standards <- CNChildBP::bp_standards

  # 4. matching logic (table lookup)
  matched <- work_data %>%
    # filter out rows with unparsed age/height (will be marked as out-of-range)
    dplyr::filter(!is.na(Age_Final_), !is.na(Height_Final_)) %>%
    # join by sex and age
    dplyr::left_join(standards, by = c("Sex_" = "Sex", "Age_Final_" = "Age")) %>%
    # filter rows within height interval
    dplyr::filter(Height_Final_ >= Height_Lower & Height_Final_ <= Height_Upper)

  # 5. evaluation (based on P90, P95, P99+5)
  results_calculated <- matched %>%
    dplyr::mutate(
      # --- systolic BP (SBP) evaluation ---
      sbp_status = dplyr::case_when(
        is.na(SBP_) ~ labels$missing,
        # 2期: >= P99 + 5 mmHg
        SBP_ >= (SBP_P99 + 5) ~ labels$stage2,
        # 1期: P95 ~ P99 + 5
        SBP_ >= SBP_P95 ~ labels$stage1,
        # 正常高值: P90 ~ P95 或 >= 120 (即使小于P90)
        SBP_ >= SBP_P90 | SBP_ >= 120 ~ labels$high_normal,
        TRUE ~ labels$normal
      ),

      # --- diastolic BP (DBP) evaluation ---
      dbp_status = dplyr::case_when(
        is.na(DBP_) ~ labels$missing,
        DBP_ >= (DBP_P99 + 5) ~ labels$stage2,
        DBP_ >= DBP_P95 ~ labels$stage1,
        DBP_ >= DBP_P90 | DBP_ >= 80 ~ labels$high_normal,
        TRUE ~ labels$normal
      ),

      # --- combined evaluation (take the more severe of SBP/DBP) ---
      BP_Evaluation = dplyr::case_when(
        sbp_status == labels$stage2 | dbp_status == labels$stage2 ~ labels$stage2,
        sbp_status == labels$stage1 | dbp_status == labels$stage1 ~ labels$stage1,
        sbp_status == labels$high_normal | dbp_status == labels$high_normal ~ labels$high_normal,
        sbp_status == labels$missing | dbp_status == labels$missing ~ labels$missing,
        TRUE ~ labels$normal
      )
    ) %>%
    dplyr::select(..temp_id.., BP_Evaluation)

  # 6. merge results back to original data
  final_result <- data
  final_result$..temp_id.. <- 1:nrow(final_result)

  final_result <- final_result %>%
    dplyr::left_join(results_calculated, by = "..temp_id..") %>%
    dplyr::select(-..temp_id..)

  # fill unmatched rows (age/height out of range or missing)
  final_result$BP_Evaluation[is.na(final_result$BP_Evaluation)] <- labels$out_of_range

  return(final_result)
}


## Declare globals to satisfy R CMD check NOTES about undefined globals
 
