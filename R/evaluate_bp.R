#' 评价中国3-17岁儿童青少年血压 (基于2017年国标)
#'
#' @description
#' 本函数依据 2017 年发布的《中国3～17岁儿童性别、年龄别和身高别血压参照标准》，对儿童青少年的血压状况进行自动评价。
#' 函数内置了智能解析逻辑，能够处理各种非标准化的年龄格式（如"3岁5月"），并严格按照医学标准进行身高查表和血压分级。
#'
#' @references
#' 范晖, 闫银坤, 米杰. 中国3～17岁儿童性别、年龄别和身高别血压参照标准修订\[J\]. 中华高血压杂志, 2017, 25(5): 428-435. DOI: 10.16439/j.cnki.1673-7245.2017.05.009
#'
#' @details
#' **1. 智能年龄解析逻辑 (Robust Age Parsing)**
#' 为了提高数据处理的鲁棒性，函数按以下优先级处理 `age_col` 列：
#' \itemize{
#'   \item **"岁"与"月"混用**：如 "3岁5月"，自动转换为 \eqn{3 + 5/12} 岁。
#'   \item **纯"岁"或纯"月"**：自动提取数字部分并识别单位。
#'   \item **纯数字智能推断**：
#'     \itemize{
#'       \item 数值 > 18：视为"月龄" (因为17岁是量表上限)，自动除以12转换为岁。
#'       \item 数值 <= 18：视为"岁龄"。
#'     }
#'   \item **查表标准**：计算出的年龄将**向下取整** (floor) 为周岁进行查表。
#' }
#'
#' **2. 身高查表逻辑 (Height Matching)**
#' \itemize{
#'   \item 依据标准，身高需精确匹配表格中的区间。
#'   \item 函数会对输入的 `height_col` 进行**四舍五入** (\code{floor(x + 0.5)}) 取整后，再与标准表中的身高段进行匹配。
#' }
#'
#' **3. 血压评价标准 (Evaluation Criteria)**
#' 依据 2017 年国标及临床指南，评价结果分为以下四类：
#' \itemize{
#'   \item **正常 (Normal)**: SBP < P90 且 DBP < P90
#'   \item **正常高值 (High-normal)**: P90 ≤ BP < P95，或 BP ≥ 120/80 mmHg (且未达到高血压标准)
#'   \item **1期高血压 (Stage 1 Hypertension)**: P95 ≤ BP < P99 + 5 mmHg
#'   \item **2期高血压 (Stage 2 Hypertension)**: BP ≥ P99 + 5 mmHg
#' }
#' *注：收缩压(SBP)和舒张压(DBP)分别评价，最终结果取两者中较严重者。*
#'
#' @param data 一个数据框 (data.frame)，包含待评价的儿童体检数据。
#' @param sex_col 字符串。指定性别列的名称（默认为 "性别"）。
#'   \itemize{
#'     \item 数据要求：必须包含 "男" 或 "女"，或可被转换为字符的标识。
#'   }
#' @param age_col 字符串。指定年龄列的名称（默认为 "年龄"）。
#'   \itemize{
#'     \item 支持格式：数值 (如 10.5)、字符串 (如 "10岁"、"120月")。
#'     \item 范围：3岁 ~ 17岁 (不满3岁或超过18岁将返回 "无法评价")。
#'   }
#' @param height_col 字符串。指定身高列的名称（默认为 "身高"）。
#'   \itemize{
#'     \item 单位：**厘米 (cm)**。需注意数据应为数值型。
#'   }
#' @param sbp_col 字符串。指定收缩压(高压)列的名称（默认为 "收缩压"）。
#'   \itemize{
#'     \item 单位：**毫米汞柱 (mmHg)**。
#'   }
#' @param dbp_col 字符串。指定舒张压(低压)列的名称（默认为 "舒张压"）。
#'   \itemize{
#'     \item 单位：**毫米汞柱 (mmHg)**。
#'   }
#' @param language 字符串。评价结果的语言，可选 `"chinese"`（默认）或 `"english"`。
#'   \itemize{
#'     \item `"chinese"`: 返回中文标识（正常、正常高值、1期高血压、2期高血压）
#'     \item `"english"`: 返回英文标识（Normal、High-normal、Stage 1、Stage 2）
#'   }
#'
#' @return 返回原始数据框 `data`，并在最后增加一列：
#' \item{BP_Evaluation}{字符型列，包含评价结果："正常"、"正常高值"、"1期高血压"、"2期高血压"、"缺失" 或 "无法评价(年龄/身高超出范围)"。}
#'
#' @import dplyr
#' @import stringr
#' @export
#'
#' @examples
#' # 1. 基础用法
#' df_basic <- data.frame(
#'   性别 = c("男", "女"),
#'   年龄 = c(10, 12),
#'   身高 = c(140, 150),
#'   收缩压 = c(110, 130),
#'   舒张压 = c(70, 85)
#' )
#' # evaluate_bp(df_basic)
#'
#' # 2. 处理复杂年龄格式
#' df_complex <- data.frame(
#'   sex = c("男", "男", "女"),
#'   age = c("75月", "3岁5月", "120"), # 分别对应：6.25岁, 3.41岁, 10岁
#'   ht = c(120.5, 98, 140),
#'   sbp = c(110, 90, 130),
#'   dbp = c(70, 60, 85)
#' )
#' # 指定列名进行评价
#' # evaluate_bp(df_complex,
#' #             sex_col = "sex", age_col = "age", height_col = "ht",
#' #             sbp_col = "sbp", dbp_col = "dbp")
#'
#' # 3. 使用英文标识
#' # evaluate_bp(df_basic, language = "english")
evaluate_bp <- function(data,
                        sex_col = "性别",
                        age_col = "年龄",
                        height_col = "身高",
                        sbp_col = "收缩压",
                        dbp_col = "舒张压",
                        language = c("chinese", "english")) {

  # 0. 匹配语言参数
  language <- match.arg(language)

  # 定义评价标签（中英文）
  labels <- if (language == "chinese") {
    list(
      normal = "正常",
      high_normal = "正常高值",
      stage1 = "1期高血压",
      stage2 = "2期高血压",
      missing = "缺失",
      out_of_range = "无法评价(年龄/身高超出范围)"
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

  # 1. 检查必要列是否存在
  required_cols <- c(sex_col, age_col, height_col, sbp_col, dbp_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("数据中缺少以下列: ", paste(missing_cols, collapse = ", "))
  }

  # ==========================================================================
  # 定义内部辅助函数：智能解析年龄 (鲁棒性处理)
  # ==========================================================================
  parse_smart_age <- function(x) {
    # 转换为字符并去除所有空格
    x_str <- stringr::str_remove_all(as.character(x), "\\s+")
    # 匹配数字的正则模式 (支持小数)
    num_pattern <- "\\d+(\\.\\d+)?"

    sapply(x_str, function(s) {
      if (is.na(s) || s == "NA" || s == "") return(NA_real_)

      val <- NA_real_

      # 模式1: "3岁5月" (同时包含岁和月)
      if (stringr::str_detect(s, "岁") && stringr::str_detect(s, "月")) {
        parts <- stringr::str_match_all(s, num_pattern)[[1]][,1]
        if (length(parts) >= 2) {
          # 公式：岁 + 月/12
          val <- as.numeric(parts[1]) + as.numeric(parts[2]) / 12
        }
      }
      # 模式2: "6岁" / "6.5岁" (仅包含岁)
      else if (stringr::str_detect(s, "岁")) {
        num <- stringr::str_extract(s, num_pattern)
        val <- as.numeric(num)
      }
      # 模式3: "75月" (仅包含月)
      else if (stringr::str_detect(s, "月")) {
        num <- stringr::str_extract(s, num_pattern)
        val <- as.numeric(num) / 12
      }
      # 模式4: 纯数字 / 纯数字字符串 ("6", "6.5", "74")
      else {
        num_val <- as.numeric(s)
        if (!is.na(num_val)) {
          # 智能阈值判断:
          # 3-17岁是量表区间。18岁以上通常不适用此表。
          # 如果数值 > 18，极大概率是“月龄”(如74月)
          # 如果数值 <= 18，默认为“岁龄”
          if (num_val > 18) {
            val <- num_val / 12
          } else {
            val <- num_val
          }
        }
      }
      return(val)
    })
  }

  # 2. 准备数据与清洗
  # 复制数据并创建临时ID，确保后续合并不乱序
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
      # 步骤A: 智能解析年龄
      Age_Parsed_ = parse_smart_age(Age_Raw_),

      # 步骤B: 标准化处理
      # 年龄：向下取整 (周岁)，对应量表中的 Age
      Age_Final_ = floor(Age_Parsed_),

      # 身高：四舍五入 (符合国标取整查表要求)
      # floor(x + 0.5) 是标准的四舍五入算法
      Height_Final_ = floor(Height_ + 0.5)
    )

  # 3. 加载参照数据
  standards <- CNChildBP::bp_standards

  # 4. 匹配逻辑 (查表)
  matched <- work_data %>%
    # 过滤掉无法解析年龄或身高的行 (这些行最后会标记为无法评价)
    dplyr::filter(!is.na(Age_Final_), !is.na(Height_Final_)) %>%
    # 按 性别 和 年龄 进行连接
    dplyr::left_join(standards, by = c("Sex_" = "Sex", "Age_Final_" = "Age")) %>%
    # 筛选符合身高区间的行
    dplyr::filter(Height_Final_ >= Height_Lower & Height_Final_ <= Height_Upper)

  # 5. 评价计算 (根据 P90, P95, P99+5 判定)
  results_calculated <- matched %>%
    dplyr::mutate(
      # --- 收缩压 (SBP) 评价 ---
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

      # --- 舒张压 (DBP) 评价 ---
      dbp_status = dplyr::case_when(
        is.na(DBP_) ~ labels$missing,
        DBP_ >= (DBP_P99 + 5) ~ labels$stage2,
        DBP_ >= DBP_P95 ~ labels$stage1,
        DBP_ >= DBP_P90 | DBP_ >= 80 ~ labels$high_normal,
        TRUE ~ labels$normal
      ),

      # --- 综合评价 (取两者中较严重者) ---
      BP_Evaluation = dplyr::case_when(
        sbp_status == labels$stage2 | dbp_status == labels$stage2 ~ labels$stage2,
        sbp_status == labels$stage1 | dbp_status == labels$stage1 ~ labels$stage1,
        sbp_status == labels$high_normal | dbp_status == labels$high_normal ~ labels$high_normal,
        sbp_status == labels$missing | dbp_status == labels$missing ~ labels$missing,
        TRUE ~ labels$normal
      )
    ) %>%
    dplyr::select(..temp_id.., BP_Evaluation)

  # 6. 将结果合并回原始数据
  final_result <- data
  final_result$..temp_id.. <- 1:nrow(final_result)

  final_result <- final_result %>%
    dplyr::left_join(results_calculated, by = "..temp_id..") %>%
    dplyr::select(-..temp_id..)

  # 填补未匹配到的行 (年龄过大/过小，或者身高数据缺失)
  final_result$BP_Evaluation[is.na(final_result$BP_Evaluation)] <- labels$out_of_range

  return(final_result)
}
