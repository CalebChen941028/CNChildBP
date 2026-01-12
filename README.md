
# CNChildBP: 中国儿童青少年血压评价工具包 🇨🇳🩺

<!-- badges: start -->
[![R-CMD-check](https://github.com/CalebChen941028/CNChildBP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CalebChen941028/CNChildBP/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R version](https://img.shields.io/badge/R-%3E%3D%203.5-blue.svg)](https://cran.r-project.org/)
[![Language](https://img.shields.io/badge/lang-R-orange.svg)](https://www.r-project.org/)
<!-- badges: end -->

**CNChildBP** (Chinese Child Blood Pressure) 是一个专为医学统计、流行病学研究及临床筛查设计的 R 语言工具包。

本工具包严格依据  **2017年《中国3～17岁儿童性别、年龄别和身高别血压参照标准》** ，实现了对儿童青少年血压数据的 **自动化清洗** 、**批量查表**及 **分级评价** 。它解决了传统手工查表效率低、易出错、且难以处理非标准化数据（如“3岁5月”）的痛点。

## 🌟 核心特性 (Key Features)

* **⚡️ 开箱即用 (Ready-to-Use)**
  * 内置完整的 2017 年国标数据，安装即用，无需用户手动整理或导入繁琐的查表数据。
* **🧠 智能解析 (Smart Parsing)**
  * 内置鲁棒的数据清洗算法，自动识别并处理混乱的年龄格式。
  * 支持格式：`10` (数值), `"10"` (字符), `"10岁"`, `"3岁5月"`, `"75月"` (纯月龄), `"10.5"` (小数岁)。
* **🌍 双语支持 (Bilingual Support)**
  * 支持中文和英文评价结果输出。
  * 使用 `language = "chinese"` (默认) 或 `language = "english"` 参数切换。
  * 当 `sex_col/age_col/...` 未显式指定时，函数会按 `language` 自动选择一套默认列名映射：
    * 中文：`性别/年龄/身高/收缩压/舒张压`
    * 英文：`sex/age/height/sbp/dbp`
* **📏 严谨合规 (Strict Compliance)**
  * **身高处理** ：严格遵循国标要求的 **四舍五入** (`floor(x + 0.5)`) 取整查表，而非简单的去尾。
  * **年龄处理** ：严格遵循医学统计的 **周岁** (`floor(x)`) 查表。
  * **分级判定** ：精确执行 P90 / P95 / P99+5 界值，并包含  **120/80 mmHg 封顶原则** （即血压 ≥ 120/80 至少判定为正常高值）。

## 📦 安装指南 (Installation)

### 方式 1: 从 GitHub 在线安装 (推荐)

这是获取最新版本、享受 bug 修复的最佳方式。

```
# 如果未安装 devtools，请先运行:
# install.packages("devtools")

devtools::install_github("CalebChen941028/CNChildBP")

```

### 方式 2: 本地离线安装

如果您处于内网环境，或通过文件分享获得了安装包（`.tar.gz`）：

```
# 请修改为您本地文件的实际路径
install.packages("C:/Downloads/CNChildBP_0.1.0.tar.gz", repos = NULL, type = "source")

```

## 🚀 快速上手 (Quick Start)

### 多场景批量评价示例

下面提供多种常见使用场景的示例，便于在真实数据中快速套用。

```r
# 加载包
library(CNChildBP)

# ------------------ 示例 1：中文列名（默认） ------------------
# 数据列名为：性别/年龄/身高/收缩压/舒张压，直接调用即可（输出中文标签）
df_cn <- data.frame(
  姓名 = c("张三", "李四"),
  性别 = c("男", "女"),
  年龄 = c(10, "10岁5月"),
  身高 = c(140.4, 142.6),
  收缩压 = c(110, 118),
  舒张压 = c(70, 78),
  stringsAsFactors = FALSE
)
res_cn <- evaluate_bp(df_cn)
print(res_cn)


# ------------------ 示例 2：英文列名（切换 language） ------------------
# 若数据使用英文列名（sex/age/height/sbp/dbp），设置 language="english"
df_en <- data.frame(
  sex = c("male", "female"),
  age = c(10, 12),
  height = c(140, 150),
  sbp = c(110, 130),
  dbp = c(70, 85),
  stringsAsFactors = FALSE
)
res_en <- evaluate_bp(df_en, language = "english")
print(res_en)


# ------------------ 示例 3：列名不一致（显式映射） ------------------
# 假设你的数据里列名为 sex_id / age_years / h_cm / systolic / diastolic
df_custom <- data.frame(
  id = 1:2,
  sex_id = c("M", "F"),
  age_years = c("3岁5月", "75months"),
  h_cm = c(98.4, 120.5),
  systolic = c(90, 122),
  diastolic = c(60, 82),
  stringsAsFactors = FALSE
)
# 显式指定每列的映射
res_custom <- evaluate_bp(
  df_custom,
  sex_col = "sex_id",
  age_col = "age_years",
  height_col = "h_cm",
  sbp_col = "systolic",
  dbp_col = "diastolic",
  language = "chinese" # 输出中文标签
)
print(res_custom)


# ------------------ 示例 4：混合年龄格式与身高取整示例 ------------------
df_mixed <- data.frame(
  sex = c("男","女"),
  age = c("6.5", "75months"),
  height = c(140.5, 140.4),
  sbp = c(119, 121),
  dbp = c(79, 81),
  stringsAsFactors = FALSE
)
res_mixed <- evaluate_bp(df_mixed)
print(res_mixed)
# 说明：height 采用 round-half-up（140.5 -> 141；140.4 -> 140），年龄解析支持 75months / 6.5 等格式


# ------------------ 示例 5：缺失值与批量导出 ------------------
df_na <- data.frame(
  sex = c("male","male"),
  age = c(10, 11),
  height = c(140, 145),
  sbp = c(NA, 130),
  dbp = c(70, NA),
  stringsAsFactors = FALSE
)
res_na <- evaluate_bp(df_na, language = "english")
print(res_na)
# 导出为 UTF-8 CSV（适用于包含中文的结果）
write.csv(res_cn, "CNChildBP_results_cn.csv", row.names = FALSE, fileEncoding = "UTF-8")


# ------------------ 示例 6：从 Excel 批量处理（readxl） ------------------
# df_file <- readxl::read_excel("体检数据.xlsx", sheet = 1)
# res_file <- evaluate_bp(df_file)
# readr::write_csv(res_file, "体检结果.csv")


# ------------------ 示例 7：在 dplyr 管线中使用（按组统计） ------------------
# library(dplyr)
# res_summary <- res_cn %>%
#   group_by(性别, BP_Evaluation) %>%
#   summarise(n = n())
# print(res_summary)

```

## 📋 数据字典与格式说明

### 输入数据要求

`evaluate_bp()` 会根据 `language` 自动选择一套默认列名：

- 当 `language = "chinese"`（默认）时，优先使用：`性别` / `年龄` / `身高` / `收缩压` / `舒张压`。
- 当 `language = "english"` 时，优先使用：`sex` / `age` / `height` / `sbp` / `dbp`。

行为要点：

- 如果你显式传入任意 `*_col`（例如 `sex_col = "sex_id"`），函数将使用你提供的映射并不再尝试自动切换；
- 如果未显式传参且当前语言对应的一整套列名不存在，函数会尝试使用另一套（中文↔英文）作为 fallback；
- 若最终仍缺少必需列，函数会报错（错误信息会依据 `language` 本地化）。

字段与内容要求（兼容说明）：

- `性别 / sex`：建议为字符或因子，常见值为 `男/女` 或 `male/female`（函数会自动规范化，支持 `m/f` 等常见写法）；
- `年龄 / age`：单位为年（years）。支持多种输入格式：纯数字（`10`, `10.5`）、中文混写（`3岁5月`）、英文缩写（`3y5m`）、纯月龄（`75months` 或 `75月`）。函数会智能解析并转换为小数岁用于计算，最终查表按整岁向下取整（floor）。
- `身高 / height`：单位 cm，数值型。查表前会使用“round half up”（即 `floor(x + 0.5)`）取整，例如 `140.4 -> 140`, `140.5 -> 141`。
- `收缩压 / sbp` 与 `舒张压 / dbp`：单位 mmHg，数值型。缺失值会被标记为 `缺失`/`Missing`。

示例：如果你的数据已经使用中文列名，可以直接调用 `evaluate_bp(df)`（默认 `language = "chinese"`）；如果数据使用英文列名，可调用 `evaluate_bp(df, language = "english")` 或显式传参映射 `sex_col = "sex"` 等。

### 输出结果含义

函数会在原数据框后追加一列 `BP_Evaluation`。默认返回中文标识，可通过 `language = "english"` 参数切换为英文。

性别字段兼容常见英文写法（例如 `male/female`、`m/f`），会自动规范化为国标表中的“男/女”后再查表。

**中文标识（默认）：**

| **评价结果**  | **判定标准 (满足其一即可)**                      |
| ------------------- | ------------------------------------------------------ |
| **正常**      | SBP < P90**且**DBP < P90                         |
| **正常高值**  | P90 ≤ BP < P95**或**BP ≥ 120/80 (且未达高血压) |
| **1期高血压** | P95 ≤ BP < P99 + 5 mmHg                               |
| **2期高血压** | BP ≥ P99 + 5 mmHg                                     |
| **缺失**      | 血压数据缺失                                           |
| **无法评价(年龄/身高超出范围)**  | 年龄不在 3-17 岁范围内，或身高超出极值                 |

**英文标识（language = "english"）：**

| **英文标识**  | **判定标准 (满足其一即可)**                      |
| ------------------- | ------------------------------------------------------ |
| **Normal**      | SBP < P90**且**DBP < P90                         |
| **High-normal**  | P90 ≤ BP < P95**或**BP ≥ 120/80 (且未达高血压) |
| **Stage 1** | P95 ≤ BP < P99 + 5 mmHg                               |
| **Stage 2** | BP ≥ P99 + 5 mmHg                                     |
| **Missing**      | 血压数据缺失                                           |
| **N/A**  | 年龄不在 3-17 岁范围内，或身高超出极值                 |

## 🔬 医学逻辑细节 (Methodology)

本包的评价逻辑严格遵循范晖等人(2017)的文献及临床指南：

1. **正常高值判定 (High-normal Logic)**
   * 引入了  **120/80 mmHg 封顶规则** ：即如果儿童血压 $\ge 120/80$ mmHg，即使未达到同龄人 $P_{90}$，也判定为正常高值（除非已达到高血压标准）。这避免了体格高大的青少年血压超过成人高血压界值却被判定为“正常”的风险。
2. **综合评价规则**
   * **收缩压 (SBP)** 与 **舒张压 (DBP)** 独立评价。
   * 最终结果取两者中 **较严重** 的等级。例如：收缩压为“正常”，舒张压为“1期高血压”，则最终评价为“1期高血压”。
3. **诊断 vs. 单次分级（重要）**
  * 本包输出的是基于一次测量记录的**分级评价/筛查分层**。
  * 根据临床指南，当收缩压或舒张压 ≥ P95 时，应在间隔 2–4 周后复测；只有在不同时间点连续多次（通常为 3 个不同时点）测量均 ≥ P95 时，方可用于儿童高血压的临床诊断。
  * 因此：建议将本工具用于筛查、分层和随访建议，不能替代正规临床诊断流程。
4. **计算精度**
   * **年龄** ：向下取整 (Floor)。例如 `10.9岁` 视为 `10岁组`。
   * **身高** ：四舍五入 (Round half up)。例如 `140.4cm` -> `140cm`，`140.5cm` -> `141cm`。

## 📚 参考文献 (Reference)

如果您的研究使用了本工具包，建议引用以下原始标准文献：

> **范晖, 闫银坤, 米杰. 中国3～17岁儿童性别、年龄别和身高别血压参照标准修订[J]. 中华高血压杂志, 2017, 25(5): 428-435. DOI: 10.16439/j.cnki.1673-7245.2017.05.009**

### 📚 数据来源、引用与授权提示

本包内置的数据集 `bp_standards` 是将上述文献/标准中的血压参照表转录为可供程序查表的结构化数据，用于科研分析与批量筛查的可重复性。

合规提示（重要）：

- 如果你计划将本包用于公开发布、商业用途或再分发，请务必确认你对该参照表的再分发权利（包括但不限于出版社/期刊/标准文件的版权要求）。
- 若未来需要更严格的合规策略，一个常见做法是：不在包中直接分发参照表数据（移除 `data/bp_standards.rda`），改为由用户在本地通过脚本生成/导入。

## 📄 许可证 (License)

MIT License. 欢迎学术界、医疗机构及公共卫生部门免费使用、修改和分发。

*由 [陈亮亮/Liangliang Chen] 维护开发*
