## Global variable declarations for R CMD check
## Place global variable declarations here to satisfy R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "bp_standards",
    "Sex_",
    "Age_Raw_", "Age_Parsed_", "Age_Final_",
    "Height_", "Height_Final_", "Height_Lower", "Height_Upper",
    "..temp_id..", "BP_Evaluation",
    "SBP_","DBP_",
    "SBP_P90","SBP_P95","SBP_P99",
    "DBP_P90","DBP_P95","DBP_P99"
  ))
}
