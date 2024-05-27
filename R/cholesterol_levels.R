#' Cholesterol Levels Function
#'
#' This function prompts the user to input their age and cholesterol readings (LDL, triglycerides, and HDL) and then categorizes the cholesterol levels based on the provided values.
#'
#' @return Prints the category of cholesterol levels based on the inputs.
#'
#' @details
#' The cholesterol levels are categorized based on age:
#' \itemize{
#'   \item \strong{For age <= 19:}
#'   \itemize{
#'     \item Normal cholesterol: Total cholesterol < 170 mg/dL, Non-HDL cholesterol < 120 mg/dL, HDL cholesterol > 45 mg/dL, and LDL cholesterol < 110 mg/dL
#'     \item Borderline high: Total cholesterol 170-199 mg/dL, Non-HDL cholesterol 120-144 mg/dL, HDL cholesterol 50-55 mg/dL, and LDL cholesterol 110-129 mg/dL
#'     \item High: Total cholesterol ≥ 200 mg/dL, Non-HDL cholesterol ≥ 145 mg/dL, HDL cholesterol < 45 mg/dL, and LDL cholesterol ≥ 130 mg/dL
#'   }
#'   \item \strong{For age >= 20:}
#'   \itemize{
#'     \item Normal cholesterol: Total cholesterol < 200 mg/dL, Non-HDL cholesterol < 130 mg/dL, HDL cholesterol ≥ 40 mg/dL, and LDL cholesterol < 100 mg/dL
#'     \item Borderline high: Total cholesterol 200-239 mg/dL, Non-HDL cholesterol 130-159 mg/dL, HDL cholesterol ≥ 40 mg/dL, and LDL cholesterol 100-159 mg/dL
#'     \item High: Total cholesterol ≥ 240 mg/dL, Non-HDL cholesterol ≥ 160 mg/dL, HDL cholesterol < 40 mg/dL, and LDL cholesterol ≥ 160 mg/dL
#'   }
#' }
#'
#' @export

cholesterol_levels <- function() {
  age <- as.numeric(readline("Enter your age: "))
  ldl_cholesterol <- as.numeric(readline("LDL Cholesterol (mg/dL): "))
  triglycerides <- as.numeric(readline("Triglycerides (mg/dL): "))
  hdl_cholesterol <- as.numeric(readline("HDL Cholesterol (mg/dL): "))

  total_cholesterol <- hdl_cholesterol + ldl_cholesterol + (0.2 * triglycerides)
  non_HDL_cholesterol <- total_cholesterol - hdl_cholesterol

  # Print the total cholesterol result
  print(paste("The total cholesterol is:", total_cholesterol, "mg/dL"))

  if (age <= 19) {
    if (total_cholesterol < 170 & non_HDL_cholesterol < 120 & hdl_cholesterol > 45 & ldl_cholesterol < 110) {
      print("Normal cholesterol")
    } else if ((total_cholesterol >= 170 & total_cholesterol <= 199) &
               (non_HDL_cholesterol >= 120 & non_HDL_cholesterol <= 144) &
               (hdl_cholesterol >= 50 & hdl_cholesterol <= 55) &
               (ldl_cholesterol >= 110 & ldl_cholesterol <= 129)) {
      print("Borderline high")
    } else if (total_cholesterol >= 200 & non_HDL_cholesterol >= 145 &
               hdl_cholesterol < 45 & ldl_cholesterol >= 130) {
      print("High")
    } else {
      print("Values do not match any category for age <= 19")
    }
  } else if (age >= 20) {
    if (total_cholesterol < 200 & non_HDL_cholesterol < 130 & hdl_cholesterol >= 40 & ldl_cholesterol < 100) {
      print("Normal cholesterol")
    } else if ((total_cholesterol >= 200 & total_cholesterol <= 239) &
               (non_HDL_cholesterol >= 130 & non_HDL_cholesterol <= 159) &
               (hdl_cholesterol >= 40) &
               (ldl_cholesterol >= 100 & ldl_cholesterol <= 159)) {
      print("Borderline high")
    } else if (total_cholesterol >= 240 | non_HDL_cholesterol >= 160 |
               hdl_cholesterol < 40 | ldl_cholesterol >= 160) {
      print("High")
    } else {
      print("Values do not match any category for age >= 20")
    }
  } else {
    print("Invalid age")
  }
}

