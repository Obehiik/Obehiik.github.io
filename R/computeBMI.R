#' Compute Body Mass Index (BMI)
#'
#' This function prompts the user to input their weight in kilograms and height in centimeters, calculates their BMI, and categorizes it.
#'
#' @return Prints the BMI value along with the category (Underweight, Normal weight, Overweight, Obesity).
#'
#' @details
#' The BMI categories are defined as follows:
#' \itemize{
#'   \item \strong{Underweight:} BMI < 18.5
#'   \item \strong{Normal weight:} BMI 18.5-24.9
#'   \item \strong{Overweight:} BMI 25-29.9
#'   \item \strong{Obesity:} BMI ≥ 30
#' }
#'
#' @export
computeBMI <- function() {
  writeLines("Calculate your Body Mass Index\n")
  weight <- as.numeric(readline("Enter your weight (kg): "))
  height <- as.numeric(readline("Enter your height (cm): "))

  bmi <- round((weight) / ((height / 100) ^ 2), 2)

  if (bmi < 18.5) {
    print(paste("BMI:", bmi, "<-> Underweight"))
  } else if (bmi >= 18.5 & bmi <= 24.9) {
    print(paste("BMI:", bmi, "<-> Normal weight"))
  } else if (bmi >= 25 & bmi <= 29.9) {
    print(paste("BMI:", bmi, "<-> Overweight"))
  } else if (bmi >= 30) {
    print(paste("BMI:", bmi, "<-> Obesity"))
  } else {
    print("Error: Invalid input for weight or height")
  }
}
