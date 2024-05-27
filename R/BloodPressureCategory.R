#' Blood Pressure Category Function
#'
#' This function prompts the user to input their systolic and diastolic blood pressure readings and then categorizes the blood pressure based on the provided values.
#'
#' @return Prints the category of blood pressure based on the systolic and diastolic inputs.
#'
#' @details
#' The categories are defined as follows:
#' \itemize{
#'   \item \strong{Normal:} Systolic < 120 mmHg and Diastolic < 80 mmHg
#'   \item \strong{Elevated:} Systolic 120-129 mmHg and Diastolic < 80 mmHg
#'   \item \strong{High blood pressure (Hypertension) Stage 1:} Systolic 130-139 mmHg or Diastolic 80-89 mmHg
#'   \item \strong{High blood pressure (Hypertension) Stage 2:} Systolic 140-180 mmHg or Diastolic ≥ 90 mmHg
#'   \item \strong{Hypertensive crisis:} Systolic > 180 mmHg or Diastolic > 120 mmHg
#' }
#'
#' @export
BloodPressureCategory<-function(){
  writeLines("Find out your Blood Pressure Category \n")
  sbp<-as.numeric(readline("Enter your Systolic Blood Pressure (mmHg): "))
  dbp<-as.numeric(readline("Enter your Diastolic Blood Pressure (mmHg): "))

  if(sbp<120 & dbp<80){
    print("Normal")
  }else if (sbp>120 & sbp<=129 & dbp>80){
    print("Elevated")
  }else if (sbp>129 & sbp<=139 | dbp %in% 80:89){
    print("High blood pressure (Hypertension): STAGE 1")
  } else if( sbp>=140 & sbp<=180 | dbp>=90){
    print("High blood pressure (Hypertension): STAGE 2")
  }else if((sbp>180 & dbp>120) | dbp>120){
    print("Hypertensive crisis")
  }

}
