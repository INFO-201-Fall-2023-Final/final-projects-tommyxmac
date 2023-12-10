library(dplyr)
library(readr)

bd_df <- read_csv("GroupBD_Project.csv")

df <- data.frame(
  Racial.Groups = c("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", "Two or more races", "White"),
  AP.Enrollment = c(12158, 345173, 273207, 717464, 6894, 103718, 1423670),
  IB.Enrollment = c(612, 22376, 23235, 44513, 489, 7382, 68933),
  Dual.Enrollment = c(11437, 91894, 147577, 338358, 3571, 51744, 981077),
  Talented.and.Gifted.Enrollment = c(19408, 307138, 240229, 571709, 5635, 140451, 1671040)
)

data <- data.frame(
  Racial_Groups = c(
    "American Indian or Alaska Native", "Asian", "Black or African American",
    "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander",
    "Two or more races", "White"
  ),
  High_School_Grad_Percentage = c(78.10, 88.20, 88.30, 73.10, 87.60, 81.30, 93.70),
  Bachelor_Degree_Percentage = c(16.80, 57.40, 25.40, 20.40, 19.80, 27.90, 39.00)
)
