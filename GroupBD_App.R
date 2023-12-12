library(dplyr)
library(readr)
library(tidyr)

bd_df <- read_csv("GroupBD_Project.csv")

df <- data.frame(
  Racial.Groups = c("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", "Two or more races", "White"),
  AP.Enrollment = c(12158, 345173, 273207, 717464, 6894, 103718, 1423670),
  IB.Enrollment = c(612, 22376, 23235, 44513, 489, 7382, 68933),
  Dual.Enrollment = c(11437, 91894, 147577, 338358, 3571, 51744, 981077),
  Talented.and.Gifted.Enrollment = c(19408, 307138, 240229, 571709, 5635, 140451, 1671040)
)

grad_df <- data.frame(
  Racial_Groups = c(
    "American Indian or Alaska Native", "Asian", "Black or African American",
    "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander",
    "Two or more races", "White"
  ),
  High_School_Grad_Percentage = c(78.10, 88.20, 88.30, 73.10, 87.60, 81.30, 93.70),
  Bachelor_Degree_Percentage = c(16.80, 57.40, 25.40, 20.40, 19.80, 27.90, 39.00)
)

insurance_data <- data.frame(
  RacialGroup = c("American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", "Two or more races", "White"),
  '2020_uninsured_rates ' = c(NA, 8.8, 14.6, 29.3, NA, NA, 9.2),
  '2021_uninsured_rates ' = c(NA, 6.3, 14.1, 30.1, NA, NA, 8.7),
  avoided_care_cost_percentage = c(6.2, 2.8, 6.1, 4.3, 4.0, 5.5, 3.2)
)

# Remove spaces from column names
names(insurance_data) <- gsub(" ", "", names(insurance_data))

plot <- plot_ly()

plot <- add_trace(plot, data = insurance_data, x = ~RacialGroup, y = ~X2020_uninsured_rates., type = 'bar', name = '2020')
plot <- add_trace(plot, data = insurance_data, x = ~RacialGroup, y = ~X2021_uninsured_rates., type = 'bar', name = '2021')

plot <- layout(plot,
               title = "Healthcare Access: Uninsured Rates by Racial Groups (2020 vs 2021)",
               xaxis = list(title = "Racial Groups"),
               yaxis = list(title = "Uninsured Rates per Year (%)"),
               barmode = 'group')

unemployment_data <- data.frame(
  Racial.Groups = c("American Indian or Alaska Native", "Asian", "Black or African American", 
                    "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", 
                    "Two or more races", "White"),
  x2020_unemployment_percentage = c(11.7, 8.7, 11.4, 10.4, 8.9, 11.6, 7.3),
  x2021_unemployment_percentage = c(8.2, 5, 8.6, 5, 6.9, 8.2, 4.7)
)

data <- data.frame(
  group = c("American Indian or Alaska Native", "Asian", "Black or African American", 
            "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", "White"),
  values = c(52810, 108700, 52860, 62800, 69973, 81060),
  frame = c(1, 2, 3, 4, 5, 6)
)
unemployment_data_long <- gather(unemployment_data, key = "Year", value = "UnemploymentRate", -Racial.Groups)

