library(dplyr)
coll_prep_rates_df <- read.csv("College prep rates.csv")
Unemployment_df <- read.csv("Unemployement Rates.csv")
Higher_edu_df <- read.csv("Rates of higher education.csv")
Uninsured_df <- read.csv("Uninsured Rates.csv")
Avoided_cost_df <- read.csv("Avoided Cost Because of Care.csv")
Median_income_df <- read.csv("Median household income.csv")
Race_fatality_df <- read.csv("Covid-19 fatality rates by race.csv")
merged_df <- merge(coll_prep_rates_df, Higher_edu_df, by = "Racial.Groups", all = TRUE)
merged_df <- select(merged_df, 2:1, X, everything())
merged_df <- left_join(merged_df, Median_income_df, by = c("Racial.Groups" = "Race"))
merged_df <- left_join(merged_df, Unemployment_df, by = c("Racial.Groups" = "race"))
merged_df <- left_join(merged_df, Uninsured_df, by = c("Racial.Groups" = "Race"))
merged_df <- left_join(merged_df, Avoided_cost_df, by = c("Racial.Groups" = "race"))
#COVID DEATHS SECTION CLEAN-UP
data_2020 <- subset(Race_fatality_df, str_detect(Date, "^2020"))
race_cols <- grep("White|Hispanic|AIAN|Multiracial|Asian|Black", names(data_2020), value = TRUE)
avg_data <- round(colMeans(data_2020[race_cols], na.rm = TRUE))

avg_data_transposed <- t(avg_data)
data_2020_df <- as.data.frame(avg_data_transposed)
colnames(data_2020_df) <- race_cols
data_2021 <- subset(Race_fatality_df, str_detect(Date, "^2021"))
race_cols_2021 <- grep("White|Hispanic|AIAN|Multiracial|Asian|Black", names(data_2021), value = TRUE)
avg_data_2021 <- round(colMeans(data_2021[race_cols_2021], na.rm = TRUE))

avg_data_transposed_2021 <- t(avg_data_2021)
data_2021_df <- as.data.frame(avg_data_transposed_2021)
colnames(data_2021_df) <- race_cols_2021

#Add new numerical variable using data from previously made dataframe
new_data <- data.frame(
  Race = c("White", "Black or African American", "Asian", "American Indian or Alaska Native", "Two or more races", "Hispanic or Latino of any race"),
  `2020_Covid_cases` = c(40918, 13945, 2475, 1530, 1928, 24219),
  `2020_Covid_deaths` = c(1806, 734, 142, 47, 31, 691),
  `2020_Covid_Hosp` = c(4466, 1878, 372, 309, 122, 2617),
  `2020_Covid_Tests` = c(547937, 100562, 37091, 3630, 41, 99182),
  `2021_Covid_cases` = c(183099, 42039, 10053, 5214, 7993, 85653),
  `2021_Covid_deaths` = c(4856, 1289, 291, 109, 103, 1583),
  `2021_Covid_Hosp` = c(10386, 3660, 815, 670, 348, 4992),
  `2021_Covid_Tests` = c(1908420, 330654, 326107, 177877, 21814, 848455)
)
merged_df <- left_join(merged_df, new_data, by = c("Racial.Groups" = "Race"))
#Add new categorical variable 
merged_df$Most_impacted_groups <- merged_df$Racial.Groups %in% c("Black or African American", "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native")

#Create summarization table dataframe
Covid_impact_on_communities_df <- merged_df[, c("Racial.Groups",
                                                "X2020_Covid_cases", "X2020_Covid_deaths", "X2020_Covid_Hosp", "X2020_Covid_Tests",
                                                "X2021_Covid_cases", "X2021_Covid_deaths", "X2021_Covid_Hosp", "X2021_Covid_Tests")]

Covid_impact_on_communities_df$Deaths_2020_percentage <- round((Covid_impact_on_communities_df$X2020_Covid_deaths / Covid_impact_on_communities_df$X2020_Covid_cases) * 100, 2)
Covid_impact_on_communities_df$Deaths_2021_percentage <- round((Covid_impact_on_communities_df$X2021_Covid_deaths / Covid_impact_on_communities_df$X2021_Covid_cases) * 100, 2)

Covid_impact_on_communities_df$Hospitalization_2020_percentage <- round((Covid_impact_on_communities_df$X2020_Covid_Hosp / Covid_impact_on_communities_df$X2020_Covid_cases) * 100, 2)
Covid_impact_on_communities_df$Hospitalization_2021_percentage <- round((Covid_impact_on_communities_df$X2021_Covid_Hosp / Covid_impact_on_communities_df$X2021_Covid_cases) * 100, 2)

Covid_impact_on_communities_df$Tests_2020_percentage <- round((Covid_impact_on_communities_df$X2020_Covid_Tests / Covid_impact_on_communities_df$X2020_Covid_cases) * 100, 2)
Covid_impact_on_communities_df$Tests_2021_percentage <- round((Covid_impact_on_communities_df$X2021_Covid_Tests / Covid_impact_on_communities_df$X2021_Covid_cases) * 100, 2)