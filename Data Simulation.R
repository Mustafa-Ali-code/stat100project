library(ggplot2)
library(dplyr)

set.seed(42)
df <- data.frame(
  Region_Area = c("Metro City", "Small Town", "Rural Village", "Industrial Hub", "Suburb A",
                  "Suburb B", "Coastal Town", "Mountain Region", "Tech City", "Agricultural Area"),
  Income_Level = c("High", "Low", "Low", "Medium", "High", 
                   "Medium", "Medium", "Low", "High", "Low"),
  Population = sample(5000:500000, 10),
  Urban_Rural = c("Urban", "Rural", "Rural", "Urban", "Urban", 
                  "Urban", "Rural", "Rural", "Urban", "Rural"),
  Broadband_Availability = round(runif(10, 40, 100), 2),
  Education_Level = c("College", "High School", "High School", "Some College", "College", 
                      "Some College", "Some College", "High School", "College", "High School"),
  Unemployment_Rate = round(runif(10, 2, 12), 2),
  Median_Age = sample(20:60, 10),
  Race_Ethnicity = c("Mixed", "White", "Black", "Asian", "Latino", 
                     "Mixed", "White", "Black", "Asian", "Latino"),
  Healthcare_Access = c("High", "Limited", "Very Limited", "Moderate", "High", 
                        "Moderate", "Limited", "Very Limited", "High", "Limited")
)

print(df)

# Broadband Availability vs. Population
ggplot(df, aes(x = Population / 1000, y = Broadband_Availability, color = Urban_Rural)) +
  geom_point(size = 3) +
  labs(title = "Broadband Availability vs. Population",
       x = "Population in Thousands",
       y = "Broadband Availability (%)") +
  theme_minimal()

# Broadband Availability by Income Level
ggplot(df, aes(x = Income_Level, y = Broadband_Availability, fill = Income_Level)) +
  geom_boxplot() +
  labs(title = "Broadband Availability by Income Level",
       x = "Income Level",
       y = "Broadband Availability (%)") +
  theme_minimal()

# Unemployment Rate vs. Broadband Availability
ggplot(df, aes(x = Unemployment_Rate, y = Broadband_Availability, color = Income_Level)) +
  geom_point(size = 3) +
  labs(title = "Unemployment Rate vs. Broadband Availability",
       x = "Unemployment Rate (%)",
       y = "Broadband Availability (%)") +
  theme_minimal()