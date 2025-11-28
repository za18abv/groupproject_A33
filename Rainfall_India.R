#Install and load library readr & tidyverse
library(readr)
library(tidyverse)

# Import the Dataset
rainfall_india <- read_csv("rainfall_india.csv")
View(rainfall_india)

print(colnames(rainfall_india))

# printing first 5 rows
print(head(rainfall_india, 5))

# Creates a dataset without NA values
rf_ds <- subset(rainfall_india, rainfall_india$ANNUAL > 60)

#Creating Decades using the years
rainfall_india$Decade <- (rainfall_india$YEAR %/% 10) * 10

#Removes NA values from dataset
rainfall_india <- na.omit(rainfall_india)

#Rename column from SUBDIVISION to REGION
colnames (rainfall_india)[1] <- "REGION"

#Calculate mean annual rainfall per region per decade
mean_annual_rainfall_per_region_per_decade <- rainfall_india %>% group_by(REGION, Decade) %>% summarize( MeanAnnualRainfall = mean(ANNUAL, na.rm = TRUE), .groups = "drop" )

#View mean annual rainfall per region per decade table created
View(mean_annual_rainfall_per_region_per_decade)

#Sort table in descending order of mean annual rainfall
sorted_meanAnnualRainfall_table <- mean_annual_rainfall_per_region_per_decade %>%
  arrange(desc(MeanAnnualRainfall))

#View sorted table (to see decade with the highest mean annual rainfall)
View(sorted_meanAnnualRainfall_table)

#Return data for the 1910s decade (decade with the highest mean annual rainfall = 5090.100)
decade_1910_table <- mean_annual_rainfall_per_region_per_decade %>%
  filter(Decade == 1910)

#View the 1910s decade table
View(decade_1910_table)
