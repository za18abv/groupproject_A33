#Install and load library readr & tidyverse
library(readr)
library(tidyverse)

# Import the Dataset
rainfall_india <- read_csv("rainfall_india.csv")
View(rainfall_india)

print(colnames(rainfall_india))

# printing first 5 rows
print(head(rainfall_india, 5))

#Creating Decades using the years
rainfall_india$Decade <- (rainfall_india$YEAR %/% 10) * 10

#Removes NA values from dataset
rainfall_india <- na.omit(rainfall_india)

#Rename column from SUBDIVISION to REGION
colnames (rainfall_india)[1] <- "REGION"

#Calculate mean annual rainfall per region per decade
mean_annual_rainfall_per_region_per_decade <- rainfall_india %>% group_by(REGION, Decade) %>% summarize( MeanAnnualRainfall = mean(ANNUAL, na.rm = TRUE), .groups = "drop" )

#Sort table in descending order of mean annual rainfall
sorted_meanAnnualRainfall_table <- mean_annual_rainfall_per_region_per_decade %>%
  arrange(desc(MeanAnnualRainfall))

#View sorted table (to see decades in descending order of mean annual rainfall)
View(sorted_meanAnnualRainfall_table)

#Create a 1930s table (decade with the second highest mean annual rainfall)
decade_1930_table <- mean_annual_rainfall_per_region_per_decade %>%  filter(Decade == 1930)

view(decade_1930_table) 

#Obtain the max and min annual rainfall regions for the 1910 decade
#decade_1910_table$REGION[which.max(decade_1910_table$MeanAnnualRainfall)]

#decade_1910_table$REGION[which.min(decade_1910_table$MeanAnnualRainfall)]

#Filter rainfall dataset to return data for the 2 regions (Arunachal Pradesh & West Rajasthan) in 1930s
two_regions <- rainfall_india %>%
  filter(REGION %in% c("ARUNACHAL PRADESH", "WEST RAJASTHAN"), YEAR >= 1930,
         YEAR <= 1939)

#Extract the annual values for each group (For statistical testing)
arunachal_pradesh <- two_regions %>% 
  filter(REGION == "ARUNACHAL PRADESH") %>% 
  pull(ANNUAL)

west_rajasthan <- two_regions %>% 
  filter(REGION == "WEST RAJASTHAN") %>% 
  pull(ANNUAL)

#Perform basic statistical analysis for both regions
summary(arunachal_pradesh)
summary(west_rajasthan)

#Plot histogram of both regions (to determine type of test)
x <- two_regions$ANNUAL
hist(x,
     breaks = 10,
     xlab = "ANNUAL RAINFALL",
     ylab = "Density",
     main = "Histogram of 1930s ANNUAL rainfall for AP & WR with Normal Curve",
     col = "steelblue",
     freq = FALSE) 

#Add normal curve overlay to histogram
curve(dnorm(x, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)
# Histogram for Arunachal Pradesh only (1930s)
ap <- arunachal_pradesh   # just to make the code shorter

hist(ap,
     breaks = 10,
     main = "Histogram of Annual Rainfall \n - Arunachal Pradesh (1930s)",
     xlab = "Annual Rainfall (mm)",
     ylab = "Density",
     col = "orange",
     freq = FALSE,
     xlim = c(3000, 6500)
     )

# Add a normal distribution curve for AP only
curve(dnorm(x, mean = mean(ap, na.rm = TRUE), sd = sd(ap, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)


#Run independent t-test
t.test(arunachal_pradesh, west_rajasthan, paired = FALSE)





