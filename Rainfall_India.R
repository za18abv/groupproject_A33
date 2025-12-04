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

sorted_decade_1930_table <- decade_1930_table %>% arrange(desc(MeanAnnualRainfall))

view(sorted_decade_1930_table) 

#Obtain the max and min annual rainfall regions for the 1930 decade
decade_1930_table$REGION[which.max(decade_1930_table$MeanAnnualRainfall)]

decade_1930_table$REGION[which.min(decade_1930_table$MeanAnnualRainfall)]

#Filter rainfall dataset to return data for the selected 2 regions (Coastal Karnataka and Andaman & Nicobar Islands) in 1930s
two_regions <- rainfall_india %>%
  filter(REGION %in% c("COASTAL KARNATAKA", "ANDAMAN & NICOBAR ISLANDS"), YEAR >= 1930,
         YEAR <= 1939)

#Extract the annual values for each group (For statistical testing)
coastal_karnataka <- two_regions %>% 
  filter(REGION == "COASTAL KARNATAKA") %>% 
  pull(ANNUAL)

andaman_nicobar_islands <- two_regions %>% 
  filter(REGION == "ANDAMAN & NICOBAR ISLANDS") %>% 
  pull(ANNUAL)

#Perform basic statistical analysis for both regions
ck <- coastal_karnataka
an <- andaman_nicobar_islands

summary(ck)
summary(an)

#Plot histogram of both regions (to determine type of test)
x <- two_regions$ANNUAL
hist(x,
     breaks = 10,
     xlab = "Annual Rainfall (mm)",
     ylab = "Density",
     main = "Histogram of 1930s ANNUAL rainfall for CK & AN with Normal Curve",
     col = "steelblue",
     freq = FALSE) 

#Add normal curve overlay to histogram
curve(dnorm(x, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)
# Histogram for Coastal Karnataka only (1930s)
ck <- coastal_karnataka   # just to make the code shorter

hist(ap,
     breaks = 10,
     main = "Histogram of Annual Rainfall \n - Coastal Karnataka (1930s)",
     xlab = "Annual Rainfall (mm)",
     ylab = "Density",
     col = "orange",
     freq = FALSE,
     xlim = c(3000, 6500)
     )

# Add a normal distribution curve for CK only
curve(dnorm(x, mean = mean(ck , na.rm = TRUE), sd = sd(ap, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)


hist(west_rajasthan, 
     main = "Histogram of Annual Rainfall \n - West Rajasthan (1930s)",
     breaks = 10, 
     freq = FALSE, 
     col = "steelblue",
     xlab = "Annual Rainfall (mm)",
     ylab = "Density",
     xlim = c(100, 500)
     )

# Add a normal distribution curve for WR only
curve(
  dnorm(x,
        mean = mean(west_rajasthan, na.rm = TRUE),
        sd   = sd(west_rajasthan, na.rm = TRUE)),
  add = TRUE,
  col = "red",
  lwd = 2
)


#Run independent t-test
t.test(ck, an, paired = FALSE)

# Boxplot comparing the two regions 
boxplot(ANNUAL ~ REGION,
        data = two_regions,
        main = "Boxplot of Annual Rainfall (1930s)\nArunachal Pradesh vs West Rajasthan",
        xlab = "Region",
        ylab = "Annual Rainfall (mm)",
        col = c("skyblue", "lightgreen"))

axis(side = 2, at = seq(0, 6000, by = 500))




