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

#Plot histogram of both regions (to determine type of test) & Scale the data by dividing by 1000
x <- two_regions$ANNUAL / 1000

hist(x,
     breaks = 10,
     xlab = "Annual Rainfall (mm × 1000)",
     ylab = "Density (per thousand mm)",  # More descriptive
     main = "Histogram of 1930s Annual Rainfall for\nCoastal Karnataka and Andaman & Nicobar Islands",
     col = "steelblue",
     border = "white",
     freq = FALSE,
     las = 1,
     xlim = c(2, 4.5),
     ylim = c(0, 1.2),
     axes = FALSE)

# Custom x-axis with better spacing
axis(side = 1, 
     at = seq(2, 4.5, by = 0.5),
     labels = seq(2, 4.5, by = 0.5),
     tcl = -0.3)

# Custom y-axis
axis(side = 2, 
     at = seq(0, 1.2, by = 0.2),
     labels = seq(0, 1.2, by = 0.2),
     las = 1,
     tcl = -0.3)

# Add minor gridlines
abline(v = seq(2, 4.5, by = 0.25), col = "gray95", lty = 1)
abline(h = seq(0, 1.2, by = 0.1), col = "gray95", lty = 1)

# Re-plot histogram on top of grid
hist(x,
     breaks = 10,
     col = "steelblue",
     border = "white",
     freq = FALSE,
     add = TRUE)

# Add a text annotation explaining the scaling
mtext("Note: Rainfall values scaled by 1/1000 for clarity", 
      side = 1, 
      line = 4, 
      cex = 0.8, 
      col = "gray40")

#Add normal curve overlay to histogram
curve(dnorm(x, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)),
      add = TRUE,
      col = "red",
      lwd = 2)


#Run independent t-test
t.test(ck, an, paired = FALSE)

# Boxplot f Annual Rainfall (1930s)\nAndaman & Nicobar vs Coastal Karnataka
boxplot(ANNUAL ~ REGION,
        data = two_regions,
        main = "Boxplot of Annual Rainfall (1930s)\nAndaman & Nicobar vs Coastal Karnataka",
        xlab = "Region",
        ylab = "Annual Rainfall (mm x 1000)",
        col = c("skyblue", "lightgreen"),
        ylim = c(2000, 4500),
        yaxt = "n")  

axis(side = 2,
     at = seq(2000, 4500, by = 500),
     labels = seq(2, 4.5, by = 0.5))

