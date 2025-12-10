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
h <- hist(x, breaks = 10, plot = FALSE)

hist(x,
     breaks = 10,
     freq = TRUE,
     xlab = "Annual Rainfall (mm × 1000)",
     ylab = "Frequency (in thousands)",  # More descriptive
     main = "Histogram of 1930s Annual Rainfall for\nCoastal Karnataka and Andaman & Nicobar Islands",
     col = "steelblue",
     border = "white",
     las = 1,
     xlim = c(2, 4.5),
     ylim = c(0, max(h$counts) * 1.1),
     axes = FALSE)

#Add normal curve overlay to histogram
xfit <- seq(min(h$breaks), max(h$breaks), length = 200)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
bin_width <- h$breaks[2] - h$breaks[1]
yfit <- yfit * length(x) * bin_width

lines(xfit, yfit, col = "red", lwd = 2)

# Custom x-axis with better spacing
axis(side = 1, 
     at = seq(2, 4.5, by = 0.5),
     tcl = -0.3)

# Custom y-axis
y_at <- seq(0, max(h$counts)*1.1, by = 1)
axis(side = 2, at = y_at, labels = y_at, las = 1, tcl = -0.3)

# Add a text annotation explaining the scaling
mtext("Note: Rainfall values scaled by 1/1000 for clarity", 
      side = 1, 
      line = 4, 
      cex = 0.8, 
      col = "gray40")

#Save image to png file
dev.copy(png, 
         filename = "annualRainfall_histogram_plot_CK&AM.png",
         width = 800,                         
         height = 600,                    
         res = 120) 

#close device
dev.off()

#Run independent t-test for the regions
t.test(ck, an, paired = FALSE)

# Boxplot of Annual Rainfall (1930s)\nAndaman & Nicobar vs Coastal Karnataka

png("Boxplot.png", width = 1200, height = 900, res = 150)

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

dev.off()

