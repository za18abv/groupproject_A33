# Import the Dataset
library(readr)
rainfall_india <- read_csv("rainfall_india.csv")
View(rainfall_india)

print(colnames(rainfall_india))

# printing first 5 rows
print(head(rainfall_india, 5))

#Creating Decades using the years
rainfall_india$Decade <- paste0((rainfall_india$YEAR %/% 10) * 10, "s")
