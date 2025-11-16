print(colnames(rainfall_india))

#Creating Decades using the years
rainfall_india$Decade <- paste0((rainfall_india$YEAR %/% 10) * 10, "s")

# printing first 5 rows
print(head(rainfall_india, 5))

