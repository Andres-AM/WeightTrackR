

used_date <- "2023-11-29"
test <- date(used_date)+days(8- wday(used_date))

wday(test)


ymd(used_date)+days(8- wday(used_date)
)
                    


# Create a sample data frame
df <- data.frame(Column1 = numeric(10))  # Create an empty column with 10 rows

# Create a sequence of numbers starting from 1 and incrementing by 0.5
df$Column1 <- seq(2, length.out = nrow(df), by = 0.5)

# Print the updated data frame
print(df)
