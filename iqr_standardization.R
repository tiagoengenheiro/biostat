iqr_standardization <- function(df, col_name) {
  # Calculate Q1 (25th percentile) and Q3 (75th percentile)
  q1 <- quantile(df[[col_name]], 0.25, na.rm = TRUE)
  q3 <- quantile(df[[col_name]], 0.75, na.rm = TRUE)
  
  # Calculate IQR
  iqr <- q3 - q1
  
  # Apply IQR standardization to each data point in the column
  df[[col_name]] <- (df[[col_name]] - q1) / iqr
  
  return(df)
}

# Load the dataset
df_final <- read.csv("data/df_final.csv")

#Summary before applying IQR standardization
summary(df_final$TotalCoffeeIntake)

# Apply IQR standardization to the Coffee intake column
df_final <- iqr_standardization(df_final, "TotalCoffeeIntake")

#Summary after applying IQR standardization
summary(df_final$TotalCoffeeIntake)
