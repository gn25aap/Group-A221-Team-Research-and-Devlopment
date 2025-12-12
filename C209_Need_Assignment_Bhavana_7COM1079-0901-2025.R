# Load necessary libraries
library(readr)
library(dplyr)

# Load the dataset 
car_data <- read_csv("D:/December/11.12.2025/car_prices_jordan.csv")

# View first few rows
head(car_data)

# Check structure of the dataset
str(car_data)


# Clean Power column (remove 'CC' and convert to numeric)
car_data$Power_clean <- gsub(" CC", "", car_data$Power)
car_data$Power_clean <- as.numeric(car_data$Power_clean)
head(car_data$Power_clean)


# Clean Price column (convert to numeric explicitly)
car_data$Price_clean <- as.numeric(car_data$Price)
str(car_data$Price_clean)
car_data <- na.omit(car_data)


# Check missing values in each column
colSums(is.na(car_data))

# Or full dataset check
sum(is.na(car_data))


summary(car_data)

summary(car_data$Power_clean)
summary(car_data$Price_clean)


car_data %>%
  summarise(
    mean_power = mean(Power_clean),
    median_power = median(Power_clean),
    sd_power = sd(Power_clean),
    mean_price = mean(Price_clean),
    median_price = median(Price_clean),
    sd_price = sd(Price_clean)
  )


# Histogram of Price
hist(car_data$Price_clean,
     main = "Histogram of Car Prices",
     xlab = "Price (JOD)",
     ylab = "Frequency")

# Histogram of Engine Power
hist(car_data$Power_clean,
     main = "Histogram of Engine Power (CC)",
     xlab = "Engine Power (CC)",
     ylab = "Frequency")

# Scatter plot of Power vs Price
plot(car_data$Power_clean, car_data$Price_clean,
     main = "Scatter Plot: Engine Power vs Car Price",
     xlab = "Engine Power (CC)",
     ylab = "Price (JOD)")


# Pearson Correlation Test
cor_test_result <- cor.test(car_data$Power_clean, car_data$Price_clean, method = "pearson")
cor_test_result


p_value <- cor_test_result$p.value

if (p_value < 0.05) {
  cat("Decision: Reject H0. There IS a statistically significant relationship between engine power and car price.\n")
} else {
  cat("Decision: Fail to reject H0. There is NO statistically significant relationship between engine power and car price.\n")
}


# Linear regression: Price predicted by Power
model <- lm(Price_clean ~ Power_clean, data = car_data)
summary(model)

anova(model)


plot(car_data$Power_clean, car_data$Price_clean,
     main = "Engine Power vs Price (with Regression Line)",
     xlab = "Engine Power (CC)",
     ylab = "Price (JOD)")

abline(model, col = "blue", lwd = 2)


# Boxplot by transmission type
boxplot(Price_clean ~ Property, data = car_data,
        main = "Car Price by Transmission Type",
        xlab = "Transmission Type",
        ylab = "Price (JOD)")


# Correlation matrix (only numeric columns)
numeric_data <- car_data[, c("Power_clean", "Price_clean")]
cor(numeric_data)


plot(car_data$Power_clean, car_data$Price_clean,
     main = "Check for Linearity",
     xlab = "Power (CC)",
     ylab = "Price (JOD)")

