# Load necessary libraries
library(readxl)
library(tseries)
library(rugarch)
library(bizdays)
library(FinTS)
library(ggplot2)

# Specify the file path
file_path <- "nasdaq_data.csv"

# Read the Excel file (assuming the sheet name is 'Sheet1' and relevant data is in a column named 'Close')
nd_data <- read.csv(file_path)

# View the first few rows of the data
head(nd_data)

# Calculate returns (log returns or percentage returns)
nd_data$Returns <- c(NA, diff(log(nd_data$Close)) * 100)

# Remove the first row with NA value
nd_data <- nd_data[-1, ]

# Check for ARCH effects
arch_test <- ArchTest(nd_data$Returns, lags = 12)
print(arch_test)

# Fit a GARCH(1,1) model
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0))
)
fit <- ugarchfit(spec = spec, data = nd_data$Returns, solver = "hybrid")

# Print the fit summary
print(fit)

# Plot the fitted GARCH model results
plot(fit, which = 1)  # Standardized Residuals
plot(fit, which = 2)  # Conditional Sigma (Volatility)
plot(fit, which = 3)  # QQ Plot of Standardized Residuals
plot(fit, which = 4)  # ACF of Standardized Residuals
plot(fit, which = 5)  # ACF of Squared Standardized Residuals

# Forecast future volatility for the next three months (assuming 63 trading days in 3 months)
forecast <- ugarchforecast(fit, n.ahead = 63)

# Print the forecast
print(forecast)

# Extract the forecasted sigma values
forecasted_sigma <- sigma(forecast)

# Create a plot for the forecasted volatility
plot(forecasted_sigma, type = "l", main = "Forecasted Volatility data", xlab = "Days Ahead", ylab = "Sigma")

#PART B

# Load necessary libraries
library(readxl)
library(tseries)
library(vars)
library(urca)
library(ggplot2)
library(reshape2)

# Step 1: Load the data from the Excel file
file_path <- 'CMO-Historical-Data-Annual.xlsx'
df <- read_excel(file_path, sheet = 'Annual Prices (Nominal)')

# Drop all columns except for the required ones
columns_to_keep <- c('Date', 'Sugar, EU', 'Sugar, US', 'Sugar, world')
df <- df[, columns_to_keep]

# Filter the data from 2003 to 2023
df <- df[df$Date >= 2003 & df$Date <= 2023,]

# Save the filtered data to a CSV file
write.csv(df, 'Sugar_commodity_prices.csv', row.names = FALSE)

# Step 2: Reshape the data for plotting
df_melt <- melt(df, id.vars = 'Date')

# Print the first few rows to inspect the melted data
print(head(df_melt))

# Convert Date column to numeric
df$Date <- as.numeric(df$Date)

# Convert other columns to numeric
df[, -1] <- lapply(df[, -1], as.numeric)

# Print the first few rows to inspect the data
print(head(df))

# Drop rows with any missing values
df <- na.omit(df)


# Reshape the data for plotting
df_melt <- melt(df, id.vars = 'Date')

# Plot the data
ggplot(df_melt, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  labs(title = "Commodity Prices", x = "Year", y = "Price") +
  theme_minimal()

# Step 4: Check for stationarity using ADF test
adf_test <- function(series, title='') {
  series <- na.omit(series)  # Remove NA values
  cat('Augmented Dickey-Fuller Test:', title, '\n')
  adf_result <- adf.test(series, alternative = "stationary")
  print(adf_result)
  cat('\n')
}

# Apply ADF test for each commodity
for (column in colnames(df)[-1]) {
  adf_test(df[[column]], title = column)
}

# Step 5: Differencing the series if not stationary
df_diff <- diff(as.matrix(df[, -1]), differences = 1)
df_diff <- na.omit(data.frame(Date = df$Date[-1], df_diff))

# Check stationarity of differenced data
for (column in colnames(df_diff)[-1]) {
  adf_test(df_diff[[column]], title = paste(column, "Differenced"))
}

# Step 6: Fit VAR model if series are stationary
# Automatically select the optimal lag length based on information criteria
var_model <- VAR(df_diff[, -1], lag.max = 10, ic = "AIC")
lag_order <- var_model$p
cat('Selected Lag Length:', lag_order, '\n')

# Fit the VAR model with the selected lag length
var_fit <- VAR(df_diff[, -1], p = lag_order)
summary(var_fit)

# Step 7: Fit VECM model if series are non-stationary but cointegrated
johansen_test <- ca.jo(df[, -1], type = "trace", ecdet = "none", K = 2)
summary(johansen_test)

# Fit VECM model if cointegrated
vecm_fit <- cajorls(johansen_test, r = 1)
summary(vecm_fit)          
                 
                 