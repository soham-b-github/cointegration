root = "/home/soham/Documents/Soham-Bhattacharya/Academic-Diaries/Post Graduation RKMVERI/Course Projects/MVS__Cointegration/notes"

# ----------------------------
# Stationary Time Series in R
# ----------------------------

# (1) Load libraries
library(tseries)   # for adf.test
library(forecast)  # for visualization

# (2) Generate a stationary time series
set.seed(123)
ts_stationary <- arima.sim(model = list(ar = 0.6), n = 200)  # AR(1) with |phi|<1 ensures stationarity

# (3) Plot the time series
filename = "stationary_ts__plot.png"
filepath <- file.path(root, filename)
plot(ts_stationary,
     main = "Stationary Time Series (AR(1) Process)",
     ylab = "Value",
     xlab = "Time")
# dev.off()

# (4) ACF and PACF plots
acf(ts_stationary, main = "ACF of Stationary Series")
pacf(ts_stationary, main = "PACF of Stationary Series")

# (5) Augmented Dickey-Fuller (ADF) Test for stationarity
adf_test_result <- adf.test(ts_stationary)
print(adf_test_result)

# (6) Compare with a non-stationary series
ts_nonstationary <- cumsum(rnorm(200))  # Random walk
plot(ts_nonstationary,
     main = "Non-Stationary Series (Random Walk)",
     ylab = "Value",
     xlab = "Time")

# (7) ADF test for non-stationary series
adf_test_nonstationary <- adf.test(ts_nonstationary)
print(adf_test_nonstationary)


############################################
# 1. Stationarity Illustration
############################################

# Load a built-in dataset
data(LakeHuron)     # Annual lake levels (1875–1972)

filename = "LakeHuron__plot.png"
filepath <- file.path(root, filename)
png(filepath, width = 800, height = 600)
plot(LakeHuron, main = "Lake Huron Levels", ylab = "Level (ft)")
dev.off()


# Check autocorrelation and partial autocorrelation

filename = "LakeHuron_ACF__plot.png"
filepath <- file.path(root, filename)
png(filepath, width = 800, height = 600)
acf(LakeHuron, main = "ACF of Lake Huron")
dev.off()

filename = "LakeHuron_PACF__plot.png"
filepath <- file.path(root, filename)
png(filepath, width = 800, height = 600)
pacf(LakeHuron, main = "PACF of Lake Huron")
dev.off()

# Apply Augmented Dickey-Fuller test (ADF)
library(tseries)
adf.test(LakeHuron)  # H0: unit root (nonstationary)

############################################
# 2. Unit Root Testing
############################################

# Example: AirPassengers (monthly, 1949–1960)
data(AirPassengers)

filename = "AirPassengers__plot.png"
filepath <- file.path(root, filename)
png(filepath, width = 800, height = 600)
plot(AirPassengers, main = "AirPassengers", ylab = "Thousands")
dev.off()

# Log and difference to induce stationarity
AP_log_diff <- diff(log(AirPassengers))

filename = "AirPassengers__diffLog.png"
filepath <- file.path(root, filename)
png(filepath, width = 800, height = 600)
plot(AP_log_diff, main = "Differenced log(AirPassengers)")
dev.off()

# Test for unit root
adf.test(log(AirPassengers))  # likely nonstationary
adf.test(AP_log_diff)         # should be stationary

############################################
# 3. Cointegration Analysis
############################################

library(urca)
data(Canada)   # from 'urca' package: interest rate, inflation, unemployment
head(Canada)

# Johansen cointegration test
cajo_test <- ca.jo(Canada, type = "trace", ecdet = "const", K = 2)
summary(cajo_test)

# Engle-Granger (two-step) example
library(dynlm)
coint_reg <- lm(e ~ prod, data = as.data.frame(Canada))
summary(coint_reg)
residuals <- resid(coint_reg)
adf.test(residuals)   # Test residuals for stationarity

############################################
# 4. Multivariate Time Series & VAR Modeling
############################################

library(vars)
data(EuStockMarkets)  # 4 European stock indices

filename = "EuropeanStockMarkets__plot.png"
filepath <- file.path(root, filename)
png(filepath, width = 800, height = 600)
plot(EuStockMarkets, main = "European Stock Markets")
dev.off()

# Select lag length for VAR
VARselect(EuStockMarkets, lag.max = 10, type = "const")

# Fit VAR(2)
var_model <- VAR(EuStockMarkets, p = 2, type = "const")
summary(var_model)

# Impulse Response Functions
filename = "EuropeanStockMarkets_irf__plot.png"
filepath <- file.path(root, filename)
png(filepath, width = 800, height = 600)
irf_plot <- irf(var_model, impulse = "DAX", response = "CAC", n.ahead = 10, boot = TRUE)
plot(irf_plot)
dev.off()

############################################
# 5. Optional: Cointegration with 'denmark' dataset
############################################

data(denmark)
summary(ca.jo(denmark, type = "trace", ecdet = "const", K = 2))
