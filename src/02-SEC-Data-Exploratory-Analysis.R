# -----------------------------------------------------------------------------
# Name: Will Palmquist
# Date: 06/13/2024
# Title: Analysis of Auto Loan Asset-Backed Securities
# -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(forecast)

######################
##### Load and Prep Data
######################
abs_ee_data <- readRDS("data/abs_ee_data_all.rds")

abs_ee_data_clean <- abs_ee_data |>
  dplyr::mutate(
    dplyr::across(c(reportingPeriodBeginningDate,reportingPeriodEndingDate,interestPaidThroughDate),
                  ~ as.Date(.x, format = "%m-%d-%Y")),
    dplyr::across(c(contains("amount"), contains("percentage"), remainingTermToMaturityNumber, obligorCreditScore),
                  ~ as.numeric(.x))
  ) |> 
  dplyr::group_by(assetNumber) |> 
  dplyr::mutate(
    prepaidFlag = ifelse(zeroBalanceCode == 1 & remainingTermToMaturityNumber > 1, 1,0), 
    prepaidFlagFirst = ifelse(zeroBalanceCode == 1 & dplyr::lead(is.na(zeroBalanceCode) & remainingTermToMaturityNumber >1), 1,0)) |> 
  dplyr::ungroup()

# Grouped Dataframe by date for time series Analysis
abs_ts_sums <- abs_ee_data_clean |> 
  dplyr::group_by(reportingPeriodEndingDate) |> 
  dplyr::summarise(
    dplyr::across(c(contains("amount"), contains("percentage"), remainingTermToMaturityNumber, obligorCreditScore),
                  ~ sum(.x, na.rm = T), .names = "{.col}_sum"),
    dplyr::across(c(contains("amount"), contains("percentage"), remainingTermToMaturityNumber, obligorCreditScore, -contains("sum")),
                  ~ mean(.x, na.rm = T), .names = "{.col}_mean"),
    numberActiveLoans = sum(remainingTermToMaturityNumber > 0 & is.na(zeroBalanceCode), na.rm = T),
    numberNewPrepaidLoans = sum(prepaidFlagFirst,na.rm = T),
    numberPrepaidLoansTotal = sum(prepaidFlag, na.rm = T),
    defaults = sum(zeroBalanceCode == 4, na.rm = T),
    prepayments = sum(zeroBalanceCode == 1, na.rm = T)
  ) |> 
  dplyr::ungroup()

abs_ts_means <- abs_ee_data_clean |> 
  dplyr::group_by(reportingPeriodEndingDate) |> 
  dplyr::summarise(
    dplyr::across(c(contains("amount"), contains("percentage"), remainingTermToMaturityNumber, obligorCreditScore),
                  ~ sum(.x, na.rm = T), .names = "{.col}_sum"),
    dplyr::across(c(contains("amount"), contains("percentage"), remainingTermToMaturityNumber, obligorCreditScore, -contains("sum")),
                  ~ mean(.x, na.rm = T), .names = "{.col}_mean"),
    numberActiveLoans = sum(remainingTermToMaturityNumber > 0 & is.na(zeroBalanceCode), na.rm = T),
    numberNewPrepaidLoans = sum(prepaidFlagFirst,na.rm = T),
    numberPrepaidLoansTotal = sum(prepaidFlag, na.rm = T),
    defaults = sum(zeroBalanceCode == 4, na.rm = T),
    prepayments = sum(zeroBalanceCode == 1, na.rm = T)
  ) |> 
  dplyr::ungroup()

# grouped dataframe by asset for obligor credit risk
abs_ee_data_asset_group <- abs_ee_data_clean |> 
  dplyr::group_by(assetNumber) |> 
  dplyr::summarise(
    creditScore = dplyr::first(obligorCreditScore),
    employmentVerification = dplyr::first(obligorEmploymentVerificationCode),
    incomeVerification = dplyr::first(obligorIncomeVerificationLevelCode),
    totalChargeOffPrincipal = sum(chargedoffPrincipalAmount, na.rm = T),
    prepaid = sum(prepaidFlag, na.rm = T),
    paymentToIncome = first(paymentToIncomePercentage),
    zeroBalanceCode = max(zeroBalanceCode, na.rm = T)
  )

########################
## Part A. 
#######################

### simple correlation
corr <- abs_ee_data_clean |> 
  dplyr::select_if(is.numeric) |> 
  cor()

ggcorrplot::ggcorrplot(corr, method = "circle", ,type = "lower")

# payment to income
ggplot2::ggplot(abs_ee_data_asset_group, ggplot2::aes(x = paymentToIncome,y = totalChargeOffPrincipal)) +
  ggplot2::geom_col()

# credit score x charged off principal 
ggplot2::ggplot(abs_ee_data_asset_group |> dplyr::filter(creditScore > 0), ggplot2::aes(x = creditScore,y = totalChargeOffPrincipal)) +
  ggplot2::geom_col()

# Create bins for credit scores
bins <- seq(400, 900,by=50)

# Bin the data and calculate average charged-off principal for each bin
binned_data <- abs_ee_data_asset_group |> 
  dplyr::filter(creditScore > 0, !is.na(creditScore))  |> 
  dplyr::mutate(creditScoreBin = cut(creditScore, breaks = bins, include.lowest = FALSE)) |>
  dplyr::group_by(creditScoreBin) |>
  dplyr::summarise(avgChargeOffPrincipal = mean(totalChargeOffPrincipal, na.rm = TRUE))

# Plot the results
ggplot2::ggplot(binned_data, ggplot2::aes(x = creditScoreBin, y = avgChargeOffPrincipal)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Average Charged-Off Principal by Credit Score Bin",
    x = "Credit Score Bin",
    y = "Average Charged-Off Principal"
  ) +
  ggplot2::theme_minimal()


# Bin the data and calculate average charged-off principal for each bin
employment_verification <- abs_ee_data_asset_group |> 
  dplyr::group_by(employmentVerification = ifelse(employmentVerification == 3, "Verified","Not Verified")) |>
  dplyr::summarise(avgChargeOffPrincipal = avg(totalChargeOffPrincipal, na.rm = TRUE))

# Plot the results
ggplot2::ggplot(employment_verification, ggplot2::aes(x = employmentVerification, y = avgChargeOffPrincipal)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Average Charged-Off Principal by Employment Verification",
    x = "Employment Verification",
    y = "Average Charged-Off Principal"
  ) +
  ggplot2::theme_minimal()

# Calculate cumulative payments
df <- abs_ee_data_clean |>
  group_by(remainingTermToMaturity = as.numeric(remainingTermToMaturityNumber)) |>
  summarise(
    actualPrincipalCollectedAmount = mean(actualPrincipalCollectedAmount, na.rm = T),
    scheduledPrincipalAmount = mean(scheduledPrincipalAmount, na.rm = T)
  ) 

# Plot cumulative payments over time
ggplot() +
  geom_line(df, mapping = aes(x = remainingTermToMaturity, y = scheduledPrincipalAmount),colour = "red") +
  geom_line(df, mapping = aes(x = remainingTermToMaturity, y = actualPrincipalCollectedAmount),colour = "blue") +
  scale_x_reverse()+
  labs(title = "Actual vs. Scheduled Payments - Remaining Term to Maturity",
       x = "Remaining Term to Maturity",
       y = "Actual-Scheduled Payments") +
  theme_minimal()

########################
## Part B. 
#######################

# cumulative charged off principal
cum_charged_off_principal <- sum(as.numeric(abs_ee_data$chargedoffPrincipalAmount), na.rm = T)
cum_charged_off_principal
#### Forecasting
charged_off_principal_ts <- abs_ee_data_grouped |> 
  dplyr::select(reportingPeriodEndingDate,chargedoffPrincipalAmount_sum,numberActiveLoans,numberNewPrepaidLoans,numberPrepaidLoansTotal) |> 
  dplyr::mutate(
    chargedoffPrincipalAmountCumulative = cumsum(chargedoffPrincipalAmount_sum)
  )

charged_off_principal_ts_long <- charged_off_principal_ts |> 
  tidyr::pivot_longer(cols = -reportingPeriodEndingDate)

ggplot2::ggplot(charged_off_principal_ts_long, ggplot2::aes(reportingPeriodEndingDate,value)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ name, scales = "free_y", ncol = 2) +
  ggplot2::theme_bw()

ggplot2::ggplot(charged_off_principal_ts_long |> dplyr::filter(name == "numberActiveLoans"), ggplot2::aes(reportingPeriodEndingDate,value)) +
  ggplot2::geom_line() +
  ggplot2::ylim(c(0,35000))+
  # ggplot2::facet_wrap(~ name, scales = "free_y", ncol = 2) +
  ggplot2::theme_bw()

### Model Fitting
ts_data <- ts(charged_off_principal_ts$chargedoffPrincipalAmountCumulative, frequency = 19)

# ARIMA
fit_arima <- auto.arima(ts_data)
forecast_arima <- forecast(fit_arima, h = 17)
autoplot(forecast_arima) + ggtitle("ARIMA Forecast of Cumulative Charged-Off Principal")
checkresiduals(fit_arima)
# ETS
fit_ets <- ets(ts_data)
forecast_ets <- forecast(fit_ets, h = 17)
autoplot(forecast_ets) + ggtitle("ETS Forecast of Cumulative Charged-Off Principal")
checkresiduals(fit_ets)

#Comparison
errors_arima <- tsCV(ts_data, forecastfunction = function(x, h) forecast(auto.arima(x), h = h), h = 17)
mae_arima <- mean(abs(errors_arima), na.rm = TRUE)
mse_arima <- mean(errors_arima^2, na.rm = TRUE)

# Cross-validation for ETS
errors_ets <- tsCV(ts_data, forecastfunction = function(x, h) forecast(ets(x), h = h), h = 17)
mae_ets <- mean(abs(errors_ets), na.rm = TRUE)
mse_ets <- mean(errors_ets^2, na.rm = TRUE)

# Compare MAE and MSE
data.frame(
  Model = c("ARIMA", "ETS"),
  MAE = c(mae_arima, mae_ets),
  MSE = c(mse_arima, mse_ets)
)

########################
## Part C.
#######################
abs_april_2024 <- abs_ee_data_clean |>
  dplyr::filter(reportingPeriodEndingDate == "2024-04-30", vehicleValueSourceCode == 3) |>
  dplyr::group_by(vehicleManufacturerName,vehicleModelName) |>
  dplyr::summarise(
    records = dplyr::n(),
    stdev = sd(vehicleValueAmount, na.rm = T)
  ) |>
  dplyr::filter(records > 30)

max_stdev_vehicle_value <- abs_april_2024 |>
  dplyr::ungroup() |>
  dplyr::slice_max(order_by = stdev,n = 1)
