# -----------------------------------------------------------------------------
# Name: Will Palmquist
# Date: 06/13/2024
# Title: Forecsting Cumulative Charge-Offs
# -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(forecast)


############################################
# !!!Load data in 02-Exploratory-Analysis.R
###########################################

#####################
### Method 1
#####################
# cumulative charged off principal
cum_charged_off_principal <- sum(as.numeric(abs_ee_data$chargedoffPrincipalAmount), na.rm = T)
cum_charged_off_principal
#### Forecasting
charged_off_principal_ts <- abs_ee_data_grouped |> 
  dplyr::select(reportingPeriodEndingDate,chargedoffPrincipalAmount_sum,numberActiveLoans,numberNewPrepaidLoans,numberPrepaidLoansTotal) |> 
  dplyr::mutate(
    chargedoffPrincipalAmountCumulative = cumsum(chargedoffPrincipalAmount_sum)
  )

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


#####################
### Method 2
#####################
df_chargeoffs <- abs_ee_data_grouped |> 
  dplyr::select(reportingPeriodEndingDate, chargedoffPrincipalAmount_sum, reportingPeriodActualEndBalanceAmount_sum) |> 
  dplyr::mutate(
    chargeOffPercentOfBalance = chargedoffPrincipalAmount_sum/reportingPeriodActualEndBalanceAmount_sum,
    EndBalanceAmountYOY = (reportingPeriodActualEndBalanceAmount_sum/lag(reportingPeriodActualEndBalanceAmount_sum))-1,
    period = "Actual"
    )

stableBalanceGrowthRate = mean(df_chargeoffs |> tail(12) |> dplyr::pull(EndBalanceAmountYOY),na.rm =T)
stableChargeOffPercent = mean(df_chargeoffs |> tail(12) |> dplyr::pull(chargeOffPercentOfBalance),na.rm =T)

last_balance <- df_chargeoffs |> pull(reportingPeriodActualEndBalanceAmount_sum) |> tail(1)

df_forecast <- data.frame(
  reportingPeriodEndingDate = seq.Date(from = max(df_chargeoffs$reportingPeriodEndingDate) %m+% months(1), by = "month",length.out = 17),
  reportingPeriodActualEndBalanceAmount_sum = last_balance *cumprod(rep(1+stableBalanceGrowthRate,17))
) |> 
  dplyr::mutate(
    chargedoffPrincipalAmount_sum = reportingPeriodActualEndBalanceAmount_sum * stableChargeOffPercent,
    period = "Forecast"
  )

combined_forecast <- df_chargeoffs |> 
  dplyr::bind_rows(df_forecast) |> 
  dplyr::mutate(
    cumulativeSumChargeOffs = cumsum(chargedoffPrincipalAmount_sum)
  )

CumulativeChargeOffsMonth19 <- sum(combined_forecast$chargedoffPrincipalAmount_sum[combined_forecast$period == "Actual"], na.rm = T)
CumulativeChargeOffsMonth36 <- sum(combined_forecast$chargedoffPrincipalAmount_sum, na.rm = T)

ggplot(combined_forecast) +
  # geom_line(aes(x = reportingPeriodEndingDate, y = reportingPeriodActualEndBalanceAmount_sum, colour = "Loan Balance Amount", color = period), size = 1)+
  geom_line(aes(x = reportingPeriodEndingDate, y = chargedoffPrincipalAmount_sum, colour = "chargedoffPrincipalAmount", color = period), size = 1)

ggplot(combined_forecast) +
  geom_line(aes(x = reportingPeriodEndingDate, y = reportingPeriodActualEndBalanceAmount_sum, colour = "Loan Balance Amount", color = period), size = 1)

ggplot(combined_forecast) +
  geom_line(aes(x = reportingPeriodEndingDate, y = cumulativeSumChargeOffs, colour = "Cumulative Charged Off Principal", color = period), size = 1)


