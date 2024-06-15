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
abs_ts_sums <- abs_ee_data_clean |> 
  dplyr::group_by(reportingPeriodEndingDate) |> 
  dplyr::summarise(
    dplyr::across(c(contains("amount"), contains("percentage"), remainingTermToMaturityNumber, obligorCreditScore),
                  ~ sum(.x, na.rm = T)),
    numberActiveLoans = sum(remainingTermToMaturityNumber > 0 & is.na(zeroBalanceCode), na.rm = T),
    numberNewPrepaidLoans = sum(prepaidFlagFirst,na.rm = T),
    numberPrepaidLoansTotal = sum(prepaidFlag, na.rm = T),
    defaults = sum(zeroBalanceCode == 4, na.rm = T),
    prepayments = sum(zeroBalanceCode == 1, na.rm = T)
  ) |> 
  dplyr::ungroup()

#####################
### Method 1
#####################
# cumulative charged off principal
cum_charged_off_principal <- sum(as.numeric(abs_ee_data$chargedoffPrincipalAmount), na.rm = T)
cum_charged_off_principal
#### Forecasting
charged_off_principal_ts <- abs_ts_sums |> 
  dplyr::select(reportingPeriodEndingDate,chargedoffPrincipalAmount,numberActiveLoans,numberNewPrepaidLoans,numberPrepaidLoansTotal) |> 
  dplyr::mutate(
    chargedoffPrincipalAmountCumulative = cumsum(chargedoffPrincipalAmount)
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
df_chargeoffs <- abs_ts_sums |> 
  dplyr::select(reportingPeriodEndingDate, chargedoffPrincipalAmount, reportingPeriodActualEndBalanceAmount) |> 
  dplyr::mutate(
    chargeOffPercentOfBalance = chargedoffPrincipalAmount/reportingPeriodActualEndBalanceAmount,
    EndBalanceAmountYOY = (reportingPeriodActualEndBalanceAmount/lag(reportingPeriodActualEndBalanceAmount))-1,
    Period = "Actual"
    )

stableBalanceGrowthRate = mean(df_chargeoffs |> tail(12) |> dplyr::pull(EndBalanceAmountYOY),na.rm =T)
stableChargeOffPercent = mean(df_chargeoffs |> tail(12) |> dplyr::pull(chargeOffPercentOfBalance),na.rm =T)

last_balance <- df_chargeoffs |> pull(reportingPeriodActualEndBalanceAmount) |> tail(1)

df_forecast <- data.frame(
  reportingPeriodEndingDate = seq.Date(from = max(df_chargeoffs$reportingPeriodEndingDate) %m+% months(1), by = "month",length.out = 17),
  reportingPeriodActualEndBalanceAmount = last_balance *cumprod(rep(1+stableBalanceGrowthRate,17))
) |> 
  dplyr::mutate(
    chargedoffPrincipalAmount = reportingPeriodActualEndBalanceAmount * stableChargeOffPercent,
    Period = "Forecast"
  )

combined_forecast <- df_chargeoffs |> 
  dplyr::bind_rows(df_forecast) |> 
  dplyr::mutate(
    cumulativeSumChargeOffs = cumsum(chargedoffPrincipalAmount)
  )

CumulativeChargeOffsMonth19 <- sum(combined_forecast$chargedoffPrincipalAmount[combined_forecast$Period == "Actual"], na.rm = T)
CumulativeChargeOffsMonth36 <- sum(combined_forecast$chargedoffPrincipalAmount, na.rm = T)

ggplot2::ggplot(combined_forecast) +
  ggplot2::geom_line(ggplot2::aes(x = reportingPeriodEndingDate, y = chargedoffPrincipalAmount, color = Period), size = 1) +
  ggplot2::theme_minimal()

ggplot2::ggplot(combined_forecast) +
  ggplot2::geom_line(ggplot2::aes(x = reportingPeriodEndingDate, y = reportingPeriodActualEndBalanceAmount, color = Period), size = 1) +
  ggplot2::theme_minimal()

ggplot(combined_forecast) +
  ggplot2::geom_line(ggplot2::aes(x = reportingPeriodEndingDate, y = cumulativeSumChargeOffs, color = Period), size = 1) +
  ggplot2::theme_minimal()



