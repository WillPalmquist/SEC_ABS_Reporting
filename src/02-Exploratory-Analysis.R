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

###############################
## Asset/Obligor Level Analysis
###############################

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

# Create bins for credit scores
# Create bins for credit scores
bins <- seq(400, 900,by=50)

# Bin the data and calculate average charged-off principal for each bin
binned_data <- abs_ee_data_asset_group |> 
  dplyr::filter(creditScore > 0)  |> 
  dplyr::mutate(creditScoreBin = cut(creditScore, breaks = bins, include.lowest = FALSE)) |>
  dplyr::filter(!is.na(creditScoreBin)) |> 
  dplyr::group_by(creditScoreBin, incomeVerification) |>
  dplyr::summarise(
    TotalChargeOffPrincipal = mean(totalChargeOffPrincipal, na.rm = TRUE))

# Plot the results
ggplot2::ggplot(binned_data, ggplot2::aes(x = creditScoreBin, y = TotalChargeOffPrincipal, fill = incomeVerification)) +
  ggplot2::geom_col(stat = "identity",position = "stack") +
  ggplot2::labs(
    title = "Average Charged-Off Principal by Credit Score Bin",
    x = "Credit Score Bin",
    y = "Average Charged-Off Principal"
  ) +
  ggplot2::theme_minimal()

### Employment Verification
# Bin the data and calculate average charged-off principal for each bin
employment_verification <- abs_ee_data_asset_group |> 
  dplyr::group_by(employmentVerification = ifelse(employmentVerification == 3, "Verified","Not Verified")) |>
  dplyr::summarise(avgChargeOffPrincipal = sum(totalChargeOffPrincipal, na.rm = TRUE))

# Plot the results
ggplot2::ggplot(employment_verification, ggplot2::aes(x = employmentVerification, y = avgChargeOffPrincipal)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Average Charged-Off Principal by Employment Verification",
    x = "Employment Verification",
    y = "Average Charged-Off Principal"
  ) +
  ggplot2::theme_minimal()

#######################
## Time Series Analysis
#######################

# ZeroBalance Breakdown 
abs_ts_zero_balance <- abs_ee_data_clean |> 
  dplyr::group_by(reportingPeriodEndingDate, zeroBalanceCode) |> 
  dplyr::summarise(
    n = n()
  ) |> 
  dplyr::mutate(
    zeroBalanceCode = dplyr::case_match(
      as.numeric(zeroBalanceCode),
      1 ~ "Prepaid or Matured",
      2 ~ "Third-Party Sale",
      3 ~ "Repurchased or Replaced",
      4 ~ "Charged-Off",
      NA ~ "Active")
    )

abs_ts_zero_balance |> 
  ggplot2::ggplot(ggplot2::aes(x = reportingPeriodEndingDate, y = n, fill = zeroBalanceCode)) + 
  ggplot2::geom_bar(stat = "identity",position = "fill") +
  ggplot2::labs(
    title = "ABS Loan Status Composition Since Inception",
    x = "Reporting Period End Date",
    y = "Percent "
  ) +
  ggplot2::theme_minimal()

abs_ts_zero_balance |> 
  ggplot2::ggplot(ggplot2::aes(x = reportingPeriodEndingDate, y = n, colour = zeroBalanceCode)) + 
  ggplot2::geom_line() +
  ggplot2::labs(
    title = "ABS Loan Status Composition Since Inception",
    x = "Reporting Period End Date",
    y = "Percent "
  ) +
  ggplot2::theme_minimal()


# Calculate cumulative payments
abs_ts_payments <- abs_ee_data_clean |>
  group_by(remainingTermToMaturity = as.numeric(remainingTermToMaturityNumber)) |>
  summarise(
    actualPrincipalCollectedAmount = mean(actualPrincipalCollectedAmount, na.rm = T),
    scheduledPrincipalAmount = mean(scheduledPrincipalAmount, na.rm = T)
  ) |> 
  dplyr::mutate(
    Diff = actualPrincipalCollectedAmount - scheduledPrincipalAmount,
    `Under/Over Payment` = ifelse(Diff < 0, "Under","Over"))

# Plot cumulative payments over time
ggplot(abs_ts_payments) +
  geom_line(aes(x = remainingTermToMaturity, y = scheduledPrincipalAmount, colour = "Scheduled Principal Amount"),linetype = "dotted", size = 1) +
  geom_line(aes(x = remainingTermToMaturity, y = actualPrincipalCollectedAmount, colour = "Actual Principal Collected Amount"), size = 1) +
  geom_col(aes(x = remainingTermToMaturity, y = Diff, fill = `Under/Over Payment`), alpha = 0.5) +
  scale_x_reverse() +
  scale_colour_manual(name = "Line Legend", values = c("Scheduled Principal Amount" = "red", "Actual Principal Collected Amount" = "blue")) +
  scale_fill_manual(name = "Column Legend", values = c("Under" = "darkred", "Over" = "darkgreen")) +
  labs(title = "Actual vs. Scheduled Payments - Remaining Term to Maturity",
       x = "Remaining Term to Maturity",
       y = "Payments") +
  theme_minimal()


