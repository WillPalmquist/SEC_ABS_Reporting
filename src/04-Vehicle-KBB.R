# -----------------------------------------------------------------------------
# Name: Will Palmquist
# Date: 06/13/2024
# Title: Forecsting Cumulative Charge-Offs
# -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

############################################
# !!!Load data in 02-Exploratory-Analysis.R
###########################################

########################
## Kelley Blue Book Value
#######################
vehicles_value_code <- abs_ee_data_clean |>
  dplyr::filter(reportingPeriodEndingDate == "2024-04-30") |>
  dplyr::mutate(
    vehicleValueSourceCode = dplyr::case_match(
      as.numeric(vehicleValueSourceCode),
      1 ~ "Invoice Price",
      2 ~ "MSRP",
      3 ~ "Kelley Blue Book",
      98 ~ "Other",
      NA ~ "Other")
  ) |> 
  dplyr::group_by(vehicleValueSourceCode) |>
  dplyr::summarise(
    records = dplyr::n()
  ) 

# April 2024 KBB Values
vehicle_kbb_std <- abs_ee_data_clean |>
  dplyr::filter(reportingPeriodEndingDate == "2024-04-30", vehicleValueSourceCode == 3) |>
  dplyr::group_by(vehicleManufacturerName,vehicleModelName) |>
  dplyr::summarise(
    records = dplyr::n(),
    stdev = sd(vehicleValueAmount, na.rm = T)
  ) |>
  dplyr::filter(records > 30)

# April 2024 Max Stdev
top10_stdev_vehicle_value <- vehicle_kbb_std |>
  dplyr::ungroup() |>
  dplyr::slice_max(order_by = stdev,n = 10)

top10_stdev_vehicle_value |> 
  dplyr::mutate(vehicleMakeModel = paste0(vehicleManufacturerName," ",vehicleModelName)) |> 
  ggplot2::ggplot(aes(x = reorder(vehicleMakeModel,-stdev), y = stdev, fill = stdev)) +
  ggplot2::geom_col() 

max_stdev_vehicle_value <- vehicle_kbb_std |>
  dplyr::ungroup() |>
  dplyr::slice_max(order_by = stdev,n = 1)
