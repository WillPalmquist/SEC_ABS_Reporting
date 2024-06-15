# -----------------------------------------------------------------------------
# Name: Will Palmquist
# Date: 06/13/2024
# Title: Auto-Loan ABS Data Extraction: Exeter Automobile Receivables Trust
# -----------------------------------------------------------------------------


library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(XML)
#####################
### Scrape Monthly Filings
#####################
filing_info <- read.csv("ABS-EE-Filing-Info.csv") 
filing_info <- filing_info |> 
  janitor::clean_names() |> 
  dplyr::filter(form_type == "ABS-EE") |> 
  dplyr::mutate(
    data_file_link = paste0("https://www.sec.gov/Archives/edgar/data/1954436/",stringr::str_remove_all(accession_number,"-"),"/eart2022-6_exhibit102.xml")
    )

useragent <- "Will Palmquist will.palmquist@outlook.com"
httr::set_config(httr::user_agent(useragent))

# Iterate through every link
process_url <- function(url) {
  cat("Processing URL:", url, "\n")
  
  page <- GET(url, user_agent(useragent))
  xml_content <- rawToChar(page$content)
  parsed_xml <- read_xml(xml_content)
  
  # Convert to Wide dataframe - bunch of unnesting
  df <- parsed_xml |>
    as_list() |>
    as_tibble() |>
    tidyr::unnest_wider(col = "assetData", names_repair = "unique") |>
    tidyr::unnest() |>
    tidyr::unnest()
  
  return(df)
}

df_list <- purrr::map(filing_info$data_file_link, process_url)
final_df <- dplyr::bind_rows(df_list)

saveRDS(final_df, "abs_ee_data_all.rds")
