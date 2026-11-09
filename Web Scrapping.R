library(rvest)
library(httr)
library(dplyr)
library(stringr) # Added for robust string manipulation

# Point Chromote to the Brave browser executable for live HTML rendering
Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe")
options(chromote.timeout = 40)
url <- "https://www.who.int/data/gho/whs-annex/"
webpage <- read_html_live(url)

# Extract raw data using your existing selectors
countries       <- html_text(html_elements(webpage, "#whs-annex-SDG3-COUNTRY .k-grid-content-locked td"), trim = TRUE)
mortality_rate  <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(3)"), trim = TRUE)
Neonatal_rate   <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(4)"), trim = TRUE)
HIV             <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(5)"), trim = TRUE)
Suicide         <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(10)"), trim = TRUE)
Total_alcohol   <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(13)"), trim = TRUE)
Road_traffic    <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(14)"), trim = TRUE)
UHC             <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(18)"), trim = TRUE)
Age_sta         <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(20)"), trim = TRUE)
Dip             <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(24)"), trim = TRUE)
Den_Doctor      <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(29)"), trim = TRUE)
Den_nursing     <- html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(30)"), trim = TRUE)
Domestic        <- html_text(html_elements(webpage,"#whs-annex-SDG_GPW-COUNTRY .k-text-right:nth-child(1)"), trim = TRUE)

library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(ggplot2) # Added for statistical analysis and visualization

# 1. Assemble the raw data frame
# Do NOT use as.numeric() here. The previous code forced characters into numeric 
# before cleaning them, which caused strings with spaces or "<" to immediately become NA.
raw_data <- data.frame(
  Countries                         = countries,
  `Under five mortality rate`       = mortality_rate,
  `Neonatal mortality rate`         = Neonatal_rate,
  `New HIV infections`              = HIV,
  `Suicide mortality rate`          = Suicide,
  `Total alcohol per capita`        = Total_alcohol,
  `Road traffic mortality rate`     = Road_traffic,
  `UHC Service coverage`            = UHC,
  `Age standardized mortality rate` = Age_sta,
  `Diphtheria tetanus pertussis`    = Dip,
  `Density medical doctors`         = Den_Doctor,
  `Density nursing`                 = Den_nursing,
  `Domestic general gov health`     = Domestic,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# 2. Clean the data for analysis
# Alternative to NA for "<" symbols: In statistics, left-censored data (like "<1") 
# is commonly imputed as half the limit of detection (0.5) or the boundary itself (1). 
# This preserves the data points for your histograms without breaking numeric transformations.
clean_data <- raw_data %>%
  filter(Countries != "" & !is.na(Countries)) %>%
  mutate(
    # Step A: Remove whitespace from all target columns
    across(-Countries, ~ str_replace_all(.x, "\\s+", "")),
    
    # Step B: Extract ONLY the leading numbers, decimals, and < or > symbols.
    # This automatically drops any confidence intervals like "[10.5-12.0]"
    across(-Countries, ~ str_extract(.x, "^[<>]?[0-9.]+")),
    
    # Step C: Handle the left-censored "<" symbols explicitly
    across(-Countries, ~ case_when(
      .x == "<1" ~ "0.5",      
      .x == "<0.1" ~ "0.05",   
      TRUE ~ str_replace_all(.x, "[<>]", "") 
    )),
    
    # Step D: Safely convert the isolated numbers to numeric
    across(-Countries, ~ as.numeric(na_if(.x, "")))
  )
View(clean_data)


saveRDS(clean_data, "who_health_data.rds")
write.csv(clean_data, "who data")
