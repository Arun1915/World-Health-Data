library(rvest)
library(httr)
library(dplyr)
Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")

url <- "https://www.who.int/data/gho/whs-annex/"
webpage <- read_html_live(url)

countries       <- html_text(html_elements(webpage, "#whs-annex-SDG3-COUNTRY .k-grid-content-locked td"), trim = TRUE)
mortality_rate  <-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(3)"),trim = TRUE)
Neonatal_rate   <-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(4)"),trim =TRUE)
HIV             <-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(5)"),trim = TRUE)
Suicide <-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(10)"),trim = TRUE)
Total_alcohol<-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(13)"),trim=TRUE)
Road_traffic<-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(14)"),trim =TRUE)
UHC<-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(18)"),trim = TRUE)
Age_sta<-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(20)"),trim =TRUE)
Dip<-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(24)"),trim =TRUE)
Den_Doctor<-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(29)"),trim =TRUE)
Den_nursing<-html_text(html_elements(webpage,"#whs-annex-SDG3-COUNTRY .k-text-right:nth-child(30)"),trim =TRUE)
Domestic<-html_text(html_elements(webpage,"#whs-annex-SDG_GPW-COUNTRY .k-text-right:nth-child(1)"),trim =TRUE)


raw_data <- data.frame(
  Countries = countries,
  `Under five mortatilty rate` = as.numeric(mortality_rate),
    Neonatal_mortality_rate=Neonatal_rate,
    New_HIV_infections=HIV,
    Suicide_mortality_rate=Suicide,
    Total_alcohol_per_capita=Total_alcohol,
    Road_traffic_mortality_rate=Road_traffic,
    UHC_Service_coverage=UHC,
    Age_standardized_mortality_rate=Age_sta,
    Diphtheria_tetanus_pertussis=Dip,
    Density_medical_doctors=Den_Doctor,
    Density_nursing=Den_nursing,
    Domestic_general_gov_health=Domestic
)
View(raw_data)
