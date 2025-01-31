# PISAdreamjob
Class repo for From Data to Manuscript in R

#Final Project 

##get the gini index of the country of each PISA year

install.packages("WDI", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)

library(WDI)
library(dplyr)

countries <- c("AL", "DZ", "AZ", "AR", "AU", "AT", "BE", "BR", "BN", "BA", 
               "BY", "BG", "KH", "CA", "CL", "TW", "CO", "CR", "HR", "CZ", 
               "DK", "DO", "SV", "EE", "FI", "FR", "GE", "PS", "DE", "GR", 
               "GT", "HK", "HU", "IS", "ID", "IE", "IL", "IT", "XK", "JM", 
               "JP", "KG", "KZ", "JO", "KR", "LB", "LV", "LI", "LU", "LT", 
               "MO", "MY", "MT", "MX", "MN", "MD", "ME", "MA", "NL", "NZ", 
               "NO", "PA", "PY", "PE", "PH", "PL", "PT", "QA", "RO", "RU", 
               "SA", "RS", "SG", "SK", "VN", "SI", "ES", "SE", "CH", "TH", 
               "AE", "TN", "TR", "MK", "GB", "US", "UY", "UZ", "UA", 
               "CN", "CN", "RU", "RU", "USA", "ES", "TT", "MK", "USA", "USA", "AR")

years <- c(2022, 2018, 2015, 2012, 2009, 2006, 2003, 2000)

gini_data <- WDI(country = countries, indicator = "SI.POV.GINI", start = 2000, end = 2022, extra = TRUE)

gini_filtered <- gini_data %>% 
  filter(year %in% years)

print(gini_filtered)

write.csv(gini_filtered, "gini_data.csv", row.names = FALSE)