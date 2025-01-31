##Final Project:PISAdreamjob

##get the gini index of the country of each PISA year

# Install necessary packages if not already installed
install.packages("WDI", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)

# Load the libraries
library(WDI)
library(dplyr)

# Picking up the countries in PISA
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

# Define the PISA years
years <- c(2022, 2018, 2015, 2012, 2009, 2006, 2003, 2000)

# Fetch the Gini data from the World Bank
gini_data <- WDI(country = countries, indicator = "SI.POV.GINI", start = 2000, end = 2022, extra = TRUE)

# Filter the data for the PISA years
gini_filtered <- gini_data %>% 
  filter(year %in% years)

#save the data
print(gini_filtered)

write.csv(gini_filtered, "gini_data.csv", row.names = FALSE)
write.csv(gini_filtered, "/Users/linjinhu/Desktop/gini_data.csv", row.names = FALSE)

#combine the 