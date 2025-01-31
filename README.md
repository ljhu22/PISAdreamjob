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

##get the sd of dreamjob in each pisa year

install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
library(haven)
library(dplyr)
library(ggplot2)

#2022
#load the spss file
file_path <- "/Users/linjinhu/Desktop/dream job project/PISA dataset/2022/2022schoolquestionaire.SAV"
data2022 <- read_sav(file_path)

## Select relevant variable
newdata2022 <- data %>% select(CNT, CNTRYID, OCOD3)

# Convert labels to names
ocod3_labels <- attr(data$OCOD3, "labels")
cntryid_labels <- attr(data$CNTRYID, "labels")
ocod3_names <- setNames(names(ocod3_labels), ocod3_labels)
cntryid_names <- setNames(names(cntryid_labels), cntryid_labels)

# Function to replace codes with labels
replace_codes_with_labels <- function(df, ocod3_labels, cntryid_labels) {
  df$OCOD3 <- factor(df$OCOD3, levels = ocod3_labels, labels = names(ocod3_labels))
  df$CNTRYID <- factor(df$CNTRYID, levels = cntryid_labels, labels = names(cntryid_labels))
  return(df)
}

# Apply the label replacement function to the data
newdata2022 <- replace_codes_with_labels(newdata2022, ocod3_labels, cntryid_labels)

# Split the newdata by country (CNTRYID)
split_data <- split(newdata2022, newdata2022$CNTRYID)

# Count the frequency of each job (OCOD3) in each country
count_ocod3 <- function(df) {
  df %>%
    count(OCOD3) %>%  # Count frequency of each occupation code
    arrange(desc(n)) %>%
    mutate(OCOD3 = factor(OCOD3, levels = OCOD3))
}

# Apply the frequency counting function to each country's data
frequency_counts <- lapply(split_data, count_ocod3)

# Function to calculate the standard deviation of occupation frequencies
calc_sd <- function(df) {
  sd(df$n, na.rm = TRUE)  # Calculate SD of the frequency counts
}

# Apply the standard deviation function to each country's data
sd_results <- lapply(frequency_counts, calc_sd)

# Combine the standard deviation results with country IDs
sd_2022 <- data.frame(
  CNTRYID = names(sd_results),
  SD = unlist(sd_results)
)


#save
write.csv(sd_2022, "pisasd_2022.csv", row.names = FALSE)
write.csv(sd_2022, "/Users/linjinhu/Desktop/sd_2022.csv", row.names = FALSE)

