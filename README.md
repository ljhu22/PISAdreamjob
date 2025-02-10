##Final Project:PISAdreamjob

#########Data Cleaning########

###Get the gini index of the country of each PISA year from The World Bank###

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



#########Get the SD of dreamjob in each pisa year############

#load the packages
install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
library(haven)
library(dplyr)
library(ggplot2)

#2022 data cleaning for sd
# Load the SPSS file
file_path <- "/Users/linjinhu/Desktop/dream job project/PISA dataset/2022/2022schoolquestionaire.SAV"
data <- read_sav(file_path)
newdata <- data %>% select(CNT, CNTRYID, OCOD3)

# delete uncertain answer
newdata$OCOD3 <- as.numeric(as.character(newdata$OCOD3))
newdata <- newdata %>% filter(OCOD3 < 9704)

# Convert labels to name
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

# Apply the label replacement function to the newdata 
newdata <- replace_codes_with_labels(newdata, ocod3_labels, cntryid_labels)

# Split the newdata by country
split_data <- split(newdata, newdata$CNTRYID)


# Count the frequency of each job
count_ocod3 <- function(df) {
  df %>%
    count(OCOD3) %>%
    arrange(desc(n)) %>%
    mutate(OCOD3 = factor(OCOD3, levels = OCOD3))
  
}

# Apply the function to each country and store the results in a list
frequency_counts <- lapply(split_data, count_ocod3)


# Function to calculate the sd of each country
calc_sd <- function(df) {
  sd(df$n, na.rm = TRUE)
}

sd_results <- lapply(frequency_counts, calc_sd)

# Combine the sd with country into one 
sd2022 <- data.frame(
  CNTRYID = names(sd_results),
  SD = unlist(sd_results)
)

write.csv(sd2022, "/Users/linjinhu/Desktop/sd2022.csv", row.names = FALSE)





#2018 data cleaning for sd
# Load the SPSS file
file_path <- "/Users/linjinhu/Desktop/dream job project/PISA dataset/2018PISA/CY07_MSU_STU_QQQ.sav"
data <- read_sav(file_path)
newdata <- data %>% select(CNT, CNTRYID, OCOD3)

# delete uncertain answer
newdata$OCOD3 <- as.numeric(as.character(newdata$OCOD3))
newdata <- newdata %>% filter(OCOD3 < 9704)

# Convert labels to name
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

# Apply the label replacement function to the newdata 
newdata <- replace_codes_with_labels(newdata, ocod3_labels, cntryid_labels)

# Split the newdata by country
split_data <- split(newdata, newdata$CNTRYID)


# Count the frequency of each job
count_ocod3 <- function(df) {
  df %>%
    count(OCOD3) %>%
    arrange(desc(n)) %>%
    mutate(OCOD3 = factor(OCOD3, levels = OCOD3))
  
}

# Apply the function to each country and store the results in a list
frequency_counts <- lapply(split_data, count_ocod3)


# Function to calculate the sd of each country
calc_sd <- function(df) {
  sd(df$n, na.rm = TRUE)
}
sd_results <- lapply(frequency_counts, calc_sd)

# Combine the sd with country into one 
sd2018 <- data.frame(
  CNTRYID = names(sd_results),
  SD = unlist(sd_results)
)

write.csv(sd2018, "/Users/linjinhu/Desktop/sd2018.csv", row.names = FALSE)



####2015 data cleaning for sd
# Load the SPSS file
file_path <- "/Users/linjinhu/Desktop/dream job project/PISA dataset/2015/PISA2015STU.sav"
data <- read_sav(file_path)
newdata <- data %>% select(CNT, CNTRYID, OCOD3)

# delete uncertain answer
newdata$OCOD3 <- as.numeric(as.character(newdata$OCOD3))
newdata <- newdata %>% filter(OCOD3 < 9704)

# Convert labels to name
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

# Apply the label replacement function to the newdata 
newdata <- replace_codes_with_labels(newdata, ocod3_labels, cntryid_labels)

# Split the newdata by country
split_data <- split(newdata, newdata$CNTRYID)

# Count the frequency of each job
count_ocod3 <- function(df) {
  df %>%
    count(OCOD3) %>%
    arrange(desc(n)) %>%
    mutate(OCOD3 = factor(OCOD3, levels = OCOD3))
  
}

# Apply the function to each country and store the results in a list
frequency_counts <- lapply(split_data, count_ocod3)


# Function to calculate the sd of each country
calc_sd <- function(df) {
  sd(df$n, na.rm = TRUE)
}

sd_results <- lapply(frequency_counts, calc_sd)

# Combine the sd with country into one 
sd2015 <- data.frame(
  CNTRYID = names(sd_results),
  SD = unlist(sd_results)
)

write.csv(sd2015, "/Users/linjinhu/Desktop/sd2015.csv", row.names = FALSE)

##2012 OCOD3 variable missing
##2009 OCOD3 variable missing

##2006
# Load the SPSS file
file_path <- "/Users/linjinhu/Desktop/dream job project/PISA dataset/2006/INT_Stu06_Dec07.sav"
data <- read_sav(file_path)
newdata <- data %>% select(CNT, SUBNATIO, ST30Q01)

# delete uncertain answer
newdata$ST30Q01 <- as.numeric(as.character(newdata$ST30Q01))
newdata <- newdata %>% filter(ST30Q01 < 9704)

# Convert labels to name
st30q01_labels <- attr(data$ST30Q01, "labels")
subnatio_labels <- attr(data$SUBNATIO, "labels")
st30q01_names <- setNames(names(st30q01_labels), st30q01_labels)
subnatio_names <- setNames(names(subnatio_labels), subnatio_labels)

# Function to replace codes with labels
replace_codes_with_labels <- function(df, st30q01_labels, subnatio_labels) {
  df$ST30Q01 <- factor(df$ST30Q01, levels = st30q01_labels, labels = names(st30q01_labels))
  df$SUBNATIO <- factor(df$SUBNATIO, levels = subnatio_labels, labels = names(subnatio_labels))
  return(df)
}

# Apply the label replacement function to the newdata 
newdata <- replace_codes_with_labels(newdata, st30q01_labels, subnatio_labels)

# Split the newdata by country
split_data <- split(newdata, newdata$SUBNATIO)

# Count the frequency of each job
count_st30q01 <- function(df) {
  df %>%
    count(ST30Q01) %>%
    arrange(desc(n)) %>%
    mutate(ST30Q01 = factor(ST30Q01, levels = ST30Q01))
}

# Apply the function to each country and store the results in a list
frequency_counts <- lapply(split_data, count_st30q01)


# Function to calculate the sd of each country
calc_sd <- function(df) {
  sd(df$n, na.rm = TRUE)
}

sd_results <- lapply(frequency_counts, calc_sd)

# Combine the sd with country into one 
sd2006 <- data.frame(
  SUBNATIO = names(sd_results),
  SD = unlist(sd_results)
)

write.csv(sd2006, "/Users/linjinhu/Desktop/sd2006.csv", row.names = FALSE)

##2003##
# Load the SPSS file
file_path <- "/Users/linjinhu/Desktop/dream job project/PISA dataset/2003/INT_stui_2003.sav"
data <- read_sav(file_path)
newdata <- data %>% select(CNT, COUNTRY, BSMJ)

# Create a mapping of country codes to country names
country_id_to_name <- c(
  '036' = 'Australia',
  '040' = 'Austria',
  '056' = 'Belgium',
  '203' = 'Czech Republic',
  '250' = 'France',
  '276' = 'Germany',
  '300' = 'Greece',
  '344' = 'Hong Kong',
  '348' = 'Hungary',
  '352' = 'Iceland',
  '360' = 'Indonesia',
  '372' = 'Ireland',
  '380' = 'Italy',
  '410' = 'Korea',
  '428' = 'Latvia',
  '446' = 'Macao',
  '484' = 'Mexico',
  '616' = 'Poland',
  '620' = 'Portugal',
  '703' = 'Slovakia',
  '764' = 'Thailand',
  '826' = 'United Kingdom',
  '840' = 'United States',
  '891' = 'Yugoslavia'
 
)

# Replace the country codes with country names
newdata$COUNTRY <- as.character(data$COUNTRY)  # Ensure COUNTRY is a character type
newdata$COUNTRY <- country_id_to_name[data$COUNTRY]  # Replace country code with country name

# delete uncertain answer
newdata$BSMJ <- as.numeric(as.character(newdata$BSMJ))
newdata <- newdata %>% filter(BSMJ < 9997)

# Split the newdata by country
split_data <- split(newdata, newdata$COUNTRY)

# Count the frequency of each job
count_bsmj <- function(df) {
  df %>%
    count(BSMJ) %>%
    arrange(desc(n)) %>%
    mutate(BSMJ = factor(BSMJ, levels = BSMJ))
}

# Apply the function to each country and store the results in a list
frequency_counts <- lapply(split_data, count_bsmj)


# Function to calculate the sd of each country
calc_sd <- function(df) {
  sd(df$n, na.rm = TRUE)
}

sd_results <- lapply(frequency_counts, calc_sd)

# Combine the sd with country into one 
sd2003 <- data.frame(
  COUNTRY = names(sd_results),
  SD = unlist(sd_results)
)

write.csv(sd2003, "/Users/linjinhu/Desktop/sd2003.csv", row.names = FALSE)


##2000##
# Load the SPSS file
file_path <- "/Users/linjinhu/Desktop/dream job project/PISA dataset/2000/pisa2000data.sav"
data <- read_sav(file_path)
newdata <- data %>% select(COUNTRY, ST40Q01)

# Create a mapping of country codes to country names
country_id_to_name <- c(
  '008' = 'Albania',
  '032' = 'Argentina',
  '036' = 'Australia',
  '040' = 'Austria',
  '056' = 'Belgium',
  '076' = 'Brazil',
  '100' = 'Bulgaria',
  '124' = 'Canada',
  '152' = 'Chile',
  '203' = 'Czech Republic',
  '208' = 'Denmark',
  '246' = 'Finland',
  '250' = 'France',
  '276' = 'Germany',
  '300' = 'Greece',
  '344' = 'Hong Kong',
  '348' = 'Hungary',
  '352' = 'Iceland',
  '360' = 'Indonesia',
  '372' = 'Ireland',
  '376' = 'Israel',
  '380' = 'Italy',
  '392' = 'Japan',
  '410' = 'Korea',
  '428' = 'Latvia',
  '438' = 'Liechtenstein',
  '442' = 'Luxembourg',
  '484' = 'Mexico',
  '528' = 'Netherlands',
  '554' = 'New Zealand',
  '578' = 'Norway',
  '604' = 'Peru',
  '616' = 'Poland',
  '620' = 'Portugal',
  '642' = 'Romania',
  '643' = 'Russian Federation',
  '724' = 'Spain',
  '752' = 'Sweden',
  '756' = 'Switzerland',
  '764' = 'Thailand',
  '807' = 'Macedonia',
  '826' = 'United Kingdom',
  '840' = 'United States'
 
)

# Replace the country codes with country names
newdata$COUNTRY <- as.character(data$COUNTRY)  # Ensure COUNTRY is a character type
newdata$COUNTRY <- country_id_to_name[data$COUNTRY]  # Replace country code with country name


# delete uncertain answer
newdata$ST40Q01 <- as.numeric(as.character(newdata$ST40Q01))
newdata <- newdata %>% filter(ST40Q01 < 9997)

# Split the newdata by country
split_data <- split(newdata, newdata$COUNTRY)

# Count the frequency of each job
count_st40q01 <- function(df) {
  df %>%
    count(ST40Q01) %>%
    arrange(desc(n)) %>%
    mutate(ST40Q01 = factor(ST40Q01, levels = ST40Q01))
}

# Apply the function to each country and store the results in a list
frequency_counts <- lapply(split_data, count_st40q01)


# Function to calculate the sd of each country
calc_sd <- function(df) {
  sd(df$n, na.rm = TRUE)
}

sd_results <- lapply(frequency_counts, calc_sd)

# Combine the sd with country into one 
sd2000 <- data.frame(
  COUNTRY = names(sd_results),
  SD = unlist(sd_results)
)

write.csv(sd2000, "/Users/linjinhu/Desktop/sd2000.csv", row.names = FALSE)

####COMPLIED THESE DATASET AS ONE####
library(dplyr)
library(tidyr)

# Read the datasets
data1 <- read.csv("/Users/linjinhu/Desktop/sd2022.csv")
data2 <- read.csv("/Users/linjinhu/Desktop/sd2018.csv")
data3 <- read.csv("/Users/linjinhu/Desktop/sd2015.csv")
data4 <- read.csv("/Users/linjinhu/Desktop/sd2006.csv")
data5 <- read.csv("/Users/linjinhu/Desktop/sd2003.csv")
data6 <- read.csv("/Users/linjinhu/Desktop/sd2000.csv")

# Rename columns to make them consistent and add a 'Year' column to each dataset
data1 <- data1 %>% rename(COUNTRY = COUNTRY) %>% mutate(Year = 2022)
data2 <- data2 %>% rename(COUNTRY = COUNTRY) %>% mutate(Year = 2018)
data3 <- data3 %>% rename(COUNTRY = COUNTRY) %>% mutate(Year = 2015)
data4 <- data4 %>% rename(COUNTRY = COUNTRY) %>% mutate(Year = 2006)
data5 <- data5 %>% rename(COUNTRY = COUNTRY) %>% mutate(Year = 2003)
data6 <- data6 %>% rename(COUNTRY = COUNTRY) %>% mutate(Year = 2000)

# Combine all datasets into one using bind_rows
combined_data <- bind_rows(data1, data2, data3, data4, data5, data6)

# Convert the dataset into long format
long_data <- combined_data %>%
  pivot_longer(cols = -c(COUNTRY, Year), names_to = "Variable", values_to = "Value")

write.csv(combined_data, "/Users/linjinhu/Desktop/combineddata.csv", row.names = FALSE)

#######CLEANING THE UNU-GINI##########
# Load necessary libraries
library(dplyr)
library(readxl)

WIID_28NOV2023 <- read_excel("/Users/linjinhu/Desktop/UNU-gini/WIID_28NOV2023.xlsx")

selected_countries <- c("Albania", "Azerbaijan", "Argentina", "Australia", "Austria", "Belgium", "Brazil", 
                        "Brunei Darussalam", "Bulgaria", "Cambodia", "Canada", "Chile", "Chinese Taipei", 
                        "Colombia", "Costa Rica", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                        "Dominican Republic", "El Salvador", "Estonia", "Finland", "France", "Georgia", 
                        "Germany", "Greece", "Guatemala", "Hong Kong", "Hungary", "Iceland", "Indonesia", 
                        "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Kazakhstan", "Jordan", "Korea", 
                        "Latvia", "Lithuania", "Malaysia", "Malta", "Mexico", "Moldova", "Montenegro", 
                        "Morocco", "Netherlands", "New Zealand", "Norway", "Panama", "Paraguay", "Peru", 
                        "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Saudi Arabia", "Serbia", 
                        "Singapore", "Slovakia", "Spain", "Sweden", "Switzerland", "Thailand", "United Arab Emirates", 
                        "Turkey", "North Macedonia", "United Kingdom", "United States", "Uruguay", "Uzbekistan", 
                        "Bosnia and Herzegovina", "Belarus", "Lebanon", "Luxembourg", "Russian Federation", 
                        "Vietnam", "Ukraine", "Algeria", "Trinidad and Tobago", "Tunisia", "FYROM", "Kyrgyzstan", 
                        "Liechtenstein")

# Filter the data to include only the selected countries and the required years
filtered_data <- WIID_28NOV2023 %>%
  filter(country %in% selected_countries, year %in% c(2000, 2003, 2006, 2015, 2018, 2022)) %>%
  select(country, year, gini)

print(filtered_data)

write.csv(filtered_data, "/Users/linjinhu/Desktop/UnU-gini.csv", row.names = FALSE)


####Cleaning for WID data####


library(readxl)

##1.income inequality
setwd("/Users/linjinhu/Desktop")
WIDIncomeInequal <- read_excel("WID_Data_10022025-181320.xlsx")

##Filter the country i need####
selected_countries <- c("Albania", "Azerbaijan", "Argentina", "Australia", "Austria", "Belgium", "Brazil", 
                        "Brunei Darussalam", "Bulgaria", "Cambodia", "Canada", "Chile", "Chinese Taipei", 
                        "Colombia", "Costa Rica", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                        "Dominican Republic", "El Salvador", "Estonia", "Finland", "France", "Georgia", 
                        "Germany", "Greece", "Guatemala", "Hong Kong", "Hungary", "Iceland", "Indonesia", 
                        "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Kazakhstan", "Jordan", "Korea", 
                        "Latvia", "Lithuania", "Malaysia", "Malta", "Mexico", "Moldova", "Montenegro", 
                        "Morocco", "Netherlands", "New Zealand", "Norway", "Panama", "Paraguay", "Peru", 
                        "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Saudi Arabia", "Serbia", 
                        "Singapore", "Slovakia", "Spain", "Sweden", "Switzerland", "Thailand", "United Arab Emirates", 
                        "Turkey", "North Macedonia", "United Kingdom", "United States", "Uruguay", "Uzbekistan", 
                        "Bosnia and Herzegovina", "Belarus", "Lebanon", "Luxembourg", "Russian Federation", 
                        "Vietnam", "Ukraine", "Algeria", "Trinidad and Tobago", "Tunisia", "FYROM", "Kyrgyzstan", 
                        "Liechtenstein")

filtered_data <- WIDIncomeInequal %>%
  filter(COUNTRY %in% selected_countries, YEAR %in% c(2000, 2003, 2006, 2015, 2018, 2022)) %>%
  select(COUNTRY, INDICATOR, YEAR, DATA)

write.csv(filtered_data, "/Users/linjinhu/Desktop/WID-INCOMEINEQUALITY.csv", row.names = FALSE)


###2. wealth ineuality 

setwd("/Users/linjinhu/Desktop")
WIDWealthInequal <- read_excel("WID_Data_10022025-184245.xlsx")

##Filter the country i need####
selected_countries <- c("Albania", "Azerbaijan", "Argentina", "Australia", "Austria", "Belgium", "Brazil", 
                        "Brunei Darussalam", "Bulgaria", "Cambodia", "Canada", "Chile", "Chinese Taipei", 
                        "Colombia", "Costa Rica", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                        "Dominican Republic", "El Salvador", "Estonia", "Finland", "France", "Georgia", 
                        "Germany", "Greece", "Guatemala", "Hong Kong", "Hungary", "Iceland", "Indonesia", 
                        "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Kazakhstan", "Jordan", "Korea", 
                        "Latvia", "Lithuania", "Malaysia", "Malta", "Mexico", "Moldova", "Montenegro", 
                        "Morocco", "Netherlands", "New Zealand", "Norway", "Panama", "Paraguay", "Peru", 
                        "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Saudi Arabia", "Serbia", 
                        "Singapore", "Slovakia", "Spain", "Sweden", "Switzerland", "Thailand", "United Arab Emirates", 
                        "Turkey", "North Macedonia", "United Kingdom", "United States", "Uruguay", "Uzbekistan", 
                        "Bosnia and Herzegovina", "Belarus", "Lebanon", "Luxembourg", "Russian Federation", 
                        "Vietnam", "Ukraine", "Algeria", "Trinidad and Tobago", "Tunisia", "FYROM", "Kyrgyzstan", 
                        "Liechtenstein")

filtered_data <- WIDWealthInequal %>%
  filter(COUNTRY %in% selected_countries, YEAR %in% c(2000, 2003, 2006, 2015, 2018, 2022)) %>%
  select(COUNTRY, INDICATOR, YEAR, DATA)

write.csv(filtered_data, "/Users/linjinhu/Desktop/WID-WealthInequal.csv", row.names = FALSE)

###########Complied all the dataset together#########




