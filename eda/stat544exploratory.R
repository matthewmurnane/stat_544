library(arrow)
library(tidyverse)
library(dplyr)

### https://api.census.gov/data/2022/acs/acs1/pums/variables.html

data <- read_parquet("clean_data.parquet")

colSums(is.na(data))

print(data$POVPIP)
data %>%
  summarise(under15 = sum(POVPIP == -1, na.rm = TRUE),
  over15 = sum(POVPIP != -1, na.rm = TRUE))

### "501": "501 percent or more",
### "-1": "N/A (individuals who are under 15 and are either living in a housing unit but are unrelated to the householder or are living in select group quarters)"
### min: "0", max: "500"

data$AGEP <- as.numeric(as.character(data$AGEP))

hist(data$AGEP, breaks = 20, xlab = "Age", main="Histogram of Age")

countsRAC1P <- table(data$RAC1P)
barplot(countsRAC1P, xlab="Race", 
        ylab="Count", main="Counts of Race",
        names.arg=c("American Native", "Asian", "Black", "Native Alaskan", "Native Hawaiian or Pacific Islander", "Other", "White"))

### "1": "White alone"
### "2": "Black or African American alone"
### "3": "American Indian alone"
### "4": "Alaska Native alone"
### "5": "American Indian and Alaska Native tribes specified; or American Indian or Alaska Native, not specified and no other races"
### "6": "Asian alone"
### "7": "Native Hawaiian and Other Pacific Islander alone"
### "8": "Some other race alone"
### "9": "Two or More Races"

countsHISP <- table(data$HISP)
barplot(countsHISP, xlab="Hispanic Origin", ylab="Count", main="Counts of Hispanic Origin")



data$GRPIP <- as.numeric(as.character(data$GRPIP))
data %>%
  summarise(percent101 = sum(GRPIP > 100, na.rm = TRUE),
            percent0 = sum(GRPIP == 0, na.rm = TRUE))
hist(data$GRPIP, breaks = 10, xlab = "Gross rent as a percentage of household income", main="Histogram of Gross rent as a percentage of household income")

print(data$STATE)
countsSTATE <- table(data$STATE)
countsSTATE


