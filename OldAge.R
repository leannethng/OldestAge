# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)

# Read web page
webpage <- read_html("http://www.grg.org/Adams/C_files/sheet001.htm")
webpage

webpage_table <- html_table(html_nodes(webpage, "table")[[1]], fill = TRUE)
webpage_table

#Cleaning 
table_cleaned_columns <- webpage_table[-c(1:8, 74:118), ]
table_cleaned_rows <- table_cleaned_columns[-c(1,18:22)]

rownames(table_cleaned_rows) <- 1:nrow(table_cleaned_rows)
colnames(table_cleaned_rows) <- c("Birthplace","Name", "Born", "Died", "AgeYears", "AgeDays", "Race", "Sex", "Deathplace", "WhenOldestYearsRange", "WhenOldestAgeRange", "LengthofReignYears", "LengthofReignDays", "ReignLengthInYears", "AgeAtAccessionYears", "AgeAtAccessionDays")

#Export as CSV ----
write.csv(table_cleaned_rows, file = "oldestAge.csv")

#Next part of cleaning, creating a copy of dataframe for going forward
OldAgeCleanedDates <- table_cleaned_rows


#This is the same as below but removing it from the for loop for testing
#results <- str_detect(OldAgeCleanedDates[,5], "\\*")
#changeResults <-  which(results)
#OldAgeCleanedDates[changeResults,5] <- sub("\\*.*", "", OldAgeCleanedDates[changeResults,5])


#This for loop looks for '*' symbol and cleans it from all the data
for(i in 1:ncol(OldAgeCleanedDates)){
    results <- str_detect(OldAgeCleanedDates[,i], "\\*")
    changeResults <-  which(results)
    OldAgeCleanedDates[changeResults,i] <- sub("\\*.*", "", OldAgeCleanedDates[changeResults,i])
}


#Made another loop for finding everything after [ to remove the numbers for footnotes. 
for(i in 1:ncol(OldAgeCleanedDates)){
    results <- str_detect(OldAgeCleanedDates[,i], "\\[.*")
    changeResults <-  which(results)
    OldAgeCleanedDates[changeResults,i] <- sub("\\[.*", "", OldAgeCleanedDates[changeResults,i])
}


#Made another loop for finding everything after ( to remove the numbers for footnotes. 
for(i in 1:ncol(OldAgeCleanedDates)){
    results <- str_detect(OldAgeCleanedDates[,i], "\\(.*")
    changeResults <-  which(results)
    OldAgeCleanedDates[changeResults,i] <- sub("\\(.*", "", OldAgeCleanedDates[changeResults,i])
}


#Remove [*] footnotes - can probably turn into a loop
#OldAgeCleanedDates$Name <- sub("\\[.*", "", OldAgeCleanedDates$Name)
#OldAgeCleanedDates$Born <- sub("\\[.*", "", OldAgeCleanedDates$Born)

#Turn dates into dates - can probably turn into a loop
OldAgeCleanedDates$Died  <- mdy(OldAgeCleanedDates$Died)
OldAgeCleanedDates$Born  <- mdy(OldAgeCleanedDates$Born)

OldAgeCleanedDates$AgeDays <- as.numeric(OldAgeCleanedDates$AgeDays)
OldAgeCleanedDates$AgeYears <- as.numeric(OldAgeCleanedDates$AgeYears)    


OldAgeCleanedDates$LengthofReignYears <- as.numeric(OldAgeCleanedDates$LengthofReignYears)



str(OldAgeCleanedDates)
##Things to do
# - Birthplace - split out states and countries
# - Names - Remove notes
# - Born and Died - turn into dates
# - Age in Years/days - turned into combined years/days
# - 




