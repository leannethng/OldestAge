# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

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


#DOESN'T WORK -- Turn dates into dates - can probably turn into a loop
#OldAgeCleanedDates$DiedNew  <- mdy(OldAgeCleanedDates$Died)
#OldAgeCleanedDates$BornNew  <- mdy(OldAgeCleanedDates$Born)

#Need to use as.Date but need to change Sept into Sep first
OldAgeCleanedDates$Born <- str_replace(OldAgeCleanedDates$Born, "Sept", "Sep")

#Then figure out how to combine these two columns
a <- as.Date(OldAgeCleanedDates$Born, format = "%b. %d, %Y")
b <- as.Date(OldAgeCleanedDates$Born, format = "%b %d, %Y")




a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
OldAgeCleanedDates$BornNew <- a # Put it back in your dataframe
OldAgeCleanedDates$BornNew


#Need to use as.Date but need to change Sept into Sep first
OldAgeCleanedDates$Died <- str_replace(OldAgeCleanedDates$Died, "Sept", "Sep")


#Then figure out how to combine these two columns
c <- as.Date(OldAgeCleanedDates$Died, format = "%b. %d, %Y")
d <- as.Date(OldAgeCleanedDates$Died, format = "%b %d, %Y")



c[is.na(c[-65])] <- d[!is.na(d[-65])] # Combine both while keeping their ranks
OldAgeCleanedDates$DiedNew <- c # Put it back in your dataframe
OldAgeCleanedDates$DiedNew


#Turning numbers into numeric values
OldAgeCleanedDates <- transform(OldAgeCleanedDates
                                , AgeDays = as.numeric(AgeDays)
                                , AgeYears = as.numeric(AgeYears)
                                , LengthofReignYears = as.numeric(LengthofReignYears)
                                , LengthofReignDays = as.numeric(LengthofReignDays)
                                , ReignLengthInYears = as.numeric(ReignLengthInYears)
                                , AgeAtAccessionYears = as.numeric(AgeAtAccessionYears)
                                , AgeAtAccessionDays = as.numeric(AgeAtAccessionDays)
                                )

str(OldAgeCleanedDates)


##Things to do
# - Birthplace - split out states and countries
# - Names - Remove notes
# - Born and Died - turn into dates
# - Age in Years/days - turned into combined years/days
# - 

#Should probably save the data into a clean file here
str(OldAgeCleanedDates)

OldestAgeVisData <- subset(OldAgeCleanedDates, select = c(Name, Birthplace, BornNew, DiedNew, Race, Sex))

#Testing some visualizations!
ggplot(data = OldestAgeVisData, mapping = aes(x = Name, y = BornNew, color = Sex)) +
    geom_jitter(width=0.2,alpha=0.4,aes(color = Sex)) 


ggplot(OldestAgeVisData, aes(x = reorder(BornNew, Name),color = Sex)) +
    geom_linerange(aes(ymin = BornNew, ymax = DiedNew)) 





