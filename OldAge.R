# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(RColorBrewer)

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


#Should probably save the data into a clean file here
str(OldAgeCleanedDates)

#Creating a dataset to work with
OldestAgeVisData <- subset(OldAgeCleanedDates, select = c(Name, Birthplace, BornNew, DiedNew, Race, Sex))

#Age in Days
OldestAgeVisData$AgeDays <- difftime(OldestAgeVisData$DiedNew, OldestAgeVisData$BornNew,  units = c("days") )

#Age in Weeks
OldestAgeVisData$AgeWeeks <- difftime(OldestAgeVisData$DiedNew, OldestAgeVisData$BornNew,  units = c("weeks") )


str(OldestAgeVisData)

OldestAgeVisData$AgeDays <- as.numeric(OldestAgeVisData$AgeDays)
OldestAgeVisData$AgeWeeks <- as.numeric(OldestAgeVisData$AgeWeeks)
#Testing some visualizations!

OldestAgeVisData$AgeYears <- OldestAgeVisData$AgeWeeks/52.143


ggplot(data = OldestAgeVisData, mapping = aes(x = order(BornNew), y = AgeYears, color = Race)) +
    geom_jitter(size=3,alpha=1,aes(color = Race)) + scale_colour_brewer(palette = "Accent")

ggplot(data = OldestAgeVisData, mapping = aes(x = BornNew, y = AgeYears, color = Race)) +
    geom_line(alpha=1,aes(color = Race)) + scale_colour_brewer(palette = "Accent") + labs(x = "Born")


ggplot(OldestAgeVisData, aes(x = reorder(Name, o), y = BornNew,color = Sex)) +
    geom_linerange(aes(ymin = BornNew, ymax = DiedNew)) 

ggplot(OldestAgeVisData, aes(x = reorder(Name, BornNew), color = Sex)) +
    geom_linerange(aes(ymin = 0, ymax = AgeYears))

#Line chart
ggplot(OldestAgeVisData, aes(x = BornNew, y = AgeYears)) +
    geom_line() +
    geom_point(aes(color = Sex)) +
    geom_text(aes(label=paste(format(round(AgeYears, digits = 1)),"years"),vjust = -1, hjust = .5), size = 3, alpha = 1, color = "#151515", check_overlap = TRUE, position = "dodge")
    
#Bar chart
ggplot(OldestAgeVisData, aes(x = AgeYears, y = BornNew, color = Sex)) +
    geom_point() +
    geom_segment(aes(xend= 0, yend=BornNew, color = Sex), size = 1, lineend = "round" ) +
    #geom_linerange(aes(xmin = 0, xmax = AgeYears)) +
    geom_text(aes(label=paste(format(round(AgeYears, digits = 1)),"years"),vjust = 0, hjust = 0), size = 2, alpha = 1, color = "#151515") +
   xlim(0,130) 
  


