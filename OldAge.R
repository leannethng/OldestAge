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
colnames(table_cleaned_rows) <- c("Birthplace","Name", "Born", "Died", "Age - Years", "Age - Days", "Race", "Sex", "Deathplace", "When Oldest - Years Range", "When Oldest - Age Range", "Length of Reign - Years", "Length of Reign - Days", "Reign Length - In Years", "Age at Accession - Years", "Age at Asccession - Days")

#Export as CSV
write.csv(table_cleaned_rows, file = "oldestAge.csv")




