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





