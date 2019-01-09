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




# OLD CODE --- Extract records info ------
results <- webpage %>% html_nodes("tr") %>% html_nodes("height:15.75pt")  %>% html_nodes("td")
results

first_result <- results[1]

first_result 

new_data <- first_result %>% html_text(trim = TRUE)
new_data


# OLD CODE --- Run through -----
records <- vector("list", length = length(results))

for (i in seq_along(results)) {
    data <- str_c(results[i] %>% html_text(trim = TRUE))
    records[[i]] <- data_frame(data = data)
}

df <- bind_rows(records)
glimpse(df)



