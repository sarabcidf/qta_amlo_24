#### Libraries, WD, Options ####

rm(list=ls())

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse","lubridate","stringr"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Loading Data #### 

# Data from scraping
amlo_scrape <- readRDS("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Scraping/amlo_speech_scrape.rds")

#### Initial filtering #### 

# First, extracting press conferences only 
# (There are other speeches in the website):

amlopress <- amlo_scrape %>%
  filter(str_detect(title, "conferencia de prensa"))
# I'm keeping this first one

amlopress2 <- amlo_scrape %>%
  filter(str_detect(title, "conferencia de prensa matutina"))

amlopress3 <- amlo_scrape %>%
  filter(str_detect(title, "conferencia"))

# Removing "extraordinarias" (only 3 obs from Hidalgo explosion)
amlopress <- amlopress %>%
  filter(!str_detect(title, "extraordinaria"))

#### Fixing dates and removing duplicate ####

# Fixing dates (months to English)
months_spanish_to_english <- setNames(
  c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
  c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
)

# Function to replace Spanish month names with English date string
replace_month_names <- function(date_string) {
  for(spanish in names(months_spanish_to_english)) {
    english <- months_spanish_to_english[spanish]
    date_string <- gsub(spanish, english, date_string, ignore.case = TRUE)
  }
  return(date_string)
}

# Applying the replacement to the date column
amlopress$date <- sapply(amlopress$date, replace_month_names)

# Converting modified date strings to Date objects
amlopress$date <- as.Date(amlopress$date, format="%B %d, %Y")

# Creating 'day', 'year' and 'month' columns from the 'date' column
amlopress$day <- format(amlopress$date, "%d")
amlopress$month <- format(amlopress$date, "%m")
amlopress$year <- format(amlopress$date, "%Y")

# Combining 'year' and 'month' for monthly aggregation
amlopress$year_month <- paste(amlopress$year, amlopress$month, sep = "-")

# Removing duplicates, there's one duplicated date! The text is identical, its is a duplicated
# entry from the site. 
duplicates <- amlopress[duplicated(amlopress$date) | duplicated(amlopress$date, fromLast = TRUE), ]
duplicates$date
duplicates$text

amlopress <- amlopress[!duplicated(amlopress$date), ]

#### Saving ####

# amlo_press_clean is with initial filtering and with fixing dates
saveRDS(amlopress, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_clean.rds")
write.csv(amlopress, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_clean.csv", row.names = FALSE)

#### Filtering for AMLO's words only ####

extract_amlo_words <- function(transcript) {
  # Pattern to identify when AMLO talks
  pattern_amlo <- "PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR:"
  
  # Splitting the transcript into lines
  lines <- unlist(strsplit(transcript, "\\n"))
  
  # Initializing empty vector to store AMLO's segments
  amlo_segments <- character()
  
  # Iterating through each line to find and extract when AMLO talks
  for (line in lines) {
    if (grepl(pattern_amlo, line)) {
      # Extract AMLO's speech from the line
      speech <- sub(pattern_amlo, "", line)
      amlo_segments <- c(amlo_segments, speech)
    }
  }
  
  # Returning all segments where AMLO talks as a single string
  return(paste(amlo_segments, collapse = "\\n"))
}

# Applying the function to our text column:
amlopress$amlo_talks <- sapply(amlopress$text, extract_amlo_words)

# Checking results:
amlopress$text[1]
amlopress$amlo_talks[1]
# This one seems to have worked
# Check against: https://lopezobrador.org.mx/2024/02/20/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-1132/

#### Saving #### 

# First, saving dataset with both columns, "text" and "amlo_talks": 
# amlo_press_both

summary(amlopress)

saveRDS(amlopress, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_both.rds")
write.csv(amlopress, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_both.csv", row.names = FALSE)

# Second, saving dataset with only amlo's words: amlo_press_wds

amlo_press_wds <- amlopress %>% select(-text)
summary(amlo_press_wds)

saveRDS(amlo_press_wds, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_wds.rds")
write.csv(amlo_press_wds, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_wds.csv", row.names = FALSE)


