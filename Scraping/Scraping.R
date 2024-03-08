#### Libraries #### 

rm(list=ls())

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("rvest", "polite", "httr", "stringr", "beepr", "lubridate"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Scraping ####

# Base URL and initial setup
base_url <- "https://lopezobrador.org.mx/transcripciones/"
total_pages <- 360
amlo <- data.frame(url = character(), title = character(), date = character(), text = character(), stringsAsFactors = FALSE)

# Looping through each page to scrape everything we're interested in: 
for (page_number in 1:total_pages) {
  cat("Scraping page:", page_number, "of", total_pages, "\n")
  
  # Construct the URL for each page
  page_url <- ifelse(page_number == 1, base_url, paste0(base_url, "page/", page_number, "/"))
  
  # Scrape the current page
  page <- read_html(page_url)
  
  # Extracting hyperlinks
  hyperlinks <- page %>%
    html_nodes("h2 a") %>%
    html_attr("href")
  
  # Extracting titles
  titles <- page %>%
    html_nodes("h2.entry-title a") %>%
    html_text()
  
  # Extracting dates
  dates <- page %>%
    html_nodes(".entry-date a") %>%
    html_text()
  
  # Extract text for each press conference on the current page
  text_list <- lapply(hyperlinks, function(link) {
    page <- read_html(link)
    text <- page %>%
      html_nodes(".entry-content") %>%
      html_text() %>%
      paste(collapse = " ")
    return(text)
  })
  
  # Creating a temporary dataframe for this page
  temp_df <- data.frame("url" = hyperlinks, "title" = titles, "date" = dates, "text" = unlist(text_list), stringsAsFactors = FALSE)
  
  # Appending to the main dataframe
  amlo <- rbind(amlo, temp_df)
  
  # Delaying to avoid overwhelming the server
  Sys.sleep(time = 5)
}

# Letting us know when it's done: 

beep(sound = 1, expr = NULL)
print("Scraping complete.")

#### Saving ####

saveRDS(amlo, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Scraping/amlo_speech_scrape.rds")
write.csv(amlo, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Scraping/amlo_speech_scrape.csv", row.names = FALSE)


