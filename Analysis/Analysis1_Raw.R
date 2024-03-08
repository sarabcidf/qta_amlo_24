#### Libraries, WD, Options ####

rm(list=ls())

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse","stringr"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Loading Data #### 

# Data from scraping

amlo_press_clean <- readRDS("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_clean.rds")

#### Preliminary analysis #### 

# Calculating word count for each press conference
amlo_press_clean$word_count <- sapply(amlo_press_clean$text, function(x) length(strsplit(x, "\\s+")[[1]]))
summary(amlo_press_clean$word_count)

# Plotting word count overtime 
ggplot(amlo_press_clean, aes(x = date, y = word_count)) +
  geom_point(alpha = 0.7, color = "snow4") +
  geom_smooth(se = TRUE, color = "royalblue3") +  
  theme_minimal() + 
  labs(title = "Word Count of Conferences Over Time",
       x = "Year",
       y = "Word Count") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

ggsave("words_time.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

# Variation in length per year
ggplot(amlo_press_clean, aes(x = year, y = word_count)) +
  geom_boxplot(outlier.color = "snow4", color = "royalblue4") +
  theme_minimal() +
  labs(title = "Variation in Word Count of Conferences Over Years",
       x = "Year",
       y = "Word Count")

ggsave("var_words_time.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

# Monthly frequency of conferences
monthly_counts <- amlo_press_clean %>%
  group_by(year_month) %>%
  summarise(count = n())
monthly_counts$year_month <- as.Date(paste0(monthly_counts$year_month, "-01"))

# Plotting monthly frequency
ggplot(monthly_counts, aes(x = year_month, y = count)) +
  geom_point(alpha = 0.7, color = "snow4") +  
  geom_smooth(se = TRUE, color = "royalblue3") +  
  theme_minimal() +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  
  labs(title = "Monthly Frequency of Press Conferences Over Time",
       x = "Year",
       y = "Number of Press Conferences")

ggsave("conf_freq_time.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)



