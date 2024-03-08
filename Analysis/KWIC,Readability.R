#### Libraries, WD, Options ####

rm(list=ls())

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "quanteda",
         "quanteda.corpora",
         "quanteda.textstats", 
         "quanteda.textplots", 
         "readtext", 
         "stringi", 
         "textstem",
         "stringr",
         "syllable"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Loading clean amlo words ####

amlo_words <- readRDS("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_wds.rds")
summary(amlo_words)

#### Corpus ####

# Making a corpus with amlo words only: 

amlo_corp <- corpus(amlo_words, 
                    docid_field = "date", 
                    text_field = "amlo_talks")

summary(amlo_corp, 5)
head(amlo_corp)
names(docvars(amlo_corp))

#### KWIC #### 

mafia_kwic <- kwic(amlo_corp,
                   pattern = "mafia",
                   window = 5,
                   case_insensitive = TRUE)

head(mafia_kwic)

neolib_kwic <- kwic(amlo_corp,
                    pattern = "neoliberal",
                    window = 5,
                    case_insensitive = TRUE)

head(neolib_kwic)

# Make more :) ***

#### Readability ####

# Readability (FK)

fk_scores <- textstat_readability(amlo_corp, measure = "Flesch.Kincaid")
fk_scores_df <- as.data.frame(fk_scores)
names(fk_scores_df)[1] <- "doc_id"

# Merging FK scores with the corpus metadata
corpus_metadata <- data.frame(doc_id = docnames(amlo_corp), docvars(amlo_corp))
combined_df <- merge(corpus_metadata, fk_scores_df, by = "doc_id")

# Aggregating FK scores by year_month
monthly_readability <- combined_df %>%
  group_by(year_month) %>%
  summarize(average_fk = mean(Flesch.Kincaid, na.rm = TRUE))

# Fixing date format
monthly_readability$year_month <- as.Date(paste0(monthly_readability$year_month, "-01"))

# Plotting 
ggplot(monthly_readability, aes(x = year_month, y = average_fk, group = 1)) + 
  geom_point(alpha = 0.7, color = "snow4") +
  geom_smooth(se = TRUE, color = "royalblue3") + 
  theme_minimal() +
  ggtitle("Readability of Conferences Over Time") +
  xlab("Year") + 
  ylab("Monthly Average Flesch-Kincaid Score") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("readability.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

# Fernandez-Huerta: 

# Syllables
count_syllables <- function(word) {
  # Convert word to lowercase and remove accents
  word <- chartr("áéíóúü", "aeiouu", tolower(word))
  # Count syllables
  syllable_count <- sum(gregexpr("[aeiouyáéíóúü]+", word)[[1]] > 0)
  return(syllable_count)
}

# Testing
test_words <- c("hello", "syllable", "programming", "understanding")
for (word in test_words) {
  syllables <- count_syllables(word)
  print(paste("Word:", word, "| Syllables:", syllables))
}

fernandez_huerta <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  total_words <- length(words)
  total_syllables <- sum(sapply(words, count_syllables))  
  fernandez_score <- 206.835 - (0.60 * (total_syllables / total_words)) - (1.02 * (total_words / length(unlist(strsplit(text, "[.!?]+")))))
  return(fernandez_score)
}

amlo_corp_filtered <- amlo_corp[amlo_corp != ""]

# Calculating Fernández-Huerta Readability Score for each document in the filtered corpus
fernandez_scores <- sapply(amlo_corp_filtered, function(text) fernandez_huerta(text))

# Convert fernandez_scores to data frame
fernandez_scores_df <- data.frame(doc_id = names(fernandez_scores), fernandez_score = fernandez_scores)
names(fernandez_scores_df)[1] <- "doc_id"

# Merging Fernández-Huerta scores with the corpus metadata
combined_df_fernandez <- merge(corpus_metadata, fernandez_scores_df, by = "doc_id")
combined_df_fernandez <- combined_df_fernandez[complete.cases(combined_df_fernandez), ]

# Aggregating Fernández-Huerta scores by year_month
monthly_fernandez <- combined_df_fernandez %>%
  mutate(year_month = as.Date(paste0(year_month, "-01"))) %>%
  group_by(year_month) %>%
  summarize(average_fernandez = mean(fernandez_score, na.rm = TRUE))

# Plotting
ggplot(monthly_fernandez, aes(x = year_month, y = average_fernandez)) + 
  geom_point(alpha = 0.7, color = "snow4") +
  geom_smooth(method = "loess", se = FALSE, color = "royalblue3") + 
  theme_minimal() +
  ggtitle("Readability of Conferences Over Time") +
  xlab("Year") + 
  ylab("Monthly Average Fernández-Huerta Score") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Saving plot
ggsave("fernandez_readability.png",
       width = 8, height = 6,
       device = "png",
       dpi = 300)

# There's other measures: https://quanteda.io/reference/textstat_readability.html
# In paper, Constantine uses Gulpease index (G-index) (Lucisano and Piemontese 1988)