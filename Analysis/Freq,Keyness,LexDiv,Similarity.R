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
         "stringr"),  pkgTest)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Loading clean amlo DFM ####

dfm_amlo <- readRDS("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/dfm_amlo")

#### Freq analysis ####

# Some examining: 

tok_freq <- textstat_frequency(dfm_amlo)
head(tok_freq, n = 100)

# Top tokens and their frequency

dfm_freq <- textstat_frequency(dfm_amlo, n = 30)
dfm_freq$feature <- with(dfm_freq, reorder(feature, -frequency))

highlight_words <- c("información", "gente", "méxico", "pueblo",
                     "informar", "corrupción")
dfm_freq$color_indicator <- ifelse(dfm_freq$feature %in% highlight_words, "highlight", "normal")

ggplot(dfm_freq, aes(x = feature, y = frequency, fill = color_indicator)) + 
  ggtitle("Most Frequent Features in Conferences") +
  geom_bar(stat = "identity", alpha = 0.9, show.legend = FALSE) + 
  geom_text(aes(label = feature), vjust = 0, angle = 90, hjust = -0.2) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Feature", y = "Frequency") + 
  scale_y_continuous(limits = c(NA, max(dfm_freq$frequency) * 1.2)) +
  scale_fill_manual(values = c("highlight" = "royalblue", "normal" = "snow4"))

ggsave("TopWords.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

# Wordcloud ?

#### Lexical Diversity ####

ld <- textstat_lexdiv(dfm_amlo, measure = "TTR") 
ld <- as.data.frame(ld)
names(ld)[1] <- "doc_id"

# Merging TTR scores with metadata

metadata <- data.frame(doc_id = docnames(dfm_amlo), docvars(dfm_amlo))
combined_df <- merge(metadata, ld, by = "doc_id")

# Aggregating TTR scores by year_month
monthly_div <- combined_df %>%
  group_by(year_month) %>%
  summarize(average_ttr = mean(TTR, na.rm = TRUE))

# Fixing date format
monthly_div$year_month <- as.Date(paste0(monthly_div$year_month, "-01"))

# Plotting
ggplot(monthly_div, aes(x = year_month, y = average_ttr, group = 1)) + 
  geom_point(alpha = 0.7, color = "snow4") +
  geom_smooth(se = TRUE, color = "royalblue3") + 
  theme_minimal() +
  ggtitle("Lexical Diversity of Conferences Over Time") + 
  xlab("Year") + 
  ylab("Average Monthly TTR Score") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("ldiversity.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

# There are other measures: https://quanteda.io/reference/textstat_lexdiv.html 

