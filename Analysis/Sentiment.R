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

#### Loading dictionaries in Spanish ####

afinn <- read_csv("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Dictionaries/lexico_afinn.csv")
nrc <- read.csv("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Dictionaries/lexico_nrc.csv")

head(afinn)
head(nrc)

#### Adapting for quanteda use ####

# Makin AFINN binary: 

afinn_positive <- afinn %>% filter(puntuacion > 0) %>% pull(palabra)
afinn_negative <- afinn %>% filter(puntuacion < 0) %>% pull(palabra)

afinn_dict <- dictionary(list(positive = afinn_positive, negative = afinn_negative))

# Making NRC binary: 

# Assuming 'sentimiento' column has broader classifications like 'positivo' or 'negativo'
nrc_positive <- nrc %>% filter(sentimiento == 'positivo') %>% pull(palabra)
nrc_negative <- nrc %>% filter(sentimiento == 'negativo') %>% pull(palabra)

nrc_dict <- dictionary(list(positive = nrc_positive, negative = nrc_negative))

# NRC with more granularity: 

nrc_emotions_dict <- nrc %>%
  group_by(sentimiento) %>%
  summarise(words = list(palabra)) %>%
  deframe() %>% 
  dictionary()

#### Sentiment analysis with AFINN ####

sentiment_afinn <- dfm_lookup(dfm_amlo, afinn_dict) 
head(sentiment_afinn)

net_sentiment_scores_afinn <- rowSums(sentiment_afinn[, "positive", drop = FALSE]) -
  rowSums(sentiment_afinn[, "negative", drop = FALSE])

summary(net_sentiment_scores_afinn)

# Distribution of net sentiment AFINN

ggplot(data.frame(Sentiment = net_sentiment_scores_afinn), aes(x = Sentiment)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Net Sentiment Scores (AFINN)",
       x = "Net Sentiment Score",
       y = "Frequency") +
  theme_minimal()

# Net sentiment overtime

names(docvars(dfm_amlo)) # Checking date is there
doc_dates <- docvars(dfm_amlo, "date")

plot_data_afinn <- data.frame(date = doc_dates, net_sentiment = net_sentiment_scores_afinn)

ggplot(plot_data_afinn, aes(x = date, y = net_sentiment)) +
  geom_smooth() + 
  theme_minimal() +
  labs(title = "Net Sentiment Over Time (AFINN)",
       x = "Date",
       y = "Net Sentiment Score")

#### Sentiment analysis with NRC ####

sentiment_nrc <- dfm_lookup(dfm_amlo, nrc_dict) 
head(sentiment_nrc)

net_sentiment_scores_nrc <- rowSums(sentiment_nrc[, "positive", drop = FALSE]) -
  rowSums(sentiment_nrc[, "negative", drop = FALSE])

summary(net_sentiment_scores_nrc)

# Distribution of net sentiment

ggplot(data.frame(Sentiment = net_sentiment_scores_nrc), aes(x = Sentiment)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Net Sentiment Scores",
       x = "Net Sentiment Score",
       y = "Frequency") +
  theme_minimal()

# Net sentiment overtime

plot_data_nrc <- data.frame(date = doc_dates, net_sentiment = net_sentiment_scores_nrc)

ggplot(plot_data_nrc, aes(x = date, y = net_sentiment)) +
  geom_smooth() + 
  theme_minimal() +
  labs(title = "Net Sentiment Over Time (NRC)",
       x = "Date",
       y = "Net Sentiment Score")

#### Putting AFINN and NRC together #### 

# Distribution of sentiment: 

df_afinn <- data.frame(Sentiment = net_sentiment_scores_afinn, Source = 'AFINN')
df_nrc <- data.frame(Sentiment = net_sentiment_scores_nrc, Source = 'NRC')

combined_df <- rbind(df_afinn, df_nrc)

ggplot(combined_df, aes(x = Sentiment, fill = Source)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +  
  scale_fill_manual(values = c('AFINN' = 'cyan4', 'NRC' = 'indianred')) +  
  labs(title = "Distribution of Net Sentiment Scores (AFINN and NRC)",
       x = "Net Sentiment Score",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank()) 

ggsave("sentdistrib.png",
       width = 8, height = 6,
       device = "png",
       dpi = 300)

# Sentiment overtime

plot_data_afinn$Source = 'AFINN'
plot_data_nrc$Source = 'NRC'

combined_sentiment_data <- rbind(plot_data_afinn, plot_data_nrc)

ggplot(combined_sentiment_data, aes(x = date, y = net_sentiment, color = Source)) +
  geom_smooth(se = TRUE) +  
  scale_color_manual(values = c('AFINN' = 'cyan4', 'NRC' = 'indianred3')) +  
  theme_minimal() +
  labs(title = "Net Sentiment Over Time (AFINN vs. NRC)",
       x = "Date",
       y = "Net Sentiment Score") +
  theme(legend.title = element_blank()) 

ggsave("senttime.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

#### Most negative and positive conferences ####

most_positive_speech <- plot_data_afinn %>%
  arrange(desc(net_sentiment)) %>%
  head(3)

print(most_positive_speech)

most_positive_speech <- plot_data_nrc %>%
  arrange(desc(net_sentiment)) %>%
  head(3)

print(most_positive_speech)

most_neg_speech <- plot_data_afinn %>%
  arrange(net_sentiment) %>%
  head(3)

print(most_neg_speech)

most_neg_speech <- plot_data_nrc %>%
  arrange(net_sentiment) %>%
  head(3)

print(most_neg_speech)

#### Targeted SA with AFINN ####

toks <- readRDS("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/toks_amlo")

dfm_toks <- dfm(toks)

window_size <- 10

# Net sentiment around pueblo

toks_key_pueblo <- tokens_select(toks, pattern = c("pueblo", "gente"), window = window_size)
dfm_pueblo <- dfm(toks_key_pueblo)
docvars(dfm_pueblo, "year_month") <- as.Date(paste0(docvars(dfm_pueblo, "year_month"), "-01"))

dfm_sentiment_pueblo <- dfm_lookup(dfm_pueblo, dictionary = afinn_dict)
dfm_sentiment_pueblo_grouped <- dfm_group(dfm_sentiment_pueblo, groups = docvars(dfm_pueblo, "year_month"))

net_sentiment_pueblo <- rowSums(dfm_sentiment_pueblo_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_pueblo_grouped[, "negative", drop = FALSE])

plot_data <- data.frame(
  Date = as.Date(names(net_sentiment_pueblo)),
  NetSentiment = net_sentiment_pueblo
)

# INE, Banxico y SCJN

toks_key_instit <- tokens_select(toks, pattern = c("ine", "instituto nacional electoral", "i.n.e.",
                                                "suprema_corte", "jueces",
                                                "banxico",
                                                "banco_méxico"), window = window_size)
dfm_instit <- dfm(toks_key_instit)
docvars(dfm_instit, "year_month") <- as.Date(paste0(docvars(dfm_instit, "year_month"), "-01"))

dfm_sentiment_instit <- dfm_lookup(dfm_instit, dictionary = afinn_dict)
dfm_sentiment_instit_grouped <- dfm_group(dfm_sentiment_instit, groups = docvars(dfm_instit, "year_month"))

net_sentiment_instit <- rowSums(dfm_sentiment_instit_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_instit_grouped[, "negative", drop = FALSE])

plot_data <- data.frame(
  Date = as.Date(names(net_sentiment_instit)),
  NetSentiment = net_sentiment_instit
)

# Reforma/Medios

toks_key_medios <- tokens_select(toks, pattern = c("reforma", "periodico", "aristegui", "dresser",
                                                   "ciro", "gómez_leyva", "sarmiento", "lópez_dóriga","denisse_dresser",
                                                   "carmen_aristegui", "loret", "mola", "krauze", "uresti", "dóriga"), 
                                 window = window_size)
dfm_medios <- dfm(toks_key_medios)
docvars(dfm_medios, "year_month") <- as.Date(paste0(docvars(dfm_medios, "year_month"), "-01"))

dfm_sentiment_medios <- dfm_lookup(dfm_medios, dictionary = afinn_dict)
dfm_sentiment_medios_grouped <- dfm_group(dfm_sentiment_medios, groups = docvars(dfm_medios, "year_month"))

net_sentiment_medios <- rowSums(dfm_sentiment_medios_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_medios_grouped[, "negative", drop = FALSE])

# Tren Maya, AIFA, GN, Dos Bocas

toks_key_tren <- tokens_select(toks, pattern = c("tren_maya", "felipe_ángeles", "aifa",
                                                 "refinería","dos_bocas","guardia_nacional"), window = window_size)
dfm_tren <- dfm(toks_key_tren)
docvars(dfm_tren, "year_month") <- as.Date(paste0(docvars(dfm_tren, "year_month"), "-01"))

dfm_sentiment_tren <- dfm_lookup(dfm_tren, dictionary = afinn_dict)
dfm_sentiment_tren_grouped <- dfm_group(dfm_sentiment_tren, groups = docvars(dfm_tren, "year_month"))

net_sentiment_tren <- rowSums(dfm_sentiment_tren_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_tren_grouped[, "negative", drop = FALSE])

# Programas

toks_key_prog <- tokens_select(toks, pattern = c("sembrando_vidas",
                                                 "adultos_mayores",
                                                 "construyendo_futuro"), window = window_size)
dfm_prog <- dfm(toks_key_prog)
docvars(dfm_prog, "year_month") <- as.Date(paste0(docvars(dfm_prog, "year_month"), "-01"))

dfm_sentiment_prog <- dfm_lookup(dfm_prog, dictionary = afinn_dict)
dfm_sentiment_prog_grouped <- dfm_group(dfm_sentiment_prog, groups = docvars(dfm_prog, "year_month"))

net_sentiment_prog <- rowSums(dfm_sentiment_prog_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_prog_grouped[, "negative", drop = FALSE])

# Elites

toks_key_elite <- tokens_select(toks, pattern = c("periodo_neoliberal",
                                                 "conservador",
                                                 "conservadores",
                                                 "bloque_conservador",
                                                 "oposición",
                                                 "élite",
                                                 "élites",
                                                 "prian",
                                                 "pan",
                                                 "mafia",
                                                 "mafia_poder"), window = window_size)
dfm_elite <- dfm(toks_key_elite)
docvars(dfm_elite, "year_month") <- as.Date(paste0(docvars(dfm_elite, "year_month"), "-01"))

dfm_sentiment_elite <- dfm_lookup(dfm_elite, dictionary = afinn_dict)
dfm_sentiment_elite_grouped <- dfm_group(dfm_sentiment_elite, groups = docvars(dfm_elite, "year_month"))

net_sentiment_elite <- rowSums(dfm_sentiment_elite_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_elite_grouped[, "negative", drop = FALSE])

# Putting everything together:

# Data with all keywords
plot_data_combined <- rbind(
  data.frame(Date = as.Date(names(net_sentiment_pueblo)), NetSentiment = net_sentiment_pueblo, Keyword = 'Pueblo/Gente'),
  data.frame(Date = as.Date(names(net_sentiment_instit)), NetSentiment = net_sentiment_instit, Keyword = 'INE/SCJN/Banxico'),
  data.frame(Date = as.Date(names(net_sentiment_medios)), NetSentiment = net_sentiment_medios, Keyword = 'Media/Journalists'),
  data.frame(Date = as.Date(names(net_sentiment_tren)), NetSentiment = net_sentiment_tren, Keyword = 'Projects'),
  data.frame(Date = as.Date(names(net_sentiment_prog)), NetSentiment = net_sentiment_prog, Keyword = 'Programs'),
  data.frame(Date = as.Date(names(net_sentiment_elite)), NetSentiment = net_sentiment_elite, Keyword = 'Elites')
)

# Plotting 
ggplot(plot_data_combined, aes(x = Date, y = NetSentiment, color = Keyword)) +
  geom_smooth() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Time", y = "Net Sentiment", title = "Net Sentiment Around Keywords (AFINN)",
       color = "Keyword Group") +
  scale_color_manual(values = c("#6BAED6", "mediumpurple", "#74C476", "hotpink3", "snow4","plum3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("targsaafinn.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

#### Targeted SA with NRC ####

# Net sentiment around pueblo

toks_key_pueblo <- tokens_select(toks, pattern = c("pueblo", "gente"), window = window_size)
dfm_pueblo <- dfm(toks_key_pueblo)
docvars(dfm_pueblo, "year_month") <- as.Date(paste0(docvars(dfm_pueblo, "year_month"), "-01"))

dfm_sentiment_pueblo <- dfm_lookup(dfm_pueblo, dictionary = nrc_dict)
dfm_sentiment_pueblo_grouped <- dfm_group(dfm_sentiment_pueblo, groups = docvars(dfm_pueblo, "year_month"))

net_sentiment_pueblo <- rowSums(dfm_sentiment_pueblo_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_pueblo_grouped[, "negative", drop = FALSE])

plot_data <- data.frame(
  Date = as.Date(names(net_sentiment_pueblo)),
  NetSentiment = net_sentiment_pueblo
)

# INE, Banxico y SCJN

toks_key_instit <- tokens_select(toks, pattern = c("ine", "instituto nacional electoral", "i.n.e.",
                                                   "suprema_corte",
                                                   "banxico",
                                                   "banco_méxico"), window = window_size)
dfm_instit <- dfm(toks_key_instit)
docvars(dfm_instit, "year_month") <- as.Date(paste0(docvars(dfm_instit, "year_month"), "-01"))

dfm_sentiment_instit <- dfm_lookup(dfm_instit, dictionary = nrc_dict)
dfm_sentiment_instit_grouped <- dfm_group(dfm_sentiment_instit, groups = docvars(dfm_instit, "year_month"))

net_sentiment_instit <- rowSums(dfm_sentiment_instit_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_instit_grouped[, "negative", drop = FALSE])

plot_data <- data.frame(
  Date = as.Date(names(net_sentiment_instit)),
  NetSentiment = net_sentiment_instit
)

# Reforma/Medios

toks_key_medios <- tokens_select(toks, pattern = c("reforma", "periodico", "aristegui", "dresser",
                                                   "ciro", "gómez_leyva", "sarmiento", "lópez_dóriga","denisse_dresser",
                                                   "carmen_aristegui", "loret", "mola", "krauze", "uresti", "dóriga"), 
                                 window = window_size)
dfm_medios <- dfm(toks_key_medios)
docvars(dfm_medios, "year_month") <- as.Date(paste0(docvars(dfm_medios, "year_month"), "-01"))

dfm_sentiment_medios <- dfm_lookup(dfm_medios, dictionary = nrc_dict)
dfm_sentiment_medios_grouped <- dfm_group(dfm_sentiment_medios, groups = docvars(dfm_medios, "year_month"))

net_sentiment_medios <- rowSums(dfm_sentiment_medios_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_medios_grouped[, "negative", drop = FALSE])

# Tren Maya, AIFA, GN, Dos Bocas

toks_key_tren <- tokens_select(toks, pattern = c("tren_maya", "felipe_ángeles", "aifa",
                                                 "refinería","dos_bocas","guardia_nacional"), window = window_size)
dfm_tren <- dfm(toks_key_tren)
docvars(dfm_tren, "year_month") <- as.Date(paste0(docvars(dfm_tren, "year_month"), "-01"))

dfm_sentiment_tren <- dfm_lookup(dfm_tren, dictionary = nrc_dict)
dfm_sentiment_tren_grouped <- dfm_group(dfm_sentiment_tren, groups = docvars(dfm_tren, "year_month"))

net_sentiment_tren <- rowSums(dfm_sentiment_tren_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_tren_grouped[, "negative", drop = FALSE])

# Programas

toks_key_prog <- tokens_select(toks, pattern = c("sembrando_vidas",
                                                 "adultos_mayores",
                                                 "construyendo_futuro"), window = window_size)
dfm_prog <- dfm(toks_key_prog)
docvars(dfm_prog, "year_month") <- as.Date(paste0(docvars(dfm_prog, "year_month"), "-01"))

dfm_sentiment_prog <- dfm_lookup(dfm_prog, dictionary = nrc_dict)
dfm_sentiment_prog_grouped <- dfm_group(dfm_sentiment_prog, groups = docvars(dfm_prog, "year_month"))

net_sentiment_prog <- rowSums(dfm_sentiment_prog_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_prog_grouped[, "negative", drop = FALSE])

toks_key_elite <- tokens_select(toks, pattern = c("periodo_neoliberal",
                                                  "conservador",
                                                  "conservadores",
                                                  "bloque_conservador",
                                                  "oposición",
                                                  "élite",
                                                  "élites",
                                                  "prian",
                                                  "pan",
                                                  "mafia",
                                                  "mafia_poder"), window = window_size)
dfm_elite <- dfm(toks_key_elite)
docvars(dfm_elite, "year_month") <- as.Date(paste0(docvars(dfm_elite, "year_month"), "-01"))

dfm_sentiment_elite <- dfm_lookup(dfm_elite, dictionary = nrc_dict)
dfm_sentiment_elite_grouped <- dfm_group(dfm_sentiment_elite, groups = docvars(dfm_elite, "year_month"))

net_sentiment_elite <- rowSums(dfm_sentiment_elite_grouped[, "positive", drop = FALSE]) -
  rowSums(dfm_sentiment_elite_grouped[, "negative", drop = FALSE])

# Putting everything together:

# Data with all keywords
plot_data_combined <- rbind(
  data.frame(Date = as.Date(names(net_sentiment_pueblo)), NetSentiment = net_sentiment_pueblo, Keyword = 'Pueblo/Gente'),
  data.frame(Date = as.Date(names(net_sentiment_instit)), NetSentiment = net_sentiment_instit, Keyword = 'INE/SCJN/Banxico'),
  data.frame(Date = as.Date(names(net_sentiment_medios)), NetSentiment = net_sentiment_medios, Keyword = 'Media/Journalists'),
  data.frame(Date = as.Date(names(net_sentiment_tren)), NetSentiment = net_sentiment_tren, Keyword = 'Projects'),
  data.frame(Date = as.Date(names(net_sentiment_prog)), NetSentiment = net_sentiment_prog, Keyword = 'Programs'),
  data.frame(Date = as.Date(names(net_sentiment_elite)), NetSentiment = net_sentiment_elite, Keyword = 'Elites')
)

# Plotting 
ggplot(plot_data_combined, aes(x = Date, y = NetSentiment, color = Keyword)) +
  geom_smooth() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Time", y = "Net Sentiment", title = "Net Sentiment Around Keywords (NRC)",
       color = "Keyword Group") +
  scale_color_manual(values = c("#6BAED6", "mediumpurple", "#74C476", "hotpink3", "snow4","plum3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("targsanrc.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)


