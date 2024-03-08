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

#### Loading clean amlo words ####

amlo_words <- readRDS("/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/amlo_press_wds.rds")
summary(amlo_words)

#### Corpus ####

# Making a corpus with amlo words only: 

amlo_corp <- corpus(amlo_words, 
                     docid_field = "date", 
                     text_field = "amlo_talks")

summary(amlo_corp, 5)

#### Pre-processing: Tokens ####

as.character(amlo_corp)[1]

# Creating tokens

# Examining spanish stopwords

print(stopwords("es"))
print(stopwords(language = "es", source = "nltk")) 
print(stopwords(language = "es", source = "stopwords-iso"))

# Putting them all together

nltk_stopwords <- stopwords(language = "es", source = "nltk")
iso_stopwords <- stopwords(language = "es", source = "stopwords-iso")
all_stopwords <- unique(c(stopwords("es"), nltk_stopwords, iso_stopwords))

# Token creation

toks <- quanteda::tokens(amlo_corp, 
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE, 
                         remove_hyphens = TRUE,
                         remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  quanteda::tokens_remove(all_stopwords, padding = TRUE) %>%
  quanteda::tokens_remove('[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE) %>%
  tokens_remove("n", valuetype = "fixed", padding = TRUE)

specific_stwrds <- c("pues", "bueno", "o sea", "sí", "muy bien", "pero", "ahí", 
                     "entonces", "así", "ánimo", "sí sí", "ah", "van", "va", "si",
                     "") # Additional

toks <- tokens_remove(toks, specific_stwrds)

#### Pre-processing: Collocations ####

collocations <- textstat_collocations(toks,
                                      method = "lambda",
                                      size = 2,
                                      min_count = 20,
                                      smoothing = 0.5)

specific_colls <- c("buenos días", "puede ser", "muchas gracias", "ningún problema", "ver si", "hace falta",
                    "cada vez", "sé si", "llevando cabo", "mañana mañana", "punto vista", "semana próxima",
                    "llevó a cabo", "") # Need to clean more!

collocations <- collocations %>%
  filter(!collocation %in% specific_colls)

# Compounding

comp_tok <- tokens_compound(toks, collocations)

kwic(comp_tok, pattern = "suprema_corte")

# Saving 

saveRDS(comp_tok, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/toks_amlo")

#### Pre-processing: DFM ####

# Creating DFM

dfm_amlo <- dfm(comp_tok)
names(docvars(dfm_amlo))

# Re-adding date: 

docvars(dfm_amlo, "date") <- ymd(with(docvars(dfm_amlo), paste(year, month, day, sep = "-")))

# Saving DFM

saveRDS(dfm_amlo, "/Users/sarabcidf/Desktop/ASDS/Dissertation/AMLO/Cleaning/dfm_amlo")

#### Pre-processing: Stemming and lemmatizing options ####



