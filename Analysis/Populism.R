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

#### Populism dictionaries ####

anti_elit1 <- dictionary(list(anti_elit1 = c("antidemocrático", "antidemocrática", "casta", "consenso", "corrupto", 
                                             "corruptos", "deshonesto", "deshonestos", "élite", "élites", "poder establecido", 
                                             "engaño", "mentir", "mentira", "mentiras", "propaganda", "escándalo", "escándalos", 
                                             "traición", "traicionar", "traicionado", "traicionados", "vergüenza", "verdad"
)))


anti_elit2 <- dictionary(list(anti_elit2 = c("antidemocrático", "antidemocrática", "casta", "consenso", "corrupto", 
                                             "corruptos", "deshonesto", "deshonestos", "élite", "élites", "poder establecido", 
                                             "engaño", "mentir", "mentira", "mentiras", "propaganda", "escándalo", "escándalos", 
                                             "traición", "traicionar", "traicionado", "traicionados", "vergüenza", "verdad",
                                             "mafia_poder","fifí", "fifís", "mafia", "conservador", "conservadores",
                                             "neoliberal", "neoliberalismo","oligarquía", "osposición", "opositores"
)))

ppl_cent1 <- dictionary(list(ppl_cent1 = c("ciudadano", "ciudadanos", 
                                           "consumidor", "consumidores", 
                                           "contribuyente", "contribuyentes", 
                                           "elector", "electores", 
                                           "gente", "pueblo"
)))

ppl_cent2 <- dictionary(list(ppl_cent2 = c("ciudadano", "ciudadanos", 
                                           "consumidor", "consumidores", 
                                           "contribuyente", "contribuyentes", 
                                           "elector", "electores", 
                                           "gente", "pueblo",
                                           "pobre","pobres","primero_pobres"
)))

#### Applying dictionaries ####

# Original anti-elitism:

dfm_populism <- dfm_lookup(dfm_amlo, dictionary = anti_elit1)
head(dfm_populism)
names(docvars(dfm_populism))

docvars(dfm_populism, "year_month") <- as.Date(paste0(docvars(dfm_populism, "year_month"), "-01"))
year_month <- docvars(dfm_populism, "year_month")
populism_counts <- rowSums(dfm_populism)

plot_data <- data.frame(Date = year_month, Populism_Count = populism_counts)

ggplot(plot_data, aes(x = Date, y = Populism_Count)) +
  geom_smooth() +
  labs(title = "Populist Language Over Time", x = "Date", y = "Count of Populist Terms")

# Expanded anti-elitism

dfm_populism <- dfm_lookup(dfm_amlo, dictionary = anti_elit2)
head(dfm_populism)
names(docvars(dfm_populism))

docvars(dfm_populism, "year_month") <- as.Date(paste0(docvars(dfm_populism, "year_month"), "-01"))
year_month <- docvars(dfm_populism, "year_month")
populism_counts <- rowSums(dfm_populism)

plot_data <- data.frame(Date = year_month, Populism_Count = populism_counts)

ggplot(plot_data, aes(x = Date, y = Populism_Count)) +
  geom_smooth() +
  labs(title = "Populist Language Over Time", x = "Date", y = "Count of Populist Terms")

# Original people-centrism

dfm_populism <- dfm_lookup(dfm_amlo, dictionary = ppl_cent1)
head(dfm_populism)
names(docvars(dfm_populism))

docvars(dfm_populism, "year_month") <- as.Date(paste0(docvars(dfm_populism, "year_month"), "-01"))
year_month <- docvars(dfm_populism, "year_month")
populism_counts <- rowSums(dfm_populism)

plot_data <- data.frame(Date = year_month, Populism_Count = populism_counts)

ggplot(plot_data, aes(x = Date, y = Populism_Count)) +
  geom_smooth() +
  labs(title = "Populist Language Over Time", x = "Date", y = "Count of Populist Terms")

# Expanded people-centrism

dfm_populism <- dfm_lookup(dfm_amlo, dictionary = ppl_cent2)
head(dfm_populism)
names(docvars(dfm_populism))

docvars(dfm_populism, "year_month") <- as.Date(paste0(docvars(dfm_populism, "year_month"), "-01"))
year_month <- docvars(dfm_populism, "year_month")
populism_counts <- rowSums(dfm_populism)

plot_data <- data.frame(Date = year_month, Populism_Count = populism_counts)

ggplot(plot_data, aes(x = Date, y = Populism_Count)) +
  geom_smooth() +
  labs(title = "Populist Language Over Time", x = "Date", y = "Count of Populist Terms")

#### Putting everything together ####

plot_data1 <- data.frame(Date = year_month, Populism_Count = rowSums(dfm_lookup(dfm_amlo, dictionary = anti_elit1)), Type = "Original Anti-Elitism")
plot_data2 <- data.frame(Date = year_month, Populism_Count = rowSums(dfm_lookup(dfm_amlo, dictionary = anti_elit2)), Type = "Expanded Anti-Elitism")
plot_data3 <- data.frame(Date = year_month, Populism_Count = rowSums(dfm_lookup(dfm_amlo, dictionary = ppl_cent1)), Type = "Original People-Centrism")
plot_data4 <- data.frame(Date = year_month, Populism_Count = rowSums(dfm_lookup(dfm_amlo, dictionary = ppl_cent2)), Type = "Expanded People-Centrism")

# Combine all data frames into one
combined_plot_data <- rbind(plot_data1, plot_data2, plot_data3, plot_data4)

# Plotting
ggplot(combined_plot_data, aes(x = Date, y = Populism_Count, color = Type)) +
  geom_smooth() + 
  labs(title = "Populist Language Over Time", x = "Year", y = "Count of Populist Terms",
       color = "Populist Dictionary") +
  scale_color_manual(values = c("#6BAED6", "#74C476", "royalblue", "mediumseagreen")) +
  theme_minimal() +
  theme(legend.position = "bottom") 

ggsave("poplangtimecount.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)

#### Average no. of populist words ####

# Function to calculate average counts

calculate_average <- function(dfm, year_month) {
  # Calculating the document lengths, ntoken already computes over documents by default
  document_lengths <- quanteda::ntoken(dfm)
  # Calculating the sum of the populism counts, handling NA with na.rm = TRUE
  populism_counts <- rowSums(dfm, na.rm = TRUE)
  # Calculating the average counts, handling division by zero if any document length is 0
  average_counts <- populism_counts / ifelse(document_lengths == 0, NA, document_lengths)
  # Replacing NA in average_counts caused by division by zero
  average_counts[is.na(average_counts)] <- 0
  # Combining into a data frame
  data.frame(Date = year_month, Populism_Avg_Count = average_counts)
}

# Calculating average count for each dictionary

avg_plot_data1 <- calculate_average(dfm_lookup(dfm_amlo, dictionary = anti_elit1), year_month)
avg_plot_data1$Type <- "Original Anti-Elitism"

avg_plot_data2 <- calculate_average(dfm_lookup(dfm_amlo, dictionary = anti_elit2), year_month)
avg_plot_data2$Type <- "Expanded Anti-Elitism"

avg_plot_data3 <- calculate_average(dfm_lookup(dfm_amlo, dictionary = ppl_cent1), year_month)
avg_plot_data3$Type <- "Original People-Centrism"

avg_plot_data4 <- calculate_average(dfm_lookup(dfm_amlo, dictionary = ppl_cent2), year_month)
avg_plot_data4$Type <- "Expanded People-Centrism"

# Combine into a single df

combined_avg_plot_data <- rbind(avg_plot_data1, avg_plot_data2, avg_plot_data3, avg_plot_data4)

# Plotting

ggplot(combined_avg_plot_data, aes(x = Date, y = Populism_Avg_Count, color = Type)) +
  geom_smooth() + 
  labs(title = "Average Populist Language Over Time", x = "Year", y = "Average Count of Populist Terms",
         color = "Populist Dictionary") +
  scale_color_manual(values = c("#6BAED6", "#74C476", "royalblue", "mediumseagreen")) +
  theme_minimal() +
  theme(legend.position = "bottom") 

ggsave("poplangtime.png",
       width = 8, height = 4,
       device = "png",
       dpi = 300)