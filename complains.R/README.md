# Load required libraries
library(tidyverse)   
library(reshape2)    
library(ggplot2)     
library(tm)          
library(wordcloud)   
library(RColorBrewer)
library(syuzhet)     
library(tidytext)    
library(textdata)    

setwd("~/Documents/r_patient/data")  

# Load the dataset
file_path <- "Consumer_Complaints.csv"
df <- read_csv(file_path, show_col_types = FALSE)

# DATA CLEANING
df_clean <- df %>%
  select(Product, Issue, `Consumer complaint narrative`, Company) %>%
  filter(!is.na(Product) & !is.na(Issue) & !is.na(`Consumer complaint narrative`))  # Remove missing values

# 1. Bar Chart - Top 10 Complaints

top_complaints <- df_clean %>%
  count(Issue, sort = TRUE) %>%
  top_n(10, n)

# Get min/max complaints
min_complaint <- min(top_complaints$n)
max_complaint <- max(top_complaints$n)

ggplot(top_complaints, aes(x = reorder(Issue, -n), y = n, fill = Issue)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Consumer Complaints",
       x = "Complaint Issue", 
       y = "Count",
       subtitle = paste("Min:", min_complaint, "| Max:", max_complaint)) +
  theme_minimal()

# 2. Bar Chart - Complaint Distribution by Product (Replacing Pie Chart)

product_counts <- df_clean %>%
  count(Product, sort = TRUE) %>%
  top_n(5, n)

ggplot(product_counts, aes(x = reorder(Product, -n), y = n, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "Complaint Distribution by Product", x = "Product", y = "Number of Complaints") +
  theme_minimal()


# 3. Histogram - Complaint Frequency by Company

company_complaints <- df_clean %>%
  count(Company, sort = TRUE) %>%
  top_n(15, n)

ggplot(company_complaints, aes(x = reorder(Company, -n), y = n, fill = Company)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 15 Companies with Most Complaints", x = "Company", y = "Number of Complaints") +
  theme_minimal()


# 4. Word Cloud - Common Words in Complaints

text_data <- df_clean$`Consumer complaint narrative` %>% na.omit()

# Convert to VCorpus to avoid warnings
corpus <- VCorpus(VectorSource(text_data))

# Apply text transformations
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Create term-document matrix
tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)

# Convert to dataframe
df_words <- data.frame(word = names(word_freqs), freq = word_freqs)

# Filter for meaningful words
df_words <- df_words %>% filter(freq > 10)

# Generate the word cloud
set.seed(1234)
wordcloud(words = df_words$word, freq = df_words$freq, min.freq = 5,
          max.words = 100, random.order = FALSE, rot.per = 0.3,
          colors = brewer.pal(8, "Dark2"))


# 5. Sentiment Analysis - Bing & NRC

df_tokens <- df_clean %>%
  unnest_tokens(word, `Consumer complaint narrative`) %>%
  anti_join(stop_words)

# Bing Sentiment Analysis
bing_sentiment <- df_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE)

ggplot(bing_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Bing Sentiment Analysis of Complaints",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()

# NRC Sentiment Analysis
nrc_sentiment <- df_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = TRUE)

ggplot(nrc_sentiment, aes(x = reorder(sentiment, -n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "NRC Sentiment Analysis of Complaints",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()

# Pivot Table - Complaints by Product & Company

pivot_table <- df_clean %>%
  count(Product, Company) %>%
  dcast(Product ~ Company, value.var = "n", fill = 0)

# Print first few rows of the pivot table
print(head(pivot_table))

