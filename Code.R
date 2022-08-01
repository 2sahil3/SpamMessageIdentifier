library(readr)
library(tm)
library(SnowballC)
library(wordcloud)
library(e1071)
library(gmodels)
library(NLP)
library(RColorBrewer)

sms_raw <- read_csv("D:\\Sahil\\VIT STUDIES\\SY\\DS\\course project\\dataset.csv")




#converting type column to a factor.
sms_raw$type <-factor(sms_raw$type) 
table(sms_raw$type)


sms_corpus <- VCorpus(VectorSource(sms_raw$text))
inspect(sms_corpus[1:2])

# To view the actual text message 
as.character(sms_corpus[[1]])

# To view multiple messages 
lapply(sms_corpus[1:2], as.character)



# Convert to lower case.
clean_data <- tm_map(sms_corpus, content_transformer(tolower))
as.character(clean_data[[1]])

# Remove numbers
clean_data <- tm_map(clean_data, removeNumbers)

# Stopwords
clean_data <- tm_map(clean_data, removeWords, stopwords())

# Punctuation
clean_data <- tm_map(clean_data, removePunctuation)

# Stemming
clean_data <- tm_map(clean_data, stemDocument)

# Strip additional whitespace
clean_data <- tm_map(clean_data, stripWhitespace)

# Create a document-term matrix 
sms_dtm <- DocumentTermMatrix(clean_data)

# short way to clean directly  with dtm
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# Create a training and testing set.

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

# Save a pair of vectors with labels for each row in the training and testing matrices
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

# view the percentage of ham and spam.
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels)) # 13% spam in both, training and testing data.

# Data Visualization
wordcloud(clean_data, min.freq = 50, random.order = FALSE)

# Compare cloud of spam/ham
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")


Encoding(spam$text) <- "UTF-8"
wordcloud(spam$text, max.words = 50, scale = c(5, 0.5), colors = "#AD1DA5")
wordcloud(ham$text, max.words = 50, scale = c(3, 0.5), colors = brewer.pal(7, "Accent"))
