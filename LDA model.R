#read the data----
setwd("/Users/lanceyu/Dropbox (BI Norwegian Business School)/Course/PhD course/session 2")
library(readxl)
dd <- read_excel("20231024_Reddit_NewTubers.xlsx")

colSums(is.na(dd))
dd[is.na(dd)] <- "none"

library(quanteda)
corp <- corpus(dd$Post)
# Tokenize a post.
train.tokens <- tokens(dd$Post, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

# Take a look at a specific post and see how it transforms.
train.tokens[[3]]

# Lower case the tokens.
train.tokens <- tokens_tolower(train.tokens)
train.tokens[[3]]

# Use quanteda's built-in stopword list for English.
# NOTE - You should always inspect stopword lists for applicability to
#        your problem/domain.
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")
train.tokens <- tokens_select(train.tokens, c("human","chatbot","bot","í","t"), 
                              selection = "remove")
train.tokens[[3]]
# Perform stemming on the tokens.
train.tokens <- tokens_wordstem(train.tokens, language = "english")
train.tokens[[3]]
# Create our first bag-of-words model.
dfm <- dfm(train.tokens, tolower = FALSE)
dfm = dfm_trim(dfm, min_docfreq = 2)

# find the topics number (may not work for short text)
library(ldatuning)
result <- ldatuning::FindTopicsNumber(
  dfm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# perform lda
library(topicmodels)
dtm = convert(dfm, to = "topicmodels") 
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 5,  control = list(alpha = 0.1))
m

words = posterior(m)$term
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)
terms(m, 10)

# show some posts in topic 5
# Get the topic-document probabilities
topic_docs <- posterior(m)$topics[, 1]
# Sort the probabilities in descending order
sorted_topic_docs <- sort(topic_docs, decreasing = TRUE)
# Extract the names of the top 20 documents
top_20_docnames <- names(sorted_topic_docs)[1:20]
# Retrieve and print the texts of these top 20 documents from the original corpus
for (docname in top_20_docnames) {
  specific_doc <- corp[docnames(corp) == docname]
  cat("Document:", docname, "\n")
  cat("Text:", texts(specific_doc), "\n\n")
}

# visualization
library(LDAvis)   
dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)
