##########################################################
# DACSS 758 SP26
# Week 7 R Lab
# Prof. Irene Morse
##########################################################

library("quanteda")
library("stringr")
library("word2vec")
library("umap")
library("plotly")
library("text2vec")

##########################################################

# Working with local embeddings

# read in text data
prideprej <- readLines("C:/Users/irene/Documents/UMass DACSS/DACSS 758/Data/prideprejudice.txt")
prideprej[10:20]

# define corpus and preprocess
prideprej <- str_remove_all(prideprej, "_")
prpr_corpus <- corpus(prideprej)
prpr_tokens <- tokens(prpr_corpus, 
                      remove_numbers = TRUE,  
                      remove_punct = TRUE,
                      remove_url = TRUE,
                      remove_symbols = TRUE)
# remove stop words
prpr_tokens <- tokens_select(prpr_tokens, 
                             stopwords("en"), 
                             selection = "remove" )

prpr_tokens[10:20]

# format for word2vec
prpr_long <- lapply(prpr_tokens, paste, collapse = " ")
prpr_long <- tolower(prpr_long)

# word2vec using cbow vs. skip-gram
set.seed(2026)
cbow_model = word2vec(x=prpr_long, type="cbow", 
                      dim=50, iter = 20)
skip_gram_model = word2vec(x=prpr_long, type="skip-gram", 
                      dim=50, iter = 20)

# cosine similarity
predict(cbow_model, c("sister"), type="nearest", top_n=10)
predict(skip_gram_model, c("sister"), type="nearest", top_n=10)
# examine embedding
predict(cbow_model, c("sister"), type="embedding")
predict(skip_gram_model, c("sister"), type="embedding")
# cosine similarty
predict(cbow_model, c("darcy"), type="nearest", top_n=10)
predict(skip_gram_model, c("darcy"), type="nearest", top_n=10)
# examine embedding
predict(cbow_model, c("darcy"), type="embedding")
predict(skip_gram_model, c("darcy"), type="embedding")

# visualization of word embeddings

# create a dtm (tm package)
prpr_dfm <- dfm(prpr_tokens)
prpr_dtm <- convert(prpr_dfm, to="tm")

# create a list of words
words <- colnames(as.matrix(prpr_dtm))
word_list <- strsplit(words, " ")
word_list <- unlist(word_list)
word_list <- word_list[word_list != ""]  # remove empty strings
num_words <- 50  # number of words to plot
word_list = head(word_list, num_words)
print(word_list)

# get embeddings for word list
chosen_model <- cbow_model
chosen_model <- skip_gram_model
chosen_embedding <- as.matrix(chosen_model)
chosen_embedding <- predict(chosen_model, word_list, type="embedding")
chosen_embedding <- na.omit(chosen_embedding)

# dimensionality reduction for visualization
visualization <- umap(chosen_embedding, n_neighbors = 15, n_threads = 2)

# format data for visualization
df  <- data.frame(word = rownames(chosen_embedding), 
                  xpos = gsub(".+//", "", rownames(chosen_embedding)), 
                  x = visualization$layout[, 1], y = visualization$layout[, 2], 
                  stringsAsFactors = FALSE)

# visualize word list
plot_ly(df, x = ~x, y = ~y, type = "scatter", mode = 'text', text = ~word) %>%
  layout(title = "Embeddings Visualization")

# vector algebra
chosen_model <- cbow_model
chosen_model <- skip_gram_model
family_embedding <- predict(chosen_model, c("family"), type="embedding")
marry_embedding <- predict(chosen_model, c("marry"), type="embedding")
new_embedding <- family_embedding - marry_embedding
predict(chosen_model, new_embedding, type="nearest", top_n = 5)

##########################################################

# Working with pre-trained embeddings
# This code is provided FYI only
# I ran the code for >5 hrs on my personal laptop (8 cores, 8 GB RAM)
# The training process still did not complete within that time frame
# Therefore the training process seems prohibitively time consuming

# Step 1: Choose a very large text dataset and download it
# Some nice options: https://nlp.stanford.edu/projects/glove/

# Step 2: Load in the downloaded dataset and train embeddings
# Wikipedia corpus
wiki = readLines("C:/Users/irene/Downloads/wiki_giga_2024_50.txt", warn = FALSE)

# Create iterator over tokens
tokens <- space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 5L)

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(rank = 50, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)

wv_context = glove$components
word_vectors = wv_main + t(wv_context)

# Step 3: Perform algebraic operations with resulting word vectors
berlin <- word_vectors["paris", , drop = FALSE] - 
  word_vectors["france", , drop = FALSE] + 
  word_vectors["germany", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

##########################################################
