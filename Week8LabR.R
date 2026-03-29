##########################################################
# DACSS 758 SP26
# Week 8 R Lab
# Prof. Irene Morse
##########################################################

library("quanteda")
library("tidytext")  # contains several dictionaries
library("stringr")

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

# create dfm
prpr_dfm <- dfm(prpr_tokens)
prpr_dfm

# what dictionaries are available?
# you may need to download them
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("nrc")

# filter dfm down to words in bing dictionary
bing <- get_sentiments("bing")
dfm_bing <- dfm_select(prpr_dfm, pattern = bing$word,
                       selection = "keep")
dfm_bing
prpr_dfm

# weight dfm using bing dictionary
# + for positive and - for negative
bing_weights <- ifelse(bing$sentiment=="negative", -1, 1)
names(bing_weights) <- bing$word
bing_weights
dfm_bing_weighted <- dfm_weight(dfm_bing, weights = bing_weights)
dfm_bing_weighted

# check out most positive and most negative docs
valences <- rowSums(dfm_bing_weighted)
which.max(valences)
prideprej[8751]
which.min(valences)
prideprej[1141]

# plot valences
boxplot(valences)
mean(valences)

# check out most used positive and negative words
word_sums <- colSums(dfm_bing_weighted)
head(sort(word_sums))
tail(sort(word_sums))

# let's try the nrc word-emotion association lexicon
# focus on joy
get_sentiments("nrc")
joy <- subset(get_sentiments("nrc"), sentiment=="joy")
head(joy)

# filter dfm down to words in bing dictionary
dfm_nrc_joy <- dfm_select(prpr_dfm, pattern = joy$word,
                          selection = "keep")
dfm_nrc_joy

# check out most joyful docs
joyful_docs <- rowSums(dfm_nrc_joy)
which.max(joyful_docs)
prideprej[8751]

# check out most used joyful words
word_sums <- colSums(dfm_nrc_joy)
tail(sort(word_sums))

# plot joy in novel over time
plot(x=1:length(joyful_docs), y=joyful_docs,
     xlab="Quasi-Sentence", ylab="Joy Score")
lines(x=1:length(joyful_docs), y=joyful_docs)

# change unit of analysis
# chapters instead of quasi-sentences
prideprej_split <- unlist(strsplit(prideprej, split = "(?<=Chapter\\s\\d{1,2})", perl=TRUE))
chapter <- vector()
prideprej_chaps <- vector()
for(qs in prideprej_split){
  if(str_detect(qs, "Chapter")){
    # beginning of the chapter
    prideprej_chaps <- c(prideprej_chaps, chapter)
    chapter <- vector()
    chapter <- paste(chapter, qs)
  }else if(!str_detect(qs, "Chapter")){
    # not the beginning of the chapter
    chapter <- paste(chapter, qs)
  }
}
length(prideprej_chaps)

# redo preprocessing
prideprej_chaps <- str_remove_all(prideprej_chaps, "_")
prpr_corpus <- corpus(prideprej_chaps)
prpr_tokens <- tokens(prpr_corpus, 
                      remove_numbers = TRUE,  
                      remove_punct = TRUE,
                      remove_url = TRUE,
                      remove_symbols = TRUE)
prpr_tokens <- tokens_select(prpr_tokens, 
                             stopwords("en"), 
                             selection = "remove" )
prpr_dfm <- dfm(prpr_tokens)
prpr_dfm

# redo weighting process
dfm_nrc_joy <- dfm_select(prpr_dfm, pattern = joy$word,
                          selection = "keep")
dfm_nrc_joy

# check out most joyful docs
joyful_docs <- rowSums(dfm_nrc_joy)

# plot joy in novel over time
plot(x=1:length(joyful_docs), y=joyful_docs, 
     xlab="Chapter", ylab="Joy Score")
lines(x=1:length(joyful_docs), y=joyful_docs)
abline(lm(joyful_docs~seq(1,length(joyful_docs),1)), col="hotpink")

##########################################################
