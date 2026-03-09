##########################################################
# DACSS 758 SP26
# Week 6 R Lab
# Prof. Irene Morse
##########################################################

# very useful reference: https://tutorials.quanteda.io/

library("quanteda")
library("quanteda.textstats")
library("quanteda.textplots")
library("stringr")
library("ggplot2")
library("RColorBrewer")

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
# create bigrams for Mr. Blah, Mrs. Blah, Miss Blah
prpr_tokens <- tokens_compound(prpr_tokens, pattern = phrase("Mr *"))
prpr_tokens <- tokens_compound(prpr_tokens, pattern = phrase("Mrs *"))
prpr_tokens <- tokens_compound(prpr_tokens, pattern = phrase("Miss *"))

prpr_tokens[10:20]

# create document feature matrix
prpr_dfm <- dfm(prpr_tokens)
print(prpr_dfm)

# calculate sparsity
sparsity <- sum(prpr_dfm == 0) / length(prpr_dfm)
print(sparsity)

# bar plot
most_common <- textstat_frequency(prpr_dfm, n=50)
ggplot(most_common, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Frequency", y = "Feature")

# word cloud
set.seed(2026) # set seed for reproducibility
textplot_wordcloud(prpr_dfm,
                   min_size = 0.5,
                   max_size = 4,
                   min_count = 3,
                   max_words = 200,
                   color = brewer.pal(8, "Dark2"))
# color palettes: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html

# key words in context
darcy_df <- kwic(prpr_tokens, "mr_darcy", window = 5)
View(darcy_df)

# feature co-occurence matrix
smaller_dfm <- dfm_trim(prpr_dfm, min_termfreq = 10)
fcm <- fcm(smaller_dfm)
dim(fcm)

# pull the top 30 features
myFeatures <- names(sort(colSums(fcm), decreasing = TRUE)[1:30])
# retain only those top features as part of our matrix
smaller_fcm <- fcm_select(fcm, pattern = myFeatures, selection = "keep")
# check dimensions
dim(smaller_fcm)

print(smaller_fcm)

# find what co-occurs with feature of interest
smaller_fcm[,which(colnames(smaller_fcm)=="mr_darcy")]

# represent co-occurring features as a network
size <- log(colSums(smaller_fcm))
textplot_network(smaller_fcm, vertex_size = size/ max(size) * 3)

# randomly sample docs for reading
sample(prideprej, 5)

# changing unit of analysis
# sentences instead of quasi-sentences
prideprej_split <- unlist(strsplit(prideprej, split = "(?<=[\\.])", perl=TRUE))
sentence <- vector()
prideprej_sents <- vector()
for(qs in prideprej_split){
  if(substr(qs, nchar(qs), nchar(qs)+1)!="."){
    # not the end of the sentence
    sentence <- paste(sentence, qs)
  }else if(substr(qs, nchar(qs), nchar(qs)+1)=="."){
    # end of the sentence
    sentence <- paste(sentence, qs)
    prideprej_sents <- c(prideprej_sents, sentence)
    sentence <- vector()
  }
}
length(prideprej)
length(prideprej_sents)
prideprej_sents[1:10]

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
length(prideprej)
length(prideprej_chaps)
prideprej_chaps[1]
prideprej_chaps[2]
prideprej_chaps[3]

##########################################################
