##########################################################
# DACSS 758 SP26
# Week 9 R Lab
# Prof. Irene Morse
##########################################################
# Setup

library("quanteda")
library("topicmodels")
library("stm")

# read in text data
MP_text <- read.csv("C:/Users/irene/Documents/UMass DACSS/DACSS 758/Data/MP_text.csv")
head(MP_text)

# define corpus and preprocess
MP_corpus <- corpus(MP_text$original)
MP_tokens <- tokens(MP_corpus, 
                      remove_numbers = TRUE,  
                      remove_punct = TRUE,
                      remove_url = TRUE,
                      remove_symbols = TRUE)
# remove stop words
MP_tokens <- tokens_select(MP_tokens, 
                             stopwords("en"), 
                             selection = "remove" )

# create dfm
MP_dfm <- dfm(MP_tokens)

topfeatures(MP_dfm, 20)

##########################################################
# K-Means

# reduce dimensions of dfm for computational tractability
MP_dfm_smaller <- dfm_trim(MP_dfm, min_termfreq = 10)

set.seed(2026)
clustering.kmeans <- kmeans(MP_dfm_smaller, centers=4) 
print(clustering.kmeans)

View(clustering.kmeans$centers)

# top words for each cluster
head(sort(clustering.kmeans$centers[1,], decreasing=TRUE), 10)
head(sort(clustering.kmeans$centers[2,], decreasing=TRUE), 10)
head(sort(clustering.kmeans$centers[3,], decreasing=TRUE), 10)
head(sort(clustering.kmeans$centers[4,], decreasing=TRUE), 10)

##########################################################
# LDA
# Helpful Reference: https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

MP_dtm <- convert(MP_dfm, to ="topicmodels")

set.seed(2026)
lda_k4 <- LDA(MP_dtm, k=4, method="VEM", 
              control=NULL, model=NULL)

# examine top words per topic
terms(lda_k4, 10)

# examine top docs per topic
post_k4 <- posterior(lda_k4)
head(sort(post_k4$topics[,1], decreasing = TRUE))
head(sort(post_k4$topics[,2], decreasing = TRUE))
head(sort(post_k4$topics[,3], decreasing = TRUE))
head(sort(post_k4$topics[,4], decreasing = TRUE))
MP_corpus[34784]
MP_corpus[names(head(sort(post_k4$topics[,4], decreasing = TRUE)))]

##########################################################
# STM
# Helpful Reference: https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf

# drop empty documents
dfm_for_convert <- dfm_subset(MP_dfm, ntoken(MP_dfm) > 0)
empty_docs <- which(ntoken(MP_dfm) == 0)
MP_text_smaller <- MP_text[-empty_docs, ]
MP_stm <- convert(dfm_for_convert, to="stm")

# Run stm to look at changes over time
stm_k4_year <- stm(MP_stm$documents, 
                    vocab=MP_stm$vocab,
                    K=4,
                    prevalence=~year,
                    max.em.its=10,
                    #max.em.its=500,
                    data=MP_text_smaller,
                    init.type="Spectral",
                    seed=2026)

summary(stm_k4_year)

year_effect <- estimateEffect(1:4~year, 
                              stm_k4_year,
                              meta=MP_text_smaller,
                              uncertainty="Global",
                              nsims=10)

year_effect$parameters

plot(year_effect, covariate="year", topics=c(1:4),
     model=stm_k4_year, method = "difference",
     cov.value1 = "2017", cov.value2 = "2020",
     xlab = "2017 ... 2020",
     main="Effect of Year on Topic")

plot(year_effect, covariate="year", topics=c(1:4),
     model=stm_k4_year, method = "difference",
     cov.value1 = "2017", cov.value2 = "2020",
     xlab = "2017 ... 2020",
     main="Effect of Year on Topic",
     labeltype ="custom",
     custom.labels=c('maori','future','health','education'))

##########################################################
