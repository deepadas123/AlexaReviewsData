library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(data.table)     # fread tsv
library(dplyr)
library(ggplot2)
library("tm")
library("caTools") #sample.split for sampling
library(rpart)
library(rpart.plot)
library(ROCR)
library("randomForest")

# alexa %>% group_by(variation) %>% summarise(x = mean(rating))

alexa<-as.data.frame(fread("amazon_alexa.tsv"))
head(alexa)
alexa <- as.tibble(alexa)

# book=variation
# chapter=review.no
# 

# counting reviews for each variation
alexa %>% 
  group_by(variation) %>% 
  count() %>% 
  arrange(desc(n))

#selecting variations having reviews>200
alexa1 <- alexa %>%  filter(variation %in% c("Black  Dot","Charcoal Fabric", "Black  Dot",
 "White  Plus","White  Show", "White  Spot"))

  #"Configuration: Fire TV Stick","Black  Plus","Black  Show", "Black", "Black  Spot",

# creating an index for each review under each variation
alexa1 <- alexa1 %>% arrange(variation) %>% group_by(variation) %>% mutate(review.no=1:n()) %>%  ungroup() %>% 
  dplyr::select(variation, review.no, verified_reviews, rating )

# Sentiment Analysis using Bing -----------------------------------------------------------------------

alexa2 <- alexa1 %>%
    unnest_tokens(word, verified_reviews)  

alexa2$variation <- as.factor(alexa2$variation)

alexa2 %>%
  group_by(variation) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(variation, review.no = review.no , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         variation = factor(variation)) %>%
  ggplot(aes(review.no, sentiment, fill = variation)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ variation, ncol = 2, scales = "free_x")


# comparing sentiments using 3 lexicons ----------------------------------------------------

afinn <- alexa2 %>%
  group_by(variation) %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(variation, review.no) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(alexa2 %>%
                            group_by(variation) %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),
                          alexa2 %>%
                            group_by(variation) %>% 
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentiment %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(variation, method, review.no = review.no , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  dplyr::select(variation, review.no, method, sentiment)


bind_rows(afinn, 
          bing_and_nrc) %>%
  ungroup() %>%
  mutate(variation = factor(variation)) %>%
  ggplot(aes(review.no, sentiment, fill = method)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_grid(variation ~ method)



# Comparing sentiments - negative reviews ---------------------------------

afinn.neg <- alexa2 %>% filter(rating <3) %>% 
  group_by(variation) %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(variation, review.no) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc.neg <- bind_rows(alexa2 %>% filter(rating <3) %>% 
                            group_by(variation) %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),
                          alexa2 %>% filter(rating <3) %>% 
                            group_by(variation) %>% 
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentiment %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(variation, method, review.no = review.no , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  dplyr::select(variation, review.no, method, sentiment)


bind_rows(afinn.neg, 
          bing_and_nrc.neg) %>%
  ungroup() %>%
  mutate(variation = factor(variation)) %>%
  ggplot(aes(review.no, sentiment, fill = method)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_grid(variation ~ method)


# bigram analysis ---------------------------------------------------------

alexa3 <- alexa1 %>%
  unnest_tokens(bigram, verified_reviews, token = "ngrams", n = 2)  

# Common bigrams
alexa3 %>%
  count(bigram, sort = TRUE)


# removing stop words - only from word 1 and not word 2. Information lost - example: "love it"
#  if "it" is removed, "love" will automatically be removed
x <- alexa3 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word1 %in% c("echo", "prime", "black", "dot", "white", "alexa") ) %>%
  count(word1, word2, sort = TRUE) %>% filter(!is.na(word1))

head(x,15)

alexa3 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word1 %in% c("echo", "prime", "black", "dot", "white", "alexa")) %>%
  count(variation, word1, word2, sort = TRUE) %>% filter(!is.na(word1)) %>% 
  unite("bigram", c(word1, word2), sep = " ") %>%
  group_by(variation) %>% 
  top_n(10) %>%
  ungroup() %>%
ggplot(aes(reorder(bigram, n), n, fill = variation)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  facet_wrap(~ variation, ncol = 2, scales = "free") +
  coord_flip()


# by sentence -------------------------------------------------------------

# get sentiment of each review by adding up sentiment scores at sentence level

alexa_sentences <- alexa1 %>% unnest_tokens(sentence, verified_reviews, token = "sentences")
# text is verified_review

abc <- alexa_sentences %>%
  group_by(review.no) %>%
  mutate(sentence_num = 1:n()) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(variation, review.no) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  mutate(calcul.sentiment = ifelse(sentiment<=0,"negative", "non-negative"))

table(abc$calcul.sentiment)

abc1 <- abc %>%  left_join(alexa1, c("variation", "review.no")) %>%
  mutate(actual.sent = ifelse(rating<=2,"negative", "non-negative")) %>% 
  mutate(match= ifelse(actual.sent==calcul.sentiment,1,0))


table(abc1$actual.sent, abc$calcul.sentiment, dnn=c("Actual","Measured Sentiment"))


# negating words ----------------------------------------------------------

alexa3 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 == "not") %>%
  count(variation, word1, word2, sort = TRUE)


AFINN <- get_sentiments("afinn")

(nots <- alexa3 %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(word1 == "not") %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word2, score, sort = TRUE) 
)

nots %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  ggplot(aes(reorder(word2, contribution), n * score, fill = n * score > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Words preceded by 'not'") +
  ylab("Sentiment score multiplied by no. of occurrances") +
  coord_flip()




# looking at a number of negation words
negation_words <- c("not", "no", "without")

(negated <- alexa3 %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(word1 %in% negation_words) %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word1, word2, score, sort = TRUE) %>%
    ungroup()
)

negated %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ggplot(aes(reorder(word2, contribution), contribution, fill = contribution > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Words preceded by 'not'") +
  ylab("Sentiment score multiplied by no. of occurrances") +
  facet_wrap(~ word1, scales = "free") +
  coord_flip()


# We can use this information to see the total impact these cases had on misspecifying sentiment. For 
# example, above we saw that the top two words preceded by "not" was "like" and "impressed". The sentiment
# score for "like" is +2; however, "like" was preceded by "not" 9 times which means the sentiment could 
# easily have been overstated by 9 × 2 = 18 points. 



# preparing data for modeling ----------------------------------------------------------------

table(alexa$rating)
# table(alexa4$negative)

# under sampling. selecting limited number of 0's in a to reduce sparsity of negative comments
# selecting only 1000 positive reviews
a <- which(alexa$rating==5)
a <- a[1:1000]

# selecting all negative reviews
b <- which(alexa$rating==2 | alexa$rating==1)
d <- c(a,b) 

alexa4 <- alexa[d,]
alexa4 <-  alexa4 %>% mutate(negative = ifelse(rating <=2,1,0 ))

alexa4$negative <- as.factor(alexa4$negative)
 

alexa_reviews <- VectorSource(alexa4$verified_reviews) 
alexa_corpus <- VCorpus(alexa_reviews)
clean_corpus <- function(corpus) {
  #remove puntuation
  corpus <- tm_map(corpus, removePunctuation)
  #transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  #add stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  #strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

alexa_clean <- clean_corpus(alexa_corpus)
alexa_tdm  <- TermDocumentMatrix(alexa_clean)
alexa_m <- as.matrix(alexa_tdm)



clean_corpus <- function(corpus) {
  #remove puntuation
  corpus <- tm_map(corpus, removePunctuation)
  #transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  #add stopwords
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"amazon","echo",
                                          "music","speaker","dot","device",
                                          "devices","product","alexa","can",
                                          "one","use","just","get","set","still","bought","will"))
  #strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}



alexa_clean <- clean_corpus(alexa_corpus)
DTM  <- DocumentTermMatrix(alexa_clean)


# alexa_m <- as.matrix(alexa_tdm)
# term_frequency <- rowSums(alexa_m) %>% 
#   sort(decreasing = TRUE)

freq <- findFreqTerms(DTM, lowfreq = 20)

sparse_DTM <- removeSparseTerms(DTM, 0.995)

alexa_sparse <- as.data.frame(as.matrix(sparse_DTM))
colnames(alexa_sparse) <- make.names(colnames(alexa_sparse))

alexa_sparse$Negative <- alexa4$negative

dim(alexa_sparse)

# modeling ----------------------------------------------------------------

set.seed(123)


splitNegative <- sample.split(alexa_sparse$Negative, SplitRatio = 0.7)

trainSparseNegative <- subset(alexa_sparse, splitNegative == TRUE)
testSparseNegative <- subset(alexa_sparse, splitNegative == FALSE)



alexaCARTNegative <- rpart(Negative ~ . , data = trainSparseNegative, method = "class")
prp(alexaCARTNegative)


predictCARTNegative <- predict(alexaCARTNegative, newdata = testSparseNegative, type = "class")
cmat_CARTNegative <- table(testSparseNegative$Negative, predictCARTNegative, dnn=c("Actual","Predicted Sentiment"))
cmat_CARTNegative 
accu_CART <- (cmat_CARTNegative[1,1] + cmat_CARTNegative[2,2])/sum(cmat_CARTNegative)



# ROC ---------------------------------------------------------------------

credit.test.prob.rpart = predict(alexaCARTNegative,testSparseNegative, type="prob")

pred = prediction(credit.test.prob.rpart[,2], testSparseNegative$Negative)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))

# random forest -----------------------------------------------------------

set.seed(123)
alexaRF <- randomForest(as.factor(Negative) ~ . , data = trainSparseNegative)

predictRFN <- predict(alexaRF, newdata = testSparseNegative)

cmat_RFN <- table(testSparseNegative$Negative, predictRFN)
accu_RFN <- (cmat_RFN[1,1] + cmat_RFN[2,2])/sum(cmat_RFN)

# ROC and AUC
alexapredict<- predict(alexaRF, testSparseNegative,type = "prob")[,2]
pred <- prediction(alexapredict, testSparseNegative$Negative)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))


# plot(alexaRF, lwd=rep(2, 3))
# legend("right", legend = c("OOB Error", "FPR", "FNR"), lwd=rep(2, 3), lty = c(1,2,3), col = c("black", "red", "green"))
# 

# pcut off ----------------------------------------------------------------

# not used
alexapredict<- predict(alexaCARTNegative, type = "prob")[,2]

costfunc = function(obs, pred.p, pcut){
  weight1 = 5   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 1    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FP)
  cost = mean(weight1*c1 + weight0*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
} 
p.seq = seq(0.01, 0.5, 0.01)
cost = rep(0, length(p.seq))  
for(i in 1:length(p.seq)){ 
  cost[i] = costfunc(obs = trainSparseNegative$Negative, pred.p = alexapredict, pcut = p.seq[i])  
}
plot(p.seq, cost)



