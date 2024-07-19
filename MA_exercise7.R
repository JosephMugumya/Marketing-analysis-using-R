rm(list=ls())

Hubspot <- read.csv("Hubspot_tweets.csv", sep =";", stringsAsFactors=FALSE,
                    encoding ='UTF-8')
str(Hubspot)
Hubspot$text[4]

Hubspot <- read.csv("Hubspot_tweets.csv", sep =";", stringsAsFactors=FALSE,
                    encoding ='latin1')

str(Hubspot)
Hubspot$text[4]

Marketo <- read.csv("Marketo_tweets.csv", sep =";", stringsAsFactors=FALSE,
                    encoding ='latin1')
str(Marketo)
install.packages("tm", dependencies=TRUE)
library(tm)

# lets start with Hubspot , Create the corpus
Hubspot.vec<- VectorSource(Hubspot$text)
str(Hubspot.vec)

textcorpus <- VCorpus(Hubspot.vec)
inspect(textcorpus)
textcorpus[[82]]$content

#create custom function for pre-processing

clean_corpus <- function(corpus){
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, content_transformer(tolower))
  corpus<-tm_map(corpus, removeNumbers)
  corpus<-tm_map(corpus, content_transformer(stemDocument))
  corpus<-tm_map(corpus, removeWords, c(stopwords("en")))
  return (corpus)
}

# use function
textcorpus2 <- clean_corpus(textcorpus)

textcorpus[[4]]$content
textcorpus2[[4]]$content

# lets go!
#simple word Cloud
install.packages("wordcloud")
library(wordcloud)


wordcloud(textcorpus2, max.words = 100, colors="darkcyan")

#start over for comparing Marketo and Hubspot

Hubspot.vec <- paste(Hubspot$text, collapse = " ")
Marketo.vec <- paste(Marketo$text, collapse = " ")

Both <- c(Hubspot.vec, Marketo.vec)
str(Both)

comparison_corpus <- VCorpus(VectorSource(Both))
inspect(comparison_corpus)


comparison_corpus <-clean_corpus(comparison_corpus)

#create a term document Matrix

comparison_tdm <- TermDocumentMatrix(comparison_corpus)
inspect(comparison_tdm)

# as matrix

ctmm <- as.matrix(comparison_tdm)

colnames(ctmm) <- c('Hubspot', 'Marketo')
head (ctmm)

# commonality cloud
library(wesanderson)
commonality.cloud(ctmm, random.order = FALSE, max.words = 100,
                  colors = wes_palette("Zissou1", n=5, type='discrete'))


#comparison cloud
comparison.cloud(ctmm, title.size= 3, max.words = 100)


####
#sentiment analysis
#lets continue with the same term document matrix

install.packages("tidytext", dep = TRUE)
library(tidytext)

lex <- get_sentiments("afinn")
lex2 <-get_sentiments("bing")
lex3 <-get_sentiments("nrc")

head(ctmm)

library(dplyr)

comparison_SA <- tibble(word=row.names(ctmm),
                        Hubspot_frq = ctmm[,1],
                        Marketo_frq = ctmm[,2])
str(comparison_SA)

#combine comparison_SA with lex

# we could do this in steps
comparison_afinn <- inner_join(comparison_SA, lex, by="word")


# lets practise the pipe operators %>%
comparison_afinn <- comparison_SA %>%
  inner_join(lex, by = "word") %>%
  mutate(sentiment_Hu = Hubspot_frq*value,
         sentiment_Ma = Marketo_frq*value)

hist(comparison_afinn$sentiment_Hu)
hist(comparison_afinn$sentiment_Ma)

#Overallsentiments

SeAn <- comparison_afinn%>%
  summarise(overall_Hu = sum(sentiment_Hu)/sum(Hubspot_frq),
            overall_Ma = sum(sentiment_Ma)/sum(Marketo_frq))
SeAn

#PLOT THIS

library(tidyr)

SeAnPlot <- SeAn %>%
  gather(Firm, Value, overall_Hu:overall_Ma)


library(ggplot2)

p<- ggplot(SeAnPlot, aes(Firm, Value))
p + geom_bar(stat = "identity", fill = "darkcyan")+
  ggtitle("overall sentiments") +
  scale_x_discrete(labels = c("Hubspot", "Marketo"))


# TOPIC MODELING
#This time we need the document term matrix
cdtm <- DocumentTermMatrix(comparison_corpus)
inspect(cdtm)

#lets start by estimating 

install.packages("ldatuning")
library(ldatuning)


HowMany <- FindTopicsNumber(
  cdtm,
  topics = seq(from =2, to =10, by = 1),
  metrics = c("CaoJuan2009", "Arun2010","Deveaud2014"),
  method = "VEM",
  control = list(seed =1234),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(HowMany)


install.packages("topicmodels")
library(topicmodels)

Lda_topics <- LDA(cdtm, k=2, control = list(seed=1234))

str(Lda_topics)


# lets start wit the betas
TTP <- tidy(Lda_topics, matrix ='beta')
  
  lda_top_terms <- TTP %>%
    group_by(topic) %>%
    top_n(10,beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  lda_top_terms %>%
    mutate(term =reorder_within(term, beta, topic))%>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free")+
    scale_x_reordered()+
    coord_flip()+
    ggtitle("Word topic probabilities")

# Gammas
  
  DTP <- tidy(Lda_topics, matrix ="gamma")



