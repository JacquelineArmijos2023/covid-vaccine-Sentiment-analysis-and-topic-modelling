# data repository
getwd()
setwd("C:/Users/jgarm/Documents/Computational Literacy/Covid-19 vaccines Sentiment analysis and LDA/")

# Libraries
library(dplyr)
library(lubridate)
library(stringr)
library(tm)
library(quanteda)
library(vader) 
library(xlsx)
library(ggplot2)
library(topicmodels)
library(wordcloud)
library(nortest)
library(ggwordcloud)

# read file

vaccine0 <- read.csv(file = "covidvaccine.csv", sep = ",", header = TRUE, na.strings = "")
View(vaccine0)

# data preprocess

## lost data

sum(is.na(vaccine0))
colSums(is.na(vaccine0))

## filter

vaccine <- select(vaccine0, date, user_location, text)
View(vaccine)

colSums(is.na(vaccine))

## type of variable

str(vaccine)

vaccine1 <- filter(vaccine,!str_detect(date, "Covid"))

vaccin1 <- na.omit(vaccine1) %>% mutate(dat = as.Date(date, format = "%y-%m-%d")) %>%
  filter(dat >= as.Date("2020-01-01") & dat <= as.Date("2022-09-20"))
vaccin2 <- na.omit(vaccine1) %>% mutate(dat = as.Date(date, format = "%d-%m-%y")) %>%
  filter(dat >= as.Date("2020-01-01") & dat <= as.Date("2022-09-20"))

max(vaccin1$dat)
max(vaccin2$dat)

final_data_vaccine <- rbind.data.frame(vaccin1, vaccin2)

colSums(is.na(final_data_vaccine))

# Plot
plot(table(final_data_vaccine$dat), type = "l")

# Text 

text_vaccine <- filter(final_data_vaccine, nchar(final_data_vaccine$user_location) >=6 & nchar(final_data_vaccine$user_location) <=20)
dim(text_vaccine)
text_vaccine$text
barplot(table(text_vaccine$user_location))

filter_text_vaccine <- gsub("http.+ |http.+$", "", text_vaccine$text) %>% tolower() %>%
  str_replace_all("[^[:alnum:][:space:]]","") %>% str_replace_all("[^A-Za-z0-9]"," ")



rm_text_number <- removeNumbers(filter_text_vaccine)

text_contents <- filter(data.frame(rm_text_number),str_detect(rm_text_number,c("covid", "vaccine")))

infinitive_text <- stemDocument(text_contents$rm_text_number)
length(infinitive_text)
stopwords("en")[30:60]


vaccine_copr <- corpus(infinitive_text)

# sentiment analysis

sentiment <- vader_df(vaccine_copr)

sentiment_text <- sentiment %>% mutate(sentiment = if_else((compound >-0.05 & compound <0.05),"Neutral",
                                                           if_else(compound <= -0.05,"Negative",
                                                                   if_else(compound >=0.05,"Possitive",""))))

hist(sentiment$compound)

# Normality test for compound

lillie.test(sentiment$compound)
shapiro.test(sentiment$compound)
pearson.test(sentiment$compound)


ggplot(sentiment_text, aes(x = compound)) + geom_histogram(aes(y = ..density..),colour = 1, fill = "green") + 
  geom_density() + ggtitle("Histogram of compound score", subtitle = "Sentiment Analisys with VADER") +
  theme(plot.title=element_text(size=20, hjust = 0.5))

ggplot(filter(sentiment_text, sentiment == "Negative"), aes(x = compound)) + geom_histogram(aes(y = ..density..),colour = 1, fill = "green") + 
  geom_density() + ggtitle("Histogram of sentiment Negative", subtitle = "Sentiment Analisys with VADER") +
  theme(plot.title=element_text(size=20, hjust = 0.5))
ggplot(filter(sentiment_text, sentiment == "Possitive"), aes(x = compound)) + geom_histogram(aes(y = ..density..),colour = 1, fill = "green") + 
    geom_density() + ggtitle("Histogram of sentiment Possitive", subtitle = "Sentiment Analisys with VADER") +
    theme(plot.title=element_text(size=20, hjust = 0.5))
ggplot(filter(sentiment_text, sentiment == "Neutral"), aes(x = compound)) + geom_histogram(aes(y = ..density..),colour = 1, fill = "green") + 
    geom_density() + ggtitle("Histogram of sentiment Neutral", subtitle = "Sentiment Analisys with VADER") +
    theme(plot.title=element_text(size=20, hjust = 0.5))

lillie.test(filter(sentiment_text, sentiment == "Negative")$compound)
lillie.test(filter(sentiment_text, sentiment == "Possitive")$compound)
lillie.test(filter(sentiment_text, sentiment == "Neutral")$compound)


ggplot(sentiment_text, aes(x = 1, y = compound)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot( color = "blue", fill = "gray") +
  ggtitle("BoxPlot of compound score", subtitle = "Sentiment Analisys with VADER") +
  theme(plot.title=element_text(size=20, hjust = 0.5))

ggplot(sentiment_text, aes(x = sentiment, y = compound, fill = sentiment)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() +
  ggtitle("BoxPlot of compound score by Sentiment", subtitle = "Sentiment Analisys with VADER") +
  theme(plot.title=element_text(size=20, hjust = 0.5))

# Doughnut chart

ss <- data.frame(table(sentiment_text$sentiment)) %>% setNames(c("sentiment", "Freq"))

ggplot(ss, aes(x = 2, y = Freq, fill = sentiment)) +
  geom_col() + coord_polar(theta = "y") +
  geom_label(aes(label = paste0(round(100* Freq/nrow(sentiment_text),2), "%")),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) + xlim(c(0.2, 2 + 0.5)) + theme_void() +
  ggtitle("Doughnut chart compound score", subtitle = "Sentiment Analisys with VADER") +
  theme(plot.title=element_text(size=20, hjust = 0.5))

#write.xlsx(x = sentiment_text, file = "Sentiment_analysis_visualization.xlsx")
#write.xlsx(table(sentiment_text$sentiment), file = "Sentiment_analysis_visualization.xlsx", sheetName = "sentimentPlot", append = TRUE)

# Sentiment


dtext <- rbind(filter(sentiment_text, sentiment == "Negative")[sample(1:nrow(filter(sentiment_text, sentiment == "Negative")),size=15,replace=FALSE),],
filter(sentiment_text, sentiment == "Possitive")[sample(1:nrow(filter(sentiment_text, sentiment == "Possitive")),size=29,replace=FALSE),],
filter(sentiment_text, sentiment == "Neutral")[sample(1:nrow(filter(sentiment_text, sentiment == "Neutral")),size=56,replace=FALSE),])

#write.xlsx(x = dtext, file = "Sample_Sentiment.xlsx")

manual_text <- read.xlsx("Sample_Sentiment.xlsx", sheetName = "Sheet1")

clasif_text <- table(manual_text$sentiment, manual_text$Read)

#Sorting quality

sum(diag(clasif_text))/sum(clasif_text)

#Error in classification
1- sum(diag(clasif_text))/sum(clasif_text)

#

vaccine_resh <- corpus_reshape(vaccine_copr, to = "paragraphs")

vaccine_rem <- dfm(vaccine_resh, remove_punct = TRUE, remove = c("s",stopwords("english")))
vaccine_freq <- dfm_trim(vaccine_rem, min_docfreq = 10)

vaccine_convert <- convert(vaccine_freq, to = "topicmodels")

vaccine_LDA <- LDA(vaccine_convert, method = "VEM", k = 4, control = list(alpha = 0.1, seed = 1111))


for(i in 1:vaccine_LDA@k) {
  vaccine_terms <- data.frame(term = vaccine_LDA@terms, p = exp(vaccine_LDA@beta[i,]))
  
  pw <- ggwordcloud(words = vaccine_terms$term, freq = vaccine_terms$p, max.words = 200, random.order = FALSE,
              colors=brewer.pal(8, "Dark2"))
 
  print(pw)  
  
}


# Possitive

possitive_sentim <- filter(sentiment_text, sentiment == "Possitive")

possitive_corp <- corpus_reshape(possitive_sentim$text, to = "paragraphs")

possitive_rem <- dfm(possitive_corp, remove_punct = TRUE, remove = c("s",stopwords("english")))

possitive_freq <- dfm_trim(possitive_rem, min_docfreq = 10)

possitive_convert <- convert(possitive_freq, to = "topicmodels")

set.seed(99)

possitive_LDA <- LDA(possitive_convert, method = "VEM", k = 4, control = list(alpha = 0.1, seed = 1111))

for(i in 1:possitive_LDA@k) {
  possitive_terms <- data.frame(term = possitive_LDA@terms, p = exp(possitive_LDA@beta[i,]))
  pw <- ggwordcloud(words = possitive_terms$term, freq = possitive_terms$p, max.words = 200, random.order = FALSE,
                    colors=brewer.pal(8, "Dark2"))
  
  ggsave(pw, file=paste0("Possitive Topic", i, ".jpeg"), #Nombre del jpeg
         height = 3, width = 6, dpi=300) 
}

# Negative

negative_sentim <- filter(sentiment_text, sentiment == "Negative")

negative_corp <- corpus_reshape(negative_sentim$text, to = "paragraphs")

negative_rem <- dfm(negative_corp, remove_punct = TRUE, remove = c("d","th","sa","o","s",stopwords("english")))

negative_freq <- dfm_trim(negative_rem, min_docfreq = 2, max_docfreq = 5)

negative_convert <- convert(negative_freq, to = "topicmodels")



negative_LDA <- LDA(negative_convert, method = "VEM", k = 4, control = list(alpha = 0.1, seed = 1111))

for(i in 1:negative_LDA@k) {
  negative_terms <- data.frame(term = negative_LDA@terms, p = exp(negative_LDA@beta[i,]))
  pw <- ggwordcloud(words = negative_terms$term, freq = negative_terms$p, max.words = 200, random.order = FALSE,
                    colors=brewer.pal(8, "Dark2"))
  
  ggsave(pw, file=paste0("Negative Topic", i, ".jpeg"), #Nombre del jpeg
         height = 3, width = 6, dpi=300) 
  
}
# Neutral

neutral_sentim <- filter(sentiment_text, sentiment == "Neutral")

neutral_corp <- corpus_reshape(neutral_sentim$text, to = "paragraphs")

neutral_rem <- dfm(neutral_corp, remove_punct = TRUE, remove = c("s",stopwords("english")))

neutral_freq <- dfm_trim(neutral_rem, min_docfreq = 10)

neutral_convert <- convert(neutral_freq, to = "topicmodels")

set.seed(99)

neutral_LDA <- LDA(neutral_convert, method = "VEM", k = 4, control = list(alpha = 0.1, seed = 1111))

for(i in 1:neutral_LDA@k) {
  neutral_terms <- data.frame(term = neutral_LDA@terms, p = exp(neutral_LDA@beta[i,]))
  pw <- ggwordcloud(words = neutral_terms$term, freq = neutral_terms$p, max.words = 200, random.order = FALSE,
                    colors=brewer.pal(8, "Dark2"))
  
  ggsave(pw, file=paste0("Neutral Topic", i, ".jpeg"),
         height = 3, width = 6, dpi = 300)
  
  
}

