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

sentiment_text <- sentiment %>% mutate(sentiment = if_else(compound ==0,"Neutral",
                                                           if_else(compound < 0,"Negative",
                                                                   if_else(compound >0,"Possitive",""))))

table(sentiment_text$sentiment)


#write.xlsx(x = sentiment_text, file = "Sentiment_analysis_visualization.xlsx")
#write.xlsx(table(sentiment_text$sentiment), file = "Sentiment_analysis_visualization.xlsx", sheetName = "sentimentPlot", append = TRUE)


vaccine_resh <- corpus_reshape(vaccine_copr, to = "paragraphs")

vaccine_rem <- dfm(vaccine_resh, remove_punct = TRUE, remove = c("s",stopwords("english")))
vaccine_freq <- dfm_trim(vaccine_rem, min_docfreq = 10)

vaccine_convert <- convert(vaccine_freq, to = "topicmodels")

vaccine_LDA <- LDA(vaccine_convert, method = "VEM", k = 5, control = list(alpha = 0.1))

terms(vaccine_LDA, 10)

vaccine_word <- posterior(vaccine_LDA)$terms[5,]
vaccine_sort <- sort(vaccine_word, decreasing = TRUE)
head(vaccine_sort)
wordcloud(names(vaccine_sort)[1:50], vaccine_sort[1:50])


# Possitive

possitive_sentim <- filter(sentiment_text, sentiment == "Possitive")

possitive_corp <- corpus_reshape(possitive_sentim$text, to = "paragraphs")

possitive_rem <- dfm(possitive_corp, remove_punct = TRUE, remove = c("s",stopwords("english")))

possitive_freq <- dfm_trim(possitive_rem, min_docfreq = 10)

possitive_convert <- convert(possitive_freq, to = "topicmodels")

set.seed(99)

possitive_LDA <- LDA(possitive_convert, method = "VEM", k = 5, control = list(alpha = 0.1))

terms(vaccine_LDA, 10)

possitive_word <- posterior(possitive_LDA)$terms[5,]
possitive_sort <- sort(possitive_word, decreasing = TRUE)
head(vaccine_sort)
wordcloud(names(possitive_sort), possitive_sort, colors = rainbow(100))


# Negative

negative_sentim <- filter(sentiment_text, sentiment == "Negative")

negative_corp <- corpus_reshape(negative_sentim$text, to = "paragraphs")

negative_rem <- dfm(negative_corp, remove_punct = TRUE, remove = c("d","th","sa","o","s",stopwords("english")))

negative_freq <- dfm_trim(negative_rem, min_docfreq = 2, max_docfreq = 5)

negative_convert <- convert(negative_freq, to = "topicmodels")



negative_LDA <- LDA(negative_convert, method = "VEM", k = 5, control = list(alpha = 0.1))

terms(vaccine_LDA, 10)

negative_word <- posterior(negative_LDA)$terms[5,]
negative_sort <- sort(negative_word, decreasing = TRUE)
head(vaccine_sort)
wordcloud(names(negative_sort), negative_sort, colors = rainbow(100))

# Neutral

neutral_sentim <- filter(sentiment_text, sentiment == "Neutral")

neutral_corp <- corpus_reshape(neutral_sentim$text, to = "paragraphs")

neutral_rem <- dfm(neutral_corp, remove_punct = TRUE, remove = c("s",stopwords("english")))

neutral_freq <- dfm_trim(neutral_rem, min_docfreq = 10)

neutral_convert <- convert(neutral_freq, to = "topicmodels")

set.seed(99)

neutral_LDA <- LDA(neutral_convert, method = "VEM", k = 5, control = list(alpha = 0.1))

terms(vaccine_LDA, 10)

neutral_word <- posterior(neutral_LDA)$terms[5,]
neutral_sort <- sort(neutral_word, decreasing = TRUE)
head(vaccine_sort)
wordcloud(names(neutral_sort), neutral_sort, colors = rainbow(100))
