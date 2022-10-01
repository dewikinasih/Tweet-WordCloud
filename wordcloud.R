install.packages("textclean")
install.packages("devtools")
install.packages("tokenizers")
install.packages("stopwords")
install.packages("wordcloud")
install.packages('tm')
install.packages('rtweet')
install.packages('Rtools')


library(textclean)
library(dplyr)
library(tokenizers)
library(wordcloud)
library(tm)
library(devtools)
install_github("nurandi/katadasaR")
library(katadasaR)

library(rtweet)
token<- create_token(
  consumer_key = "IjKJaj2Nx4e9GJ9iWfO0TCNWD",
  consumer_secret = "AKoqpXcEkAPhTk681Ms0mZWFWocdrw8RBL0NpTrMCwHXiFxI2d",
  access_token = "1253611872449789952-n7QSrIhee0ZfEWBn0MrJAsaZFYxgY2",
  access_secret = "PNEuHb8TwFKNqEUBNU67mUGX4jZLVIwo6CWM3q4CD0TOU")

tweets <- search_tweets("presiden", n=100, lang='id', since='2022-02-20', until='2022-02-21')
colnames(tweets)
tweets[,c("created_at","screen_name","text")]

tweets1 <- tweets$text %>%
  as.character()
head(tweets1)

#remove substrings (\n)
tweets1 <- gsub("\n", "",tweets1)
head(tweets1)

#remove html&url
tweets1 <- tweets1 %>%
  replace_html() %>%
  replace_url()

#replace emoticons & emoji
replace_emoji(tweets1)
replace_html(replace_emoji(tweets1))

tweets1 <- tweets1 %>%
  replace_emoji(.) %>%
  replace_html(.)

#replace mention & hastags
replace_tag(tweets1)
tweets1 <- tweets1 %>%
  replace_tag(tweets1, pattern = "@([A-Za-z0-9_]+)", replacement = "") %>%
  replace_hash(tweets1, pattern = "#([A-Za-z0-9_]+)", replacement = "")
tweets1

#replace slang word
spell <- read.csv('colloquial-indonesian-lexicon.csv')
tweets1 <- replace_internet_slang(tweets1, slang = paste0("\\b", 
                                                          spell$slang, "\\b"),
                                  replacement = spell$formal, 
                                  ignore.case = TRUE)

#text stripping (ubah huruf kapital ke huruf kecil)
tweets1 <- strip(tweets1)

#untuk melihat apakah data memiliki teks duplikat
tweets1 <- tweets1 %>%
  as.data.frame() %>%
  distinct()
nrow(tweets1)

tweets1 <- as.character(tweets1$.)
stemming <- function(x){
  paste(lapply(x, katadasar), collapse = " ")}
tweets1 <- lapply(tokenize_words(tweets1[]), stemming)

#tokenize
tweets1 <- tokenize_words(tweets1)
head(tweets1, 3)

tweets1 <- Corpus(VectorSource(tweets1))
{
  dtm <- TermDocumentMatrix(tweets1)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing = TRUE)
  d <- data.frame(words = names(v), freq = v)
}
head(d, 20)

mystopword <- readLines('stopwords_list.txt')
tweets1 <- as.character(tweets1)
tweets1 <- tokenize_words(tweets1, stopwords = mystopword)
head(tweets1)

class(tweets1)
tweets1 <- as.character(tweets1)
wordcloud(tweets1, random.order = FALSE, colors = brewer.pal(12, "Paired"))
