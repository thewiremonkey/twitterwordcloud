rm(list=ls())

library("twitteR")
library("tm")
library("ggplot2")
library("reshape2")
library("wordcloud")
library("RWeka")
library("dplyr")
library("rJava")

options(mc.cores=2)
if(getwd()!="C:/Users/Marketing/Documents/twitter"){setwd("C:/Users/Marketing/Documents/twitter")}
#set the search term.  If you are using more than one, either add a plus sign (+) between them or put them in a list c("term1", "term2")
term <- "unga"

#you can set your own color scheme, or use default values.  I set this up with Wonder Woman colors for my Wondy cloud.
colors <- c("blue", "red", "gold", "lightslategrey")

#set authorization tokens for twitter api 
#go to dev.twitter.com to create an app and get authorization codes.
setup_twitter_oauth(consumer_key = "sAjD0s4T5415LZQ7LSiLsvWJS",
                    consumer_secret = 	"bxUxU1u7uRuO7Ao5H6tkFiPO2lMI960p79trQCGPriKd4HEsqW",
                    access_token = "15231924-OA0BMnK6Vs1xpTgP99N9IAuM52o5XBTmUypw4Sgxm",
                    access_secret = "VA7vEj3fm5yyGa7XkMBQn9ptAWwCwfuK5XH6LOMJ4R74N"
)

#run the search using the term. Twitter rate limits, but it is sometimes possible to push the n number.  This can take a while, be patient.
tweets <- searchTwitter(term, n = 999,
                        resultType = "mixed",lang = "en")


# #uncomment if you want to strip out retweets
# tweets <- strip_retweets(tweets, strip_mt = T, strip_manual=T)

#turn tweets into a data frame with the twitteR function twListToDF.  
#tweets need to be in a data frame in order to process them as a corpus for the wordcloud.
tweets.df <- twListToDF(tweets)


#start cleaning!
#remove the RT at the beginning of retweets. I found this to be particularly stubbon and finally decided to gsub it outside fo the clean_text function.
tweets.df$text<-gsub("^RT ", "", tweets.df$text, ignore.case=FALSE)

tweets.df$text<-iconv(tweets.df$text, from = "UTF8", to = "windows-1250")

#clean_text function swiped from http://technokarak.com/how-to-clean-the-twitter-data-using-r-twitter-mining-tutorial.html
clean_text = function(x)
{
  x = gsub("@\\w+ *", "", x) # remove at(@)
  # x = gsub("RT ", "", x, ignore.case = T)
  x = gsub("[[:punct:]]", "", x) # remove punctuation
  x = gsub("[[:digit:]]", "", x) # remove numbers/Digits
  x = gsub("http\\w+", "", x)  # remove links http
  x = gsub("[ |\t]{2,}", "", x) # remove tabs
  x = gsub("^ ", "", x)  # remove blank spaces at the beginning
  x = gsub(" $", "", x) # remove blank spaces at the end
  try.error = function(z)
    #To convert the text in lowercase
  {
    y = NA
    try_error = tryCatch(
      tolower(z),
      error = function(e)
        e
    )
    if (!inherits(try_error, "error"))
      y = tolower(z)
    return(y)
  }
  x = sapply(x, try.error)
  return(x)
}

tweets.df$text <- clean_text(tweets.df$text)
tweets.df$text<-gsub(" amp ", " and ", tweets.df$text)

term<-clean_text(term)

#If I'm running a key word several times I generate a random number, rn, so that my data doesn't get overwritten.

rn <- sample(1:5000, 1)

#not necessary to save the data, but I like to hang onto it for further exploration.
write.csv(tweets.df, paste0(rn, "cloud.csv"))

#using the tm package, transform the tweets.df$text column to a corpus
t_corp <- Corpus(VectorSource(tweets.df$text))


#clean corpus"
#remove your search term or words (be sure to put them in a list if there is more than one), otherwise your cloud will be overwhelmed with your search term and the underlying trends will be obscured.
hashremove <- term

#more cleaning.  You can learn about this via the tm package. Many tutorials online.
t_corp <- tm_map(t_corp, removePunctuation)
t_corp <- tm_map(t_corp, content_transformer(tolower))
t_corp <- tm_map(t_corp, function(x)
  removeWords(x, stopwords()))
t_corp <- tm_map(t_corp, function(x)
  removeWords(x, c(hashremove)))

# if you want, you can process the text even more and create a document term matrix which is useful if you want to create bar charts or other types of visualizations, but it is not necessary to create a word cloud. The following commented out code is swiped from
# https://www.r-bloggers.com/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know/

dtm <- TermDocumentMatrix(t_corp)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

png(
      filename = paste0(rn, term, ".png", collapse = ""),
      type = "cairo",
      res = 300,
      width = 5,
      height = 5,
      units = "in"
)

#the magic via the wordcloud package
xcloud <-
      wordcloud(words = d$word,
                freq=d$freq,
                # max.words = 100,
                scale=c(6,.25),
                rot.per = 0.3,
                color = colors)

dev.off()


#I like to save the wordcloud down to a png file using cairo, it produces a nice clean image.  Wrap png() and dev.off() around your wordcloud code.
png(
  filename = paste0(rn, term, "2.png", collapse = ""),
  type = "cairo",
  res = 300,
  width = 5,
  height = 5,
  units = "in"
)

#the magic via the wordcloud package
xcloud <-wordcloud(t_corp,
                   # max.words = 100, #how many words you want to include
                   scale=c(4, .2),
                   rot.per = 0.3, #what percent you want to be rotated. in this case, 30%
                   color = colors)#use the colors set at the top of the script or leave blank.

dev.off()

#get rid of nulls or n-gram will fail
tweets.df<-tweets.df[!is.na(tweets.df$text),]

#create bigram corpus
b_corp<-VCorpus(VectorSource(tweets.df$text))
b_corp <- tm_map(b_corp, removePunctuation)
b_corp <- tm_map(b_corp, content_transformer(tolower))
b_corp <- tm_map(b_corp, function(x)
  removeWords(x, stopwords()))
b_corp <- tm_map(b_corp, function(x)
  removeWords(x, c(hashremove)))


BigramTokenizer <- function(x) NGramTokenizer(x, RWeka::Weka_control(min=2, max=2))

bi_TDM <- TermDocumentMatrix(b_corp, control=list(tokenize=BigramTokenizer))
bm<-as.matrix(bi_TDM)
bv = sort(rowSums(bm),decreasing = TRUE)
bd = data.frame(word=names(bv), freq=bv)
head(bd, 20)

#create trigram corpus

tri_corp<-VCorpus(VectorSource(tweets.df$text))
tri_corp <- tm_map(tri_corp, removePunctuation)
tri_corp <- tm_map(tri_corp, content_transformer(tolower))
tri_corp <- tm_map(tri_corp, function(x)
  removeWords(x, stopwords()))
tri_corp <- tm_map(tri_corp, function(x)
  removeWords(x, c(hashremove)))
TrigramTokenizer <- function(x) NGramTokenizer(x, RWeka::Weka_control(min=3, max=3))

tri_TDM <- TermDocumentMatrix(tri_corp, control=list(tokenize=TrigramTokenizer))
trm<-as.matrix(tri_TDM)
trv = sort(rowSums(trm),decreasing = TRUE)
trd = data.frame(word=names(trv), freq=trv)
head(trd, 20)


png(
  filename = paste0(rn, term, "bi.png", collapse = ""),
  type = "cairo",
  res = 300,
  width = 5,
  height = 5,
  units = "in"
)
bicloud<-wordcloud(words=bd$word, freq=bd$freq,
          max.words = 100, #how many words you want to include
          scale=c(4, .2),
          rot.per = 0.3, #what percent you want to be rotated. in this case, 30%
          color = colors)#use the colors set at the top of the script or leave blank.)

dev.off()


#generate trigram wordcloud
png(
  filename = paste0(rn, term, "tri.png", collapse = ""),
  type = "cairo",
  res = 300,
  width = 5,
  height = 5,
  units = "in"
)
tricloud<-wordcloud(words=trd$word, freq=trd$freq,
                    max.words = 75, #how many words you want to include
                    scale=c(4, .1),
                    rot.per = 0.3, #what percent you want to be rotated. in this case, 30%
                    color = colors)#use the colors set at the top of the script or leave blank.)

dev.off()
