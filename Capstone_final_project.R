Sys.setenv(JAVA_HOME='C:\\Users\\Thinkpad\\Documents\\java\\')
library(NLP)
library(stringi)
library(ngram)
library(parallel)


dataUrl<- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("SwiftKey.zip")){
        download.file(dataUrl, "SwiftKey.zip", method = "libcurl")
        unzip("SwiftKey.zip")
}

blogs <- readLines(data1 <- file("./final/en_US/en_US.blogs.txt"), 
                   warn = FALSE, skipNul= TRUE, encoding = "UTF-8")
close(data1)
twitter <- readLines(data2 <- file("./final/en_US/en_US.twitter.txt"),
                     warn = FALSE, skipNul = TRUE, encoding = "UTF-8")
close(data2)


mysample <- list()
sampleblogs <- sample(blogs, length(blogs)*0.3)
sampletwitter <- sample(twitter, length(twitter)*0.2)
mysample <- c(sampleblogs, sampletwitter)
writeLines(mysample, "./sample/mysample.txt")
directory <- file.path(".", "sample")


corpus <- VCorpus(DirSource(directory))
for(i in seq(corpus)){
        corpus[[i]]<-gsub("(f|ht)tp(s?)://(.*)[.][a-z]+"," ",corpus[[i]])
        corpus[[i]]<-gsub("\\S+@\\S+"," ",corpus[[i]])
        corpus[[i]]<-gsub("#[a-z0-9]+"," ",corpus[[i]])
        corpus[[i]]<-gsub("@\\w+"," ",corpus[[i]])
        corpus[[i]]<-gsub("[^('.)[:alnum:] ]"," ",corpus[[i]])
        corpus[[i]]<-gsub("[()]"," ",corpus[[i]])
        corpus[[i]]<-gsub("[\\.']{2,}"," ",corpus[[i]])
        corpus[[i]]<-gsub("\\s+"," ",corpus[[i]])
        corpus[[i]]<-gsub("\\s+$"," ",corpus[[i]])
}
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)


tdm_Ngram <- function (corpus, n) {
        NgramTokenizer <- function(x) {
                RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))
                }
        tdm <- TermDocumentMatrix(corpus, control = list(tokenizer = NgramTokenizer))
        tdm
        }

ngram_sorted_df <- function (tdm) {
        tdm_m <- as.matrix(tdm)
        tdm_df <- as.data.frame(tdm_m)
        colnames(tdm_df) <- "Count"
        tdm_df <- tdm_df[order(-tdm_df$Count), , drop = FALSE]
        tdm_df
        }

tdm_1 <- tdm_Ngram(corpus, 1)
tdm_2 <- tdm_Ngram(corpus, 2)
tdm_3 <- tdm_Ngram(corpus, 3)
tdm_4 <- tdm_Ngram(corpus, 4)

tdm_1_df <- ngram_sorted_df(tdm_1)
tdm_2_df <- ngram_sorted_df(tdm_2)
tdm_3_df <- ngram_sorted_df(tdm_3)
tdm_4_df <- ngram_sorted_df(tdm_4)



quadgram <- data.frame(rows=rownames(tdm_4_df),count=tdm_4_df$Count)
quadgram$rows <- as.character(quadgram$rows)
quadgram_split <- strsplit(as.character(quadgram$rows),split=" ")

quadgram <- transform(quadgram,
                      first = sapply(quadgram_split,"[[",1),
                      second = sapply(quadgram_split,"[[",2),
                      third = sapply(quadgram_split,"[[",3), 
                      fourth = sapply(quadgram_split,"[[",4))

quadgram <- data.frame(unigram = quadgram$first,
                       bigram = quadgram$second, 
                       trigram = quadgram$third, 
                       quadgram = quadgram$fourth, 
                       freq = quadgram$count,stringsAsFactors=FALSE)

write.csv(quadgram[quadgram$freq > 1,],"./MyApp/quadgram.csv",row.names=F)
quadgram <- read.csv("./MyApp/quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"./MyApp/quadgram.RData")


trigram <- data.frame(rows=rownames(tdm_3_df),count=tdm_3_df$Count)
trigram$rows <- as.character(trigram$rows)
trigram_split <- strsplit(as.character(trigram$rows),split=" ")

trigram <- transform(trigram,
                     first = sapply(trigram_split,"[[",1),
                     second = sapply(trigram_split,"[[",2),
                     third = sapply(trigram_split,"[[",3))

trigram <- data.frame(unigram = trigram$first,
                      bigram = trigram$second, 
                      trigram = trigram$third, 
                      freq = trigram$count,stringsAsFactors=FALSE)

write.csv(trigram[trigram$freq > 1,],"./MyApp/trigram.csv",row.names=F)
trigram <- read.csv("./MyApp/trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"./MyApp/trigram.RData")

bigram <- data.frame(rows=rownames(tdm_2_df),count=tdm_2_df$Count)
bigram$rows <- as.character(bigram$rows)
bigram_split <- strsplit(as.character(bigram$rows),split=" ")

bigram <- transform(bigram,
                    first = sapply(bigram_split,"[[",1),
                    second = sapply(bigram_split,"[[",2))

bigram <- data.frame(unigram = bigram$first,
                     bigram = bigram$second,
                     freq = bigram$count,stringsAsFactors=FALSE)

write.csv(bigram[bigram$freq > 1,],"./MyApp/bigram.csv",row.names=F)
bigram <- read.csv("./MyApp/bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"./MyApp/bigram.RData")






























