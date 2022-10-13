
setwd("D:/BSCIT-SEM5/ADS(data science)/practice model")

readLines("sample.txt")
str(readLines("sample.txt"))
st = paste(readLines("sample.txt"),collapse = " ")
st
st2 = gsub(pattern = "\\W",replace=" ",st)
st2
st2 = gsub(pattern = "\\d",replace=" ",st)
st2
st2 = tolower(st2)
st2
install.packages("tm")
library(tm)
st2 = removeWords(st2,stopwords())
st2
st2 = gsub(pattern = "\\b[A-z]\\b[1]",replace = " ",st2)
st2
st2 = stripWhitespace(st2)
st2
install.packages("stringr")
library(stringr)
st2words = str_split(st2,pattern = "\\st")
st2words
pos = readLines("poswords.txt")
pos
class(st2words)
st2words
st2words = unlist(st2words)
class(st2words)
match(st2words,pos)
!is.na(match(st2words,pos))
sum(!is.na(match(st2words,pos)))
neg = readLines("negwords.txt")
neg
match(st2words,neg)
!is.na(match(st2words,neg))
sum(!is.na(match(st2words,neg)))
install.packages("wordcloud")
library(wordcloud)
score = sum(!is.na(match(st2words,pos))) - sum(!is.na(match(st2words,neg)))
score
wordcloud(st2words)
wordcloud(st2words,min.freq = 2)
wordcloud(st2words,min.freq = 2,random.order = F)
wordcloud(st2words,min.freq = 2,random.order = F,colors = rainbow(7))