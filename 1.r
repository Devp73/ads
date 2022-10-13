install.packages('officer') 
install.packages('dplyr') 
install.packages('tm') 
install.packages('ggplot2') 
install.packages('wordcloud') 
require(officer)#Access word documents
require(dplyr)#Manipulate Data
require(tm) #Text Mining
require(ggplot2)#Data Visualization
require(wordcloud)#create word cloud
sample_data <- paste(readLines("meditation-1.txt"),collapse = " ") 
content = sample_data

#data cleaning remove special characters and white spaces
content = gsub("[[:punct:]]", "", content)
content = stripWhitespace(content)


#content <- docx_summary(sample_data) 
#read text from the content variable 
#paragraphs <- content %>% filter(content_type == "paragraph") 
#Doc_Data<-paragraphs$text # Access the actual text 
#Doc_Data
#A corpus is a collection of texts, written or spoken, usually stored in a database.
# convert the vector Doc_Data to a corpus
new_corpus <- Corpus(VectorSource(content)) 
word.tdm <- TermDocumentMatrix(new_corpus) 
inspect(word.tdm[1:10,]) # Examine 10 words at a time 

#Examine the frequently appearing words in the term document matrix
FrequentTerms <- findFreqTerms(word.tdm, lowfreq = 5, highfreq = Inf) 

#Convert term document matrix to data frame
word.tdm <- TermDocumentMatrix(new_corpus)
m <- as.matrix(word.tdm) 
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) 


#set.seed(10000) 
#wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1, 
          max.words=500, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Set1")) 

#graphical view
barplot(d[1:11,]$freq, las = 2, names.arg = d[1:11,]$word, 
        col ="pink", main ="Most frequent words", 
        ylab = "Word frequencies")

#star view
wordcloud2(d,0.7,shape="star")

#word find
d[d$word == "life",]


