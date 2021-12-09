
# this is demo script for text data mining
# 
# clear everything
rm(list = ls())
cname <- file.path("/Users/sven/Google Drive/Teaching/Data_Mining_2021/Practice_12/Texts")
dir(cname) #check loaded texts

# load package tm  which is framework for text data mining / NLP
library(tm)

# load available documents
docs <- VCorpus(DirSource(cname))   #corpus or set of texts
#print(summary(docs))

inspect(docs[2]) #some data about a loaded text - order 2
writeLines(as.character(docs[2])) #content of text 2

# Preprocessing
docs <- tm_map(docs,removePunctuation)   #remove punctuation symbols
docs <- tm_map(docs, removeNumbers)   #remove numbers
docs <- tm_map(docs, tolower)   #remove capitalization - to lowcase
docs <- tm_map(docs, removeWords, c("the", "and", stopwords("english")))   #remove common words such as "a, and, also, the"
docs <- tm_map(docs, removeWords, c("tennis", "football"))   #remove particular words
docs <- tm_map(docs, stripWhitespace)  #remove whitespaces from previous eliminations
#for (j in seq(docs)) {
#  docs[[j]] <- gsub("????", " ", docs[[j]])  #not well translated character
#}
docs <- tm_map(docs, PlainTextDocument)   #prepare de document as text


#Work with the data
#we use a Document-Term Matrix (DTM) representation: documents as the rows, terms/words as the columns, frequency of the term in the document as the entries. Because the number of unique words in the corpus the dimension can be large.
dtm <- DocumentTermMatrix(docs)   #create document-term matrix
dim(dtm)
print(dtm)

tdm <- TermDocumentMatrix(docs)   #creating the transpose of dtm
print(tdm)


#exploring data
freq <- colSums(as.matrix(dtm))   #organize terms by frequency
length(freq)

head(table(freq), 10) #check the less occuring words - top number is frequency of appearing, bottom number the number of words
tail(table(freq), 10) #check the most occurring words

dtms <- removeSparseTerms(dtm, 0.1) #remove sparse - less frequent items, matrix that is only 10% empty space
print(dtms)

freq <- colSums(as.matrix(dtms))   
freq   

# table after removing sparse terms
freq <- colSums(as.matrix(dtms))  
print(freq)

print(findFreqTerms(dtm, lowfreq=30)) #show words that appear 30 or more times

#plotting

# word frequencies
library(ggplot2)
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>50), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
print(p)

#or ordered...
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
print(p)  

#find correlations between terms
findAssocs(dtm, c("will" , "american"), corlimit=0.85) #find correlations between words

#cloud of words
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

set.seed(1234)
freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
wordcloud::wordcloud(rownames(freq), freq[,1], scale=c(4, .1), max.words=50, colors=brewer.pal(6, "Dark2")) #most frequently used 50 words

#plot words that occur at least 50 times
wordcloud::wordcloud(rownames(freq), freq[,1], scale=c(4, .1), min.freq=25, colors=brewer.pal(6, "Dark2"))  #words used at least 25 times or more

#clustering by term similarity
#hierarchical
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
print(dtmss)

library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="complete")   # for a different look try substituting: method="ward.D"
fit  
plot(fit, hang=-1) #plot

plot.new() #find number of clusters
plot(fit, hang=-1)
groups <- cutree(fit, k=3)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 6 clusters   

# K-means
library(fpc)
library(cluster)
dtms <- removeSparseTerms(dtm, 0.15)
d <- dist(t(dtms), method="manhattan")
kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

