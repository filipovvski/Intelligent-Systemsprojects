library(tm)
library(dplyr)
library(ggplot2)
library(proxy)
library(wordcloud)
library(utf8)
library(decoder)
library(devtools)
library(openNLPmodels.en)
library(NLP)
library(openNLP)
detach("package:ggplot2", unload=TRUE)
#install.packages("decoder")
#install_github("vqv/ggbiplot")

#rm(list=ls())


setwd("c:/users/filipowicz/desktop/przedmioty/intelligent Systems/project 2/kowal")

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

unescape_unicode <- function(x){
  #single string only
  stopifnot(is.character(x) && length(x) == 1)
  
  #find matches
  m <- gregexpr("(\\\\)+u[0-9a-z]{4}", x, ignore.case = TRUE)
  
  if(m[[1]][1] > -1){
    #parse matches
    p <- vapply(regmatches(x, m)[[1]], function(txt){
      gsub("\\", "\\\\", parse(text=paste0('"', txt, '"'))[[1]], fixed = TRUE, useBytes = TRUE)
    }, character(1), USE.NAMES = FALSE)
    
    #substitute parsed into original
    regmatches(x, m) <- list(p)
  }
  
  x
}


unescape_unicode2 <- function(x){
  #single string only
  stopifnot(is.character(x) && length(x) == 1)
  
  #find matches
  m <- gregexpr("(\\\\)+x[0-9a-z]{2}", x, ignore.case = TRUE)
  
  if(m[[1]][1] > -1){
    #parse matches
    p <- vapply(regmatches(x, m)[[1]], function(txt){
      gsub("\\", "\\\\", parse(text=paste0('"', txt, '"'))[[1]], fixed = TRUE, useBytes = TRUE)
    }, character(1), USE.NAMES = FALSE)
    
    #substitute parsed into original
    regmatches(x, m) <- list(p)
  }
  
  x
}

# train
train = read.table('train.tsv', header=T, sep="\t", colClasses=c("factor", "character"),quote = "", encoding="UTF-8" , fileEncoding = "UTF-8", dec=".")
#train$text_a <- gsub(".*/","", train$text_a)


for (x in 1:2220){
  train$text_a[x] <- unescape_unicode(train$text_a[x])
  train$text_a[x] <- unescape_unicode2(train$text_a[x])
}

train$text_a <- cleanFun(train$text_a)
train$text_a <- gsub("http.*","", train$text_a)
train$text_a <- gsub("(\n)","", train$text_a)

train$text_a[1]



texts = train %>% pull('text_a')
labels = train %>% pull('label')
corpus <- Corpus(VectorSource(texts))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

conn = file("english.stop.txt", open="r")
mystopwords = readLines(conn)
close(conn)

corpus <- tm_map(corpus, removeWords, mystopwords)
#corpus <- tm_map(corpus, stemDocument)   
corpus <- tm_map(corpus, stripWhitespace)

corpus[[1]]$content
corpus[[44]]$content

for (x in 1:50){
  print(corpus[[x]]$content)
}

#test

test = read.table('test.tsv', header=T, sep="\t", colClasses=c("factor", "character"),quote = "", encoding="UTF-8" , fileEncoding = "UTF-8", dec=".")
#train$text_a <- gsub(".*/","", train$text_a)


for (x in 1:987){
  test$text_a[x] <- unescape_unicode(test$text_a[x])
  test$text_a[x] <- unescape_unicode2(test$text_a[x])
}

test$text_a <- cleanFun(test$text_a)
test$text_a <- gsub("http.*","", test$text_a)
test$text_a <- gsub("(\n)","", test$text_a)

texts_test = test %>% pull('text_a')
labels_test = test %>% pull('label')
corpus_test <- Corpus(VectorSource(texts_test))

corpus_test <- tm_map(corpus_test, content_transformer(tolower))
corpus_test <- tm_map(corpus_test, removePunctuation)
corpus_test <- tm_map(corpus_test, removeNumbers)
corpus_test <- tm_map(corpus_test, removeWords, stopwords('english'))

conn = file("english.stop.txt", open="r")
mystopwords = readLines(conn)
close(conn)

corpus_test <- tm_map(corpus_test, removeWords, mystopwords)
#corpus <- tm_map(corpus, stemDocument)   cos tu sie pierdoli
corpus_test <- tm_map(corpus_test, stripWhitespace)

for (x in 1:50){
  print(corpus_test[[x]]$content)
}




tdm <- TermDocumentMatrix(corpus)
tdm
findFreqTerms(tdm, lowfreq=50)
tdm


termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency >= 50)
qplot(seq(length (termFrequency)),sort(termFrequency), xlab = "index", ylab = "Freq")
termFrequency

# Convert the term-document matrix to a normal matrix and calculate word frequencies
mat <- as.matrix(tdm)
wordFreq <- sort(rowSums(mat), decreasing=TRUE)
grayLevels <- gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=50, random.order=F, colors=grayLevels)

findAssocs(tdm, "dont", 0.2)
corpus[[1]]$content


tdm2 <- removeSparseTerms(tdm, sparse=0.7)
mat <- as.matrix(tdm2)
distMatrix <- dist(mat)
distMatrix


dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 2
k <- 2
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n") 

}

dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 4 
k <- 4
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
  
}


dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 3 
k <- 8
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
  
}


corpus[44]$content
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 3 
k <- 16
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
  
}
length(kmeansResult)

# Read the document classes

# Visualize mat via decomposition (e.
length(mat)
text.pca <- prcomp(mat)
dim(text.pca$x)
comp1 <- as.numeric(text.pca$x[,1])
comp2 <- as.numeric(text.pca$x[,2])

length(comp2)
cos <- factor(kmeansResult$cluster)
qplot(comp1, comp2, colour= cos)
qplot(comp1, comp2, colour= labels)

which(kmeansResult$cluster %in% TRUE)
cos

which(kmeansResult$cluster == 2)

kmeansResult$cluster == 2
cos[cos == 2]

install.packages('M3C')
library(M3C)
tsne(pollen$data,colvec=c('gold'))

content(corpus[[1]])
content(corpus[[2]])
dist(as.matrix(dtm)[c(1,2),], method = "cosine")
dtm2
# Find the most similar documents in the term-document matrix (this may take a few moments to compute...)
dtm2 <- removeSparseTerms(dtm, sparse=0.8)
mat <- as.matrix(dtm2)
dist.mat <- as.matrix(dist(mat, method = "cosine"))
sim.idx <- which(dist.mat == min(dist.mat[dist.mat > 0]), arr.ind = TRUE)
sim.idx
dist(mat[c(sim.idx[1,1], sim.idx[1,2]),], method = "cosine")
content(corpus[[sim.idx[1,1]]])
content(corpus[[sim.idx[1,2]]])



#POS tags
corpus[[1]]$content
s <- as.String(corpus$content)
s
sent_ann <- Maxent_Sent_Token_Annotator()
sent_ann
a1 <- annotate(s, sent_ann)
a1

word_ann <- Maxent_Word_Token_Annotator()
word_ann
a2 <- annotate(s, word_ann, a1)
a2

# Extract words
a2w <- subset(a2, type == "word")
s[a2w]

pos_ann <- Maxent_POS_Tag_Annotator()
pos_ann
a3 <- annotate(s, pos_ann, a2)
a3

a3w <- subset(a3, type == "word")
a3w

# Extract token/POS pairs

tags <- vector()
tags
for (i in 1:length(a3w$features))
  tags <- c(tags, a3w$features[[i]]$POS)

table(tags)
tags

tokenPOS <- cbind(s[a3w], tags)
tokenPOS


#Punkt 3
  #1. 

table(labels)

  #2.

labels <- data.frame(train$label)
labels
table(labels)

#
# Transforming the corpus into a data set
#

# Construct a document-term matrix

corpus[[1]]$content
data.tfidf <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
text.mat <- as.matrix(data.tfidf)

# Read the document classes
table(labels)

# Construct a data set
data <- cbind(text.mat, labels)
names(data)[nrow(data)] <- "labels"

train <- data
train<-sapply(train,function(x) as.numeric(as.character(x)))

#test

labels_test <- data.frame(test$label)
labels_test
table(labels_test)

data_test.tfidf <- DocumentTermMatrix(corpus_test, control = list(weighting=weightTfIdf))
text_test.mat <- as.matrix(data_test.tfidf)

# Read the document classes
table(labels)

# Construct a data set
data_test <- cbind(text_test.mat, labels_test)
names(data_test)[nrow(data_test)] <- "labels"

test <- data_test
test<-sapply(test,function(x) as.numeric(as.character(x)))

#
# Document classification using SVM
#
library(class)
library(kernlab)
#knn
predicted <- knn(train, test, labels)
observed <- test$Topic
t <- table(observed, predicted)
t

# svm with a radial basis kernel
model.svm <- ksvm(labels ~ ., train, kernel = "rbfdot")
predicted <- predict(model.svm, test, type = "response")
t <- table(observed, predicted)
t

head(train)

  #3

#decision tree
train <- data.frame(train)
library(rpart)
dt <- rpart(labels ~ ., data = train)
plot(dt);text(dt)
predicted <- predict(dt, test, type="class")
CA(observed, predicted)