## TO PREDICT WHETHER CUSTOMER WILL RECOMMEND THE PRODUCT BASED REVIEW
library(tm)  #Text Mining
library(SnowballC) 
library(dplyr)

#Importing dataset
data=read.csv("C:/Users/ACE/Desktop/data/Womens Clothing E-Commerce Reviews.csv", 
              stringsAsFactors = FALSE)
names(data)

data[[2]][1]

#Getting subset of data (data[rows, cols])
sub_data <- data[c(0:550), c(2, 5, 7)]
glimpse(sub_data)

#Creating corpus (Collection of documents)
corpus = Corpus(VectorSource(sub_data$Review.Text))
corpus[[2]][1]
sub_data$Recommended.IND[1]

#Converting words to lower case
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus[[2]][1]  

#Removing punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[2]][1]

#Removing stop words
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))
corpus[[2]][1]  

#Stemming
corpus = tm_map(corpus, stemDocument)
corpus[[2]][1]

#Creating DTM which describes frequency of terms in the documents
#rows - documents in the collection and columns - terms.
frequencies = DocumentTermMatrix(corpus)
glimpse(frequencies)
#Removing Sparsity (Zeros)
sparse = removeSparseTerms(frequencies, 0.995)
sparse

#Creating dataframe
tSparse = as.data.frame(as.matrix(sparse))
#making col names suitable for R
colnames(tSparse) = make.names(colnames(tSparse))
#Adding target variable
tSparse$recommended_id = sub_data$Recommended.IND
tSparse
prop.table(table(tSparse$recommended_id)) #=> data mostly consists of recommended
#Making 0 -> not recommended, 1 -> recommended
tSparse$recommended_id <- factor(tSparse$recommended_id, levels=c(0,1), 
                                 labels=c("not recommended", "recommended"))

#Rpart is used for building classification trees
library(rpart)
#Rpart.plot - to plot the decision tree
library(rpart.plot)

#Creating train & test set 70-30 ratio
set.seed(1234) #to get same result at every run
ind <- sample(2, nrow(tSparse), replace=TRUE, prob=c(0.7, 0.3))
trainSparse <- tSparse[ind==1,]
testSparse <- tSparse[ind==2,]

#Fitting decision tree to data (method = class => binary model)
tree = rpart(recommended_id ~ ., data=trainSparse, method="class")
print(tree)

#Plotting decision tree
rpart.plot(tree, extra = 100, nn = TRUE)
#rpart.rules(tree)