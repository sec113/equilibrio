# Equilibrio
# Document Clustering
install.packages(c("reshape2","stringdist","tm","arules"))
# libs=c("reshape2","stringdist","tm","arules")

memory.limit(4.6)
# List of all libraries required 
libs=c("stringdist","tm","reshape2","proxy","RTextTools","wordcloud")

# Load the libraries
lapply(libs, require, character.only=TRUE)

# Remove libraries list
rm(libs)

# objetivo de revisar los fragmentos y agruparlos dejar de lado las opciones de los medicos y hacer un poco de 
# text mining.


#Load database with UTF encoding
full.df=read.csv("C:/Users/SergioMC/Documents/Equilibrio/Research/database2.csv",na="", encoding = "UTF-8")

# Temporary database for manipulation
mydf=full.df

# Get variables name
# variables_df=names(mydf)

# Select a subset of the database
mydf=subset(mydf, select=-c(Timestamp, learning, follow, learning.1, follow.1, learning.2, follow.2, learning.3, follow.3, learning.4, follow.4, learning.5, follow.5, learning.6, follow.6, learning.7, follow.7, learning.8, follow.8, learning.9, follow.9, learning.10, follow.10, learning.11, follow.11, learning.12, follow.12, learning.13, follow.13, learning.14, follow.14, learning.15, follow.15, learning.16, follow.16, learning.17, follow.17, learning.18, follow.18, learning.19, follow.19, learning.20, follow.20, learning.21, follow.21, learning.22, follow.22, learning.23, follow.23, learning.24, follow.24, learning.25, follow.25, learning.26, follow.26, learning.27, follow.27, learning.28, follow.28, learning.29, follow.29, learning.30, follow.30, learning.31, follow.31, learning.32, follow.32, learning.33, follow.33, learning.34, follow.34))

# Obtain the corpus
# body.corpus=melt(mydf, id.var = c('Terapeuta','number'), variable.name = 'annotated',na.rm = "TRUE")
mydf_melt=melt(mydf, id.var = c('Terapeuta','number'), variable.name = 'annotated',na.rm = "TRUE")
mydf_melt=as.array(unique(mydf_melt$value))

####################################################################################################

# make them all lower case
# body.corpus$value=tolower(body.corpus$value)
body.corpus=e.corpus

# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
ndocs=length(body.corpus)
minTermFreq=ndocs*0.01

# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- ndocs * .5


body.corpus=tm_map(body.corpus, content_transformer(tolower))

test_words = c("mmm", "y", "mmmm", "document","osea","pues","sino", "entonces","después", "ahí", "así", "ósea","creo","vez","cosa","digamos","siempre", "solo", "cosas","tan","tal","ahi","nunca","darme", "cuenta")

# All the normalization
body.corpus = tm_map(body.corpus, removeWords, c(test_words, stopwords(kind = "spanish")))
body.corpus = tm_map(body.corpus, removePunctuation)
body.corpus = tm_map(body.corpus,content_transformer(gsub),pattern = "x\ndocument\\d", replacement = "  ")

body.corpus = tm_map(body.corpus, removeNumbers)
body.corpus =  tm_map(body.corpus, stripWhitespace)
body.corpus  = tm_map(body.corpus, content_transformer(iconv),from="UTF-8", to='ASCII//TRANSLIT')


# Document Term Matrix based on frequency only
body.dtm = DocumentTermMatrix(body.corpus,control=list(bounds= list(global=c(minTermFreq))))

freq.body = data.frame(sort(colSums(as.matrix(body.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.body), freq.body[,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(3,.5))


# Begin Process for document clustering
# First we calculate distance between documents
body.dtm.matrix = as.matrix(body.dtm)

body.distMatrix = dist(body.dtm.matrix, method="euclidean")


# Cluster creation
freq.cluster= hclust(body.distMatrix,method="ward.D")

plot(freq.cluster, cex=0.9, hang=-1)
rect.hclust(freq.cluster,k =7)
output=data.frame(paste("document",names(body.corpus)),cutree(freq.cluster,k=7))
names(output)[1]="document"
names(output)[2]="cluster"
output$content=e.corpus$content
table(output$cluster)
write.csv(output,"C:/Users/SergioMC/Documents/Equilibrio/Research/clusters.csv")



## Create the clusters wordclouds
## Cluster 1
freq.cluster1 = subset(body.corpus, output$cluster == 1)

# Document Term Matrix based on frequency only
cluster1.dtm = DocumentTermMatrix(freq.cluster1,control=list(bounds= list(global=c(minTermFreq))))

freq.cluster1.dtm = data.frame(sort(colSums(as.matrix(cluster1.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.cluster1.dtm ), freq.cluster1.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.6))

## Cluster 2
freq.cluster2 = subset(body.corpus, output$cluster == 2)

# Document Term Matrix based on frequency only
cluster2.dtm = DocumentTermMatrix(freq.cluster2,control=list(bounds= list(global=c(minTermFreq))))

freq.cluster2.dtm = data.frame(sort(colSums(as.matrix(cluster2.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.cluster2.dtm ), freq.cluster2.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(3,.6))



## Cluster 3
freq.cluster3 = subset(body.corpus, output$cluster == 3)

# Document Term Matrix based on frequency only
cluster3.dtm = DocumentTermMatrix(freq.cluster3,control=list(bounds= list(global=c(minTermFreq))))

freq.cluster3.dtm = data.frame(sort(colSums(as.matrix(cluster3.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.cluster3.dtm ), freq.cluster3.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.6))

## Cluster 4

freq.cluster4 = subset(body.corpus, output$cluster == 4)

# Document Term Matrix based on frequency only
cluster4.dtm = DocumentTermMatrix(freq.cluster4,control=list(bounds= list(global=c(minTermFreq))))

freq.cluster4.dtm = data.frame(sort(colSums(as.matrix(cluster4.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.cluster4.dtm ), freq.cluster4.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.6))


## Cluster 5

freq.cluster5 = subset(body.corpus, output$cluster == 5)

# Document Term Matrix based on frequency only
cluster5.dtm = DocumentTermMatrix(freq.cluster5,control=list(bounds= list(global=c(minTermFreq))))

freq.cluster5.dtm = data.frame(sort(colSums(as.matrix(cluster5.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.cluster5.dtm ), freq.cluster5.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(1.5,.6))

## Cluster 6

freq.cluster6 = subset(body.corpus, output$cluster == 6)

# Document Term Matrix based on frequency only
cluster6.dtm = DocumentTermMatrix(freq.cluster6,control=list(bounds= list(global=c(minTermFreq))))

freq.cluster6.dtm = data.frame(sort(colSums(as.matrix(cluster6.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.cluster6.dtm ), freq.cluster6.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(1.5,.6))

## Cluster 7

freq.cluster7 = subset(body.corpus, output$cluster == 7)

# Document Term Matrix based on frequency only
cluster7.dtm = DocumentTermMatrix(freq.cluster7,control=list(bounds= list(global=c(minTermFreq))))

freq.cluster7.dtm = data.frame(sort(colSums(as.matrix(cluster7.dtm)), decreasing=TRUE))


# Wordcloud based on frequency only
wordcloud(rownames(freq.cluster7.dtm ), freq.cluster7.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.6))

#######################################################################################################################

### Document Term Matrix based on tf-idf

body.dtm.tfidf = DocumentTermMatrix(body.corpus,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

freq.body.tfidf = data.frame(sort(colSums(as.matrix(body.dtm)), decreasing=TRUE))

# Wordcloud based on tf-idf
wordcloud(rownames(freq.body.tfidf), freq.body.tfidf[,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2.5,.7))


# Begin Process for document clustering
# First we calculate distance between documents
tfidf.dtm.matrix = as.matrix(body.dtm.tfidf)

tfidf.distMatrix = dist(tfidf.dtm.matrix, method="euclidean")


# Cluster creation
tfidf.cluster= hclust(tfidf.distMatrix,method="ward.D")

plot(tfidf.cluster, cex=0.9, hang=-1)
rect.hclust(tfidf.cluster,k =7)
tfidf.output=data.frame(paste("document",names(body.corpus)),cutree(freq.cluster,k=7))
names(tfidf.output)[1]="document"
names(tfidf.output)[2]="cluster"
tfidf.output$content=e.corpus$content
table(tfidf.output$cluster)

## Create the clusters wordclouds
## Cluster 1
tfidf.cluster1 = subset(body.corpus, tfidf.output$cluster == 1)

# Document Term Matrix based on tf-idf
tfidf.cluster1.dtm = DocumentTermMatrix(tfidf.cluster1,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

tfidf.cluster1.dtm = data.frame(sort(colSums(as.matrix(tfidf.cluster1.dtm)), decreasing=TRUE))


# Wordcloud based on tf-idf
wordcloud(rownames(tfidf.cluster1.dtm ), tfidf.cluster1.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.7))

## Cluster 2
tfidf.cluster2 = subset(body.corpus, tfidf.output$cluster == 2)

# Document Term Matrix based on tf-idf
tfidf.cluster2.dtm = DocumentTermMatrix(tfidf.cluster2,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

tfidf.cluster2.dtm = data.frame(sort(colSums(as.matrix(tfidf.cluster2.dtm)), decreasing=TRUE))


# Wordcloud based on tf-idf
wordcloud(rownames(tfidf.cluster2.dtm ), tfidf.cluster2.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(3,.6))



## Cluster 3
tfidf.cluster3 = subset(body.corpus, tfidf.output$cluster == 3)

# Document Term Matrix based on tf-idf
tfidf.cluster3.dtm = DocumentTermMatrix(tfidf.cluster3,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

tfidf.cluster3.dtm = data.frame(sort(colSums(as.matrix(tfidf.cluster3.dtm)), decreasing=TRUE))


# Wordcloud based on tf-idf
wordcloud(rownames(tfidf.cluster3.dtm), tfidf.cluster3.dtm[,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.6))

## Cluster 4

tfidf.cluster4 = subset(body.corpus, tfidf.output$cluster == 4)

# Document Term Matrix based on tf-idf
tfidf.cluster4.dtm = DocumentTermMatrix(tfidf.cluster4,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

tfidf.cluster4.dtm = data.frame(sort(colSums(as.matrix(tfidf.cluster4.dtm)), decreasing=TRUE))


# Wordcloud based on tf-idf
wordcloud(rownames(tfidf.cluster4.dtm ), tfidf.cluster4.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.6))


## Cluster 5

tfidf.cluster5 = subset(body.corpus, tfidf.output$cluster == 5)

# Document Term Matrix based on tf-idf
tfidf.cluster5.dtm = DocumentTermMatrix(tfidf.cluster5,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

tfidf.cluster5.dtm = data.frame(sort(colSums(as.matrix(tfidf.cluster5.dtm)), decreasing=TRUE))


# Wordcloud based on tf-idf
wordcloud(rownames(tfidf.cluster5.dtm ), tfidf.cluster5.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(1.5,.6))

## Cluster 6

tfidf.cluster6 = subset(body.corpus, tfidf.output$cluster == 6)

# Document Term Matrix based on tf-idf
tfidf.cluster6.dtm = DocumentTermMatrix(tfidf.cluster6,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

tfidf.cluster6.dtm = data.frame(sort(colSums(as.matrix(tfidf.cluster6.dtm)), decreasing=TRUE))


# Wordcloud based on tf-idf
wordcloud(rownames(tfidf.cluster6.dtm ), tfidf.cluster6.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(1.5,.6))

## Cluster 7

tfidf.cluster7 = subset(body.corpus, tfidf.output$cluster == 7)

# Document Term Matrix based on tf-idf
tfidf.cluster7.dtm = DocumentTermMatrix(tfidf.cluster7,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))

tfidf.cluster7.dtm = data.frame(sort(colSums(as.matrix(tfidf.cluster7.dtm)), decreasing=TRUE))


# Wordcloud based on tf-idf
wordcloud(rownames(tfidf.cluster7.dtm ), tfidf.cluster7.dtm [,1], max.words=40, colors=brewer.pal(3, "Dark2"),scale = c(2,.6))


##########################################################################################################################

### Document Term Matrix based on tf-idf

body.dtm.tfidf = DocumentTermMatrix(body.corpus,control=list(weighting = weightTfIdf, bounds= list(global=c(minTermFreq))))
body.dtm.tfidf = as.matrix(body.dtm.tfidf)

row.names(body.dtm.tfidf) = 1:nrow(body.dtm.tfidf)

m = body.dtm.tfidf
library(proxy)
d <- stringdist(m, m, method="cosine")
hc <- hclust(d, method="average")

body.dtm.tfidf = removeSparseTerms(body.dtm.tfidf, 0.99)

freq = data.frame(sort(colSums(as.matrix(body.dtm.tfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(2, "Dark2"),scale = c(2,.5))




value_cluster=hclust(body.dtm.tfidf,method="ward.D") 

#remove quotations
body.corpus$value=gsub("\"", "",body.corpus$value)

#remove accents
body.corpus$value=iconv(body.corpus$value, from="UTF-8", to='ASCII//TRANSLIT')

body.corpus$value=gsub("dr.", "dr. ",body.corpus$value)

#remove gramatical errors
# pattern=c("\"|\\:\\s|\\s$")
# body.corpus$value=gsub(pattern, "",body.corpus$value)

rm(pattern)

#create frequency table
# df_table=as.data.frame(table(table))

#export
# write.csv(df_table,"C:/Users/SergioMC/Documents/Equilibrio/Research/preliminares.csv")

### CLUSTERING
# Get only unique selections
b.corpus=as.array(unique(body.corpus$value))

# Create a matrix of string distances
# value_matrix=stringdistmatrix(b.corpus,b.corpus)
value_matrix=stringdistmatrix(body.corpus,body.corpus)
names(e.corpus)


#results from the hierarchical clustering
value_cluster=hclust(as.dist(value_matrix),method="ward.D") 
plot(value_cluster, cex=0.9, hang=-1)
rect.hclust(value_cluster,k =7)
output=data.frame(cutree(value_cluster,k=7))
names(output)[1]="cluster"
output$names="1"
output$names=c(names(body.corpus))
