library(tm)
library(wordcloud)
library(topicmodels)
library(LDAvis)

datasetrackspace <- read.csv("C:/Users/owner/Documents/AWScloudtweets20150421.csv")
datasetrackspace$text <- as.factor(datasetrackspace$text)

# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors
doc.vec = VectorSource(datasetrackspace$text)
doc.corpus = Corpus(doc.vec)
#inspect(doc.corpus)

#Now we perform some transormations
doc.corpus = tm_map(doc.corpus, function(x) iconv(enc2utf8(x$content), sub = "byte"))
doc.corpus = tm_map(doc.corpus, PlainTextDocument)

#force to lowercase
doc.corpus = tm_map(doc.corpus, content_transformer(tolower))

#remove stopwords
doc.corpus = tm_map(doc.corpus, removeWords, stopwords('english'))
#doc.corpus = tm_map(doc.corpus, removeWords, 'cloudcomputing')

#remove punctuations
doc.corpus = tm_map(doc.corpus, removePunctuation)

#remove numbers
doc.corpus = tm_map(doc.corpus, removeNumbers)
doc.corpus = tm_map(doc.corpus, stripWhitespace)

dtm = DocumentTermMatrix(doc.corpus)
dtm
dtm = removeSparseTerms(dtm, 0.96)
dtm
x = as.matrix(dtm)
x = x[which(rowSums(x) > 0),]
rownames(x) = 1:nrow(x)

lda = LDA(x,3)

terms(lda,10)

topics(lda)


tabulate(topics(lda))

perplexity(lda)
