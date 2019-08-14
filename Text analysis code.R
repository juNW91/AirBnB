#text analysis and word mining code:
#packages required:

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

#load in library's:

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#read in csv file
text <- readLines(file.choose())
head( text )

#load docs in as corpus which is a collection of documents:
docs <- Corpus(VectorSource(text))
inspect(docs)

#tm_map used to change and replace special characters from the text:

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "!")
docs <- tm_map(docs, toSpace, "we’re")
docs <- tm_map(docs, toSpace, "We’re ")

#below functions cleans up the file:

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))

#removed repetative words that we though were not important to wordcloud:

docs <- tm_map(docs, removeWords, c("one", "denver", "Denver", "'s", "neighborhood",
"away", "within", "rino", "neighborhoods", "south", "field", "can", "denvers",
"many", "also", "highlands", "hill", "will", "north", "minute", "two", "colfax",
"five", "lodo", "find", "highright", "house", "well", "bar", "get", "zooget",
"just", "min", "colorado", "points", "homes", "colfax", "two", "around",
"min", "take", "high", "less", "points", "across", "around", "walk",
"blocks", "right", "like", "store", "mile", "street", "city", "distance",
"minutes", "located", "west"))

#creates a term document matrix of all the words that repeat
#and their frequency:

denTerms <- TermDocumentMatrix(docs)
m <- as.matrix(denTerms)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

#creates the wordcloud with the parameters set, wich are:
#minimum frequency is 200, and max number of words 100.

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 200,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#adds a title to the wordcloud:

mtext("Denver Air BnB Word Overview", side = 3, padj = -1)

#plots in a bar chart, the most frequent words used after we removed
#some in the docs <- tm_map(docs, removeWords code line:

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="blue", main ="Denver Air BnB Word Frequency",
        ylab = "Word Count")