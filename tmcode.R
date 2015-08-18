#####text mining acsp book of abstracts
##h.estiri


###install needed packages
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")   
packagaes <- c("tm", "SnowballCC", "qdap", "RColorBrewer", "ggplot2", "scales", "wordcloud")   
install.packages(packages, dependencies=TRUE)


library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(qdap) # Quantitative discourse analysis of transcripts.
library(dplyr) # Data preparation and pipes %>%.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.



cname <- file.path("~", "Desktop/Cloud Drives/OneDrive/R/tm", "text")   
cname

##make corpus
doc <- Corpus(DirSource(cname))

####clean up the data
##remove punctuation
doc <- tm_map(doc, removePunctuation)   

#remove numbers 
doc <- tm_map(doc, removeNumbers) 

#convert to lowercase
doc <- tm_map(doc, tolower)  

#remove stopwords
doc <- tm_map(doc, removeWords, stopwords("english"))

#remove specific words and numbers
doc <- tm_map(doc, removeWords, c("abstract","paper", "university", "research", "new", "study", "will", "can","references", "author","also", "may", "one", "two", "journal","however","many"))

#remove numbers
doc <- tm_map(doc, removeNumbers)

##also combine words such as urban planning and land use to urban_planning and land_use and unified words like communities and community.

#Removing common word endings (e.g., “ing”, “es”, “s”)
library(SnowballC)   
doc <- tm_map(doc, stemDocument)

#Stripping unnecesary whitespace from your documents:
doc <- tm_map(doc, stripWhitespace)   

#treat your preprocessed documents as text documents.
doc <- tm_map(doc, PlainTextDocument)   


#create a document term matrix.
dtm <- DocumentTermMatrix(doc)   
inspect(dtm[, 1:20]) #inspecting the first 20 rows

#create a transpose of this matrix
tdm <- TermDocumentMatrix(doc)   

##explore data
freq <- colSums(as.matrix(dtm))   
length(freq)  

#order by freq
ord <- order(freq)

#save a csv just in case you need it!
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv") 

# make a matrix that is 10% empty space, maximum.   
dtms <- removeSparseTerms(dtm, 0.1) 


freq <- sort(colSums(as.matrix(dtms)), decreasing=TRUE)   
freq 

#create a data fram
df <- data.frame(word=names(freq), freq=freq)   
head(df,200)
sum(freq)
#add percentages to df
df$percentage <- (df$freq/292306)*100 #total number of frequencies is 292306


#plot df -- histogram
library(ggplot2)   
p <- ggplot(subset(df, freq>600), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 


#Plot word cloud from the 100 most frequently occurring words.
require(wordcloud)
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=200, scale=c(5, .1), rot.per=0.2)
#, colors=dark2)   