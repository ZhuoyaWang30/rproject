#Text Mining
#Step 1: Import data and build a corpus for text mining
# define vector of sentences ("docs")
text <- c("this is the first     sentence!!", 
          "this is a second Sentence :)", 
          "the third sentence, is here", 
          "forth of all 4 sentences")
install.packages("tm")
library(tm)
#convert documents into a corpus which is a specific data structure that "tm" package can work with.
corp <- Corpus(VectorSource(text))  
#Inspect the content of the corpus
inspect(corp)
#Step 2: Clean and pre-process the text data
# 1. Switch to lower case
corp <- tm_map(corp, tolower)
# inspect the corpus
inspect(corp)

# 2. Remove numbers 
corp <- tm_map(corp, removeNumbers)
# 3. Remove punctuation marks
corp <- tm_map(corp, removePunctuation) 
inspect(corp)

# 4. Remove stopwords
# These include words such as articles (a, an, the), conjunctions (and, or but etc.), common verbs (is), 
# qualifiers (yet, however etc). The tm package includes  a standard list of such stop words 
# examine the list of stopwords by typing in:
# stopwords("english")
corp <- tm_map(corp, removeWords, stopwords("english"))
# the third argument is the list of stop words
# 5. Remove extra whitespaces
corp <- tm_map(corp, stripWhitespace)
inspect(corp)



#In class practice
#Use the argument stringsAsFactors = F to avoid automatically treating the text column as a factor.
reviews <- read.csv("reviews.csv", stringsAsFactors = F)
View(reviews)
#Step 2: Clean and pre-process the text data
library(tm)
#we only use the review.text - single column which is a vector
corp_1 <- Corpus(VectorSource(reviews$Review.Text))

#3  1. Switch to lower case 2. Remove numbers 3. Remove punctuation marks 
#4. Remove stopwords 5. Remove extra whitespaces 6. Stemming
corp_1 <- tm_map(corp_1, tolower)
corp_1 <- tm_map(corp_1, removeNumbers)
corp_1 <- tm_map(corp_1, removePunctuation)
corp_1 <- tm_map(corp_1, removeWords, stopwords("english"))
corp_1 <- tm_map(corp_1, stripWhitespace)
install.packages("SnowballC")
library(SnowballC)
corp_1 <- tm_map(corp_1, stemDocument) 

inspect(corp_1)
